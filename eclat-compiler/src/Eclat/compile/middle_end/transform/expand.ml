open Ast
open Ast_subst

let has_changed = ref false

let eval_size ~loc sz =
  let rec eval sz =
    match Types.canon_size sz with
    | Types.Sz_lit n -> n
    | Types.Sz_add(sz',n) -> Size_limits.Size_op.add (eval sz') n
    | Types.Sz_twice(sz') -> Size_limits.Size_op.mul 2 (eval sz')
    | _ -> let open Prelude.Errors in
           error ~loc (fun fmt ->
           Format.fprintf fmt
             "@[<v>Cannot statically determine size %a@]" 
                 Types.pp_size sz)
  in
  let n = eval sz in
      (n,Types.new_size_unknown())


let eval_static_exp_int ~loc ~statics e =
  let exception Cannot in 
  let rec eval e =
    match e with
    | E_const (C_size(sz)) ->
        (match Types.canon_size sz with
         | Sz_lit n -> (n,sz)
         | _ -> raise Cannot)
    | E_const (Int(n,w)) -> (n,w)
    | E_app(E_const(Op(Runtime (External_fun (op,_)))),e) ->
        let app_binop (f,e1,e2) = 
          let (v1,size) = eval e1 in
          let (v2,_) = eval e2 in
          (f v1 v2, size)
        in
        (match op,e with 
         | "Int.add",E_tuple [e1;e2] -> app_binop(Size_limits.Size_op.add,e1,e2)
         | "Int.sub",E_tuple [e1;e2] -> app_binop(Size_limits.Size_op.sub,e1,e2)
         | "Int.mul",E_tuple [e1;e2] -> app_binop(Size_limits.Size_op.mul,e1,e2)
         | "Int.div",E_tuple [e1;e2] -> app_binop(Size_limits.Size_op.div,e1,e2)
         | _ -> raise Cannot)
    | E_array_length(x,_) ->
       (match List.assoc_opt x statics with
       | Some (Static_array(_,n)) -> (n,Types.new_size_unknown())
       | _ -> Ast_pprint.pp_exp Format.std_formatter e; assert false (* ill-typed *) ) 
    | _ -> raise Cannot
  in 
  try eval e with
  | Cannot -> 
      let open Prelude.Errors in
      error ~loc (fun fmt ->
         Format.fprintf fmt
           "@[<v>Cannot statically evaluate expression %a@]" 
               Ast_pprint.pp_exp e)

let rec expand ~statics e =
  match e with
  | E_app(E_const(Op(Int_of_size loc)),E_const(C_size sz)) ->
       E_const (Int(eval_size ~loc sz))
  | E_generate((p,(ty,tyB),e1),init,sz3,sz4,loc) ->
      expand ~statics @@ (
        Matching.matching @@ Anf.anf (
        has_changed := true;
        let (n0,w) = eval_size ~loc sz3 in
        let (n,w) = eval_size ~loc sz4 in
        let rec loop i =
          if i >= n0 then
            E_letIn(p,Types.new_ty_unknown(),E_tuple[E_const (Int(i,w)); loop(i-1)],e1)
          else init
         in loop n))
  | E_for(x,e1,e2,e3,sz,loc) ->    
        let (c,w) = eval_size ~loc sz in
        expand ~statics @@
        let e2' = match e2 with 
                   | E_const(C_size sz) ->
                       let (n,sz') = eval_size ~loc sz in
                       E_const (Int(n,sz'))
                   | _ -> e2 in
        let loop = gensym ~prefix:"loop" () in
        let i = x in
        let n0 = gensym ~prefix:"n0" () in
        let n = gensym ~prefix:"n" () in
        let ii = gensym ~prefix:"ii" () in
        let j = gensym ~prefix:"j" () in
        let open Types in
        E_letIn(P_var n0,Types.new_ty_unknown(), e1,
        E_letIn(P_var n, Types.new_ty_unknown(), e2',
        E_letIn(P_var loop,Types.new_ty_unknown(),
                          E_fix(loop,(P_var i,(Types.new_ty_unknown(),Types.new_tyB_unknown()),
                              E_if(E_app(E_const(Op(Runtime(External_fun("Int.gt",new_ty_unknown ())))),E_tuple[E_var i;E_var n]),
                                            E_const(Unit),
                              let e4 = E_letIn(P_var j,Types.new_ty_unknown(), 
                                               E_app(E_const(Op(Runtime(External_fun("Int.add",new_ty_unknown ())))),E_tuple[E_var i;E_var ii]),
                                          E_if(E_app(E_const(Op(Runtime(External_fun("Int.gt",new_ty_unknown ())))),E_tuple[E_var j;E_var n]),
                                            E_const(Unit),Ast_subst.subst_e x (E_var j) e3)) in
                              E_letIn(P_unit,Types.new_ty_unknown(), 
                                  E_parfor(ii,Sz_lit 0,Sz_lit(c-1),e4,loc),
                                  E_letIn(P_var i,new_ty_unknown(),E_app(E_const(Op(Runtime(External_fun("Int.add",new_ty_unknown ())))),
                                        E_tuple[E_var i;E_const (Int (c,w))]),
                                        E_app(E_var loop,E_var i)))))),
              E_app(E_var loop,E_var n0))))
    | E_parfor(x,sz1,sz2,e3,loc) -> 
      expand ~statics @@ (
        Matching.matching @@ Anf.anf  (
        has_changed := true;
        let (n,w) = eval_size ~loc sz1 in
        let (m,w') = eval_size ~loc sz2 in
        (* assert(w = w'); *)
        let ignore = gensym () in
        E_letIn(P_var ignore,Types.new_ty_unknown(),
                (let es = List.init (m-n+1) (fun i -> 
                   E_letIn(P_var x,Types.new_ty_unknown(),E_const (Int(n+i,w)),e3)) in
                E_par(es)),
                E_const Unit)))

  | E_vector_mapi(_is_par,(p,tyB,e1),e2,sz) ->
      expand ~statics @@ (
      has_changed := true;
      let e1' = expand ~statics @@ e1 in
      (match Types.canon_size sz with
      | Sz_lit n ->
          Matching.matching @@
          let y = gensym () in
          E_letIn(P_var y,Types.new_ty_unknown(),e2,
            let rec loop xs i =
            if i >= n then E_vector(List.rev_map (fun x -> E_var x) xs) else
            let z = gensym () in
            let x = gensym () in
            let ii = E_const(Int(i,Types.new_size_unknown())) in
            E_letIn(P_var x, Types.new_ty_unknown(),E_letIn(P_var z,Types.new_ty_unknown(),
                                  E_app(E_const(Op(Runtime (External_fun("Vect.nth",Types.new_ty_unknown())))),
                                  E_tuple[E_var y;ii]),
                              Ast_subst.subst_p_e p (E_tuple[ii;E_var z]) (Ast_rename.rename_e ~statics e1')),
            loop (x::xs) (i+1)) in loop [] 0)
      | _ -> assert false (* todo error *)
      ))

  | e -> Ast_mapper.map (expand ~statics) e

(*
let rec expand_e ~statics e =
  has_changed := false;
  let e' = expand_e ~statics e in
  if !has_changed then expand_e ~statics e' else e' ;;
*)
let expand_pi pi =
  let main = expand ~statics:pi.genv.statics pi.main in
  { pi with main }
