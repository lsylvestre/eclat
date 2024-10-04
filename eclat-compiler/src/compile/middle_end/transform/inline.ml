open Ast
open Ast_subst

let has_changed = ref false

let eval_static_exp_int ~loc ~statics e =
  let exception Cannot in 
  let rec eval e =
    match e with
    | E_const (Int(n,w)) -> (n,w)
    | E_app(E_const(Op(Runtime (External_fun (op,_)))),e) ->
        let app_binop (f,e1,e2) = 
          let (v1,size) = eval e1 in
          let (v2,_) = eval e2 in
          (f v1 v2, size)
        in
        (match op,e with 
         | "Int.add",E_tuple [e1;e2] -> app_binop((+),e1,e2)
         | "Int.sub",E_tuple [e1;e2] -> app_binop((-),e1,e2)
         | "Int.mul",E_tuple [e1;e2] -> app_binop(( * ),e1,e2)
         | "Int.div",E_tuple [e1;e2] -> app_binop((/),e1,e2)
         | _ -> raise Cannot)
    | E_array_length(x) ->
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



let subst_ty ty e =
  let open Types in
  let vs = free_vars_of_type (Vs.empty,ty) in
  let unknowns = Hashtbl.create (Vs.cardinal vs) in
  Vs.iter (fun n -> Hashtbl.add unknowns n (new_unknown_generic())) vs;
  let rec ss e =
    let open Operators in
    match e with
    | E_letIn(p, ty, e1, e2) ->
        E_letIn(p, rename_ty unknowns ty, ss e1, ss e2)
    | E_fun(p, (ty,tyB), e1) ->
       E_fun(p, (rename_ty unknowns ty,rename_tyB unknowns tyB), ss e1)
    | E_fix(f, (p, (ty, tyB), e1)) ->
        E_fix(f, (p, (rename_ty unknowns ty, rename_tyB unknowns tyB), ss e1))
    | E_app(E_const(Op(TyConstr ty)),e) ->
        E_app(E_const(Op(TyConstr (rename_ty unknowns ty))),ss e)
    | E_app(E_const(Op(Runtime (op))),e) ->
        let op' = (match op with 
                  | Resize_int sz -> Resize_int (rename_size unknowns sz)
                  | Vector_create sz -> Vector_create (rename_size unknowns sz)
                  | External_fun (op,ty) -> External_fun(op,(rename_ty unknowns ty))
                  | op -> op) in
        E_app(E_const(Op(Runtime (op'))),ss e)
    | E_array_create(sz,deco) -> E_array_create(rename_size unknowns sz,deco)
    | E_array_make(sz,c,deco) -> E_array_make(rename_size unknowns sz,c,deco)
    | e -> Ast_mapper.map ss e
  in
  ss e



(* inline a program given in ANF, lambda-lifted form. The resulting program is
   in lambda-lifted-form but not necessarily in ANF-form. *)

(** [inline e] returns a non-ANF expression in which non-recursive functions
    are inlined *)
let inline_with_statics ~statics e =
  let rec inline e =
    match e with
    | E_letIn(p,ty,e1,e2) ->
        (match p,e1 with
        | P_var x,E_fun _ -> 
            has_changed := true;
            inline @@ subst_e x e1 e2
        | _ -> E_letIn(p,ty,inline e1,inline e2))

    | E_app(E_fun(p,(ty,tyB),e1),e2) ->
        has_changed := true;
        (* substitution is needed (rather than a let-binding)
           since e2 could be a function (fun x -> e3)       (* no, first order now *) (* ah ? *) *)
        inline @@ subst_ty ty @@ E_letIn(p,ty,e2,e1)

(*     | E_generate((p,e1),init,e_st3,loc) ->
        has_changed := true;
        let (n,w) = eval_static_exp_int ~loc ~statics e_st3 in
        inline @@
        let rec loop i =
          if i < n then
            E_letIn(p,E_tuple[E_const (Int(i,w)); loop(i+1)],e1)
          else init
         in loop 0

    | E_for(x,e_st1,e_st2,e3,loc) ->
        has_changed := true;
        let (n,w) = eval_static_exp_int ~loc ~statics e_st1 in
        let (m,w') = eval_static_exp_int ~loc ~statics e_st2 in
        (* assert(w = w'); *)
        inline @@
        let ignore = gensym () in
        E_letIn(P_var ignore,
                (let es = List.init (m-n+1) (fun i -> 
                   E_letIn(P_var x, E_const (Int(n+i,w)),e3)) in
                E_par(es)),
                E_const Unit)
*)
    | e -> Ast_mapper.map inline e
  in 
  inline e

let inl_pi pi =
  has_changed := false;
  let main = inline_with_statics ~statics:pi.statics pi.main in
  { pi with main }
