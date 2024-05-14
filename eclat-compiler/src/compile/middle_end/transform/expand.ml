open Ast
open Ast_subst

let has_changed = ref false

let eval_static_exp_int ~loc ~statics e =
  let exception Cannot in 
  let rec eval e =
    match e with
    | E_const (Int(n,w)) -> (n,w)
    | E_app(E_const(Op(Runtime op)),e) ->
        let app_binop (f,e1,e2) = 
          let (v1,size) = eval e1 in
          let (v2,_) = eval e2 in
          (f v1 v2, size)
        in
        (match op,e with 
         | Add,E_tuple [e1;e2] -> app_binop((+),e1,e2)
         | Sub,E_tuple [e1;e2] -> app_binop((-),e1,e2)
         | Mult,E_tuple [e1;e2] -> app_binop(( * ),e1,e2)
         | Div,E_tuple [e1;e2] -> app_binop((/),e1,e2)
         | _ -> raise Cannot)
    | E_array_length(x) ->
       (match List.assoc_opt x statics with
       | Some (Static_array(_,n)) -> (n,Types.unknown())
       | _ -> Ast_pprint.pp_exp Format.std_formatter e; assert false (* ill-typed *) ) 
    | E_matrix_size(x,n) ->
       (match List.assoc_opt x statics with
        | Some (Static_matrix(_,n_list)) -> 
          let num = try List.nth n_list n
                    with _ -> assert false (* ill-typed *)
          in
          (num,Types.unknown())
       | _ -> Printf.printf "---> %s\n" x; assert false (* ill-typed *) ) 
    | _ -> raise Cannot
  in 
  try eval e with
  | Cannot -> 
      let open Prelude.Errors in
      error ~loc (fun fmt ->
         Format.fprintf fmt
           "@[<v>Cannot statically evaluate expression %a@]" 
               Ast_pprint.pp_exp e)


(* inline a program given in ANF, lambda-lifted form. The resulting program is
   in lambda-lifted-form but not necessarily in ANF-form. *)

(** [inline e] returns a non-ANF expression in which non-recursive functions
    are inlined *)
let rec expand ~statics e =
  match e with
  (*
  | E_generate((p,e1),init,e_st3,loc) ->
      has_changed := true;
      let (n,w) = eval_static_exp_int ~loc ~statics e_st3 in
      expand @@
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
      expand @@
      let ignore = gensym () in
      E_letIn(P_var ignore,
              (let es = List.init (m-n+1) (fun i -> 
                 E_letIn(P_var x, E_const (Int(n+i,w)),e3)) in
              E_par(es)),
              E_const Unit)
*)

  | E_vector_mapi(_is_par,(p,e1),e2,ty) ->
      has_changed := true;
      let e1' = expand ~statics @@ e1 in
      (match Types.canon ty with
      | T_size n ->
          Matching.matching @@
          let y = gensym () in
          E_letIn(P_var y,e2,
            let rec loop xs i =
            if i >= n then E_vector(List.rev_map (fun x -> E_var x) xs) else
            let z = gensym () in
            let x = gensym () in
            let ii = E_const(Int(i,Types.unknown())) in
            E_letIn(P_var x, E_letIn(P_var z,
                                  E_app(E_const(Op(Runtime (Vector_get (Types.unknown())))),
                                  E_tuple[E_var y;ii]),
                              Ast_subst.subst_p_e p (E_tuple[ii;E_var z]) (Ast_rename.rename_e ~statics e1')),
            loop (x::xs) (i+1)) in loop [] 0)
            (* Ast_pprint.pp_exp Format.std_formatter e0; *)
      | _ -> assert false (* todo error *)
      )
  | E_int_mapi(_is_par,(p,e1),e2,ty) ->
      has_changed := true;
      let e1' = expand ~statics @@ e1 in
      let t = match e2 with E_const(Int(_,size)) -> size | _ -> ty in
      (match Types.canon t with
      | T_size n ->
          Matching.matching @@
          let y = gensym () in
          E_letIn(P_var y,e2,
            let rec loop xs i =
            if i < 0 then E_app(E_const(Op(Runtime(Int_of_tuple n))),E_tuple(List.rev_map (fun x -> E_var x) xs)) else
            let z = gensym () in
            let x = gensym () in
            let ii = E_const(Int(i,Types.unknown())) in
            E_letIn(P_var x, E_letIn(P_var z,
                                  E_app(E_const(Op(Runtime (GetBit))),
                                  E_tuple[E_var y;ii]),
                              Ast_subst.subst_p_e p (E_tuple[ii;E_var z]) (Ast_rename.rename_e ~statics e1')),
            loop (x::xs) (i-1)) in loop [] (n-1))
      (* Ast_pprint.pp_exp Format.std_formatter e0; *)
      | _ -> Ast_pprint.pp_exp Format.std_formatter e; assert false (* todo error *)
      )

  | e -> Ast_mapper.map (expand ~statics) e


let expand_pi pi =
  let main = expand ~statics:(List.map fst pi.statics) pi.main in
  { pi with main }
