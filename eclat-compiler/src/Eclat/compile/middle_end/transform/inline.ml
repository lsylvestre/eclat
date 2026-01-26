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

let fv_type_in ?(s=Types.Vs.empty) e =
  let open Types in
  let (++) m1 m2 =
    Vs.union m1 m2 in
  let r = ref s in
  let rec ss e =
    match e with
    | E_letIn(_, ty, e1, e2) ->
        r := !r ++ free_vars_of_type (Vs.empty,ty);
        ss e1;
        ss e2
    | E_fun(p, (ty,tyB), e1) ->
        r := !r ++ free_vars_of_type (Vs.empty,ty);
        r := !r ++ free_vars_of_type (Vs.empty,Ty_base tyB);
        ss e1
    | E_fix(f, (p, (ty,tyB), e1)) ->
        r := !r ++ free_vars_of_type (Vs.empty,ty) ;
        r := !r ++ free_vars_of_type (Vs.empty,Ty_base tyB);
        ss e1
    | E_reg((p, tyB, e1), e0, _) ->
        r := !r ++ free_vars_of_type (Vs.empty,Ty_base tyB);
        ss e1;
        ss e0
    | E_for(_,sz1,sz2,e3,_) ->
        r := !r ++ free_vars_of_type (Vs.empty,Ty_base(TyB_int sz1));
        r := !r ++ free_vars_of_type (Vs.empty,Ty_base(TyB_int sz2));
        ss e3
    | E_generate((p, (ty,tyB), e1), e2, sz3, sz4, _) ->
        r := !r ++ free_vars_of_type (Vs.empty,ty);
        r := !r ++ free_vars_of_type (Vs.empty,Ty_base tyB);
        ss e1;
        ss e2;
        r := !r ++ free_vars_of_type (Vs.empty,Ty_base(TyB_int sz3));
        r := !r ++ free_vars_of_type (Vs.empty,Ty_base(TyB_int sz4))
    | E_vector_mapi(is_par, (p, (tyB1,tyB2), e1), e2, sz) ->
        r := !r ++ free_vars_of_type (Vs.empty,Ty_base tyB1);
        r := !r ++ free_vars_of_type (Vs.empty,Ty_base tyB2);
        r := !r ++ free_vars_of_type (Vs.empty,Ty_base(TyB_int(sz)));
        ss e1;
        ss e2
    | E_const c ->
        let rec ss_const = function
        | Unit | Bool _ | String _ | V_loc _ | Inj _ -> ()
        | Int (_,sz) -> r := !r ++ free_vars_of_type (Vs.empty,Ty_size sz)
        | C_tuple cs | C_vector cs -> List.iter ss_const cs
        | C_appInj(x,c,tyB) -> 
            ss_const c;
            r := !r ++ free_vars_of_type (Vs.empty,Ty_base tyB)
        | C_size sz ->
            r := !r ++ free_vars_of_type (Vs.empty,Ty_size sz)
        | Op(op) -> (match op with
                     | TyConstr ty -> r := !r ++ free_vars_of_type (Vs.empty,ty)
                     | Runtime prim ->
                        (match prim with 
                         | External_fun (op,ty) -> r := !r ++ free_vars_of_type (Vs.empty,ty)
                         | _ -> ())
                      | op -> ())
        in ss_const c
    | e -> Ast_mapper.iter ss e
  in
  ss e;
  !r

(* instanciate type annotation in expression [e]
   while preserving sharing *)
let subst_ty e = (* todo: rename this function and remove the unused parameter *)
  let open Types in
  let vs = fv_type_in e in
  let unknowns = Hashtbl.create (Vs.cardinal vs) in
  (* Ast_pprint.pp_exp Format.std_formatter e; *)
   Vs.iter (fun n -> Hashtbl.add unknowns n.id (new_unknown_generic ())) vs;
  
  let rec ss e =
    let open Operators in
    match e with
    | E_letIn(p, ty, e1, e2) ->
        E_letIn(p, (* rename_ty unknowns ty *) Types.new_ty_unknown() 
                       (* fresh unknown because duplication breaks polymorphism.
                          todo: avoid this unknown (which can cause the lost of a type annotation) *)
                , ss e1, ss e2)
    | E_fun(p, (ty,tyB), e1) ->
       E_fun(p, (rename_ty unknowns ty,rename_tyB unknowns tyB), ss e1)
    | E_fix(f, (p, (ty, tyB), e1)) ->
        E_fix(f, (p, (rename_ty unknowns ty, rename_tyB unknowns tyB), ss e1))
    | E_reg((p,tyB, e1),e0,l) ->
        E_reg((p, rename_tyB unknowns tyB, ss e1),ss e0,l) 
    | E_array_create(sz,deco) -> E_array_create(rename_size unknowns sz,deco)
    | E_array_make(sz,c,deco) -> E_array_make(rename_size unknowns sz,c,deco)
    | E_const c ->
        let rec ss_const = function
        | Unit | Bool _ | String _ | V_loc _ | Inj _ as c -> c
        | Int (n,sz) -> Int (n,rename_size unknowns sz)
        | C_tuple cs -> C_tuple (List.map ss_const cs)
        | C_vector cs -> C_vector (List.map ss_const cs)
        | C_appInj(x,c,tyB) -> C_appInj(x,ss_const c,rename_tyB unknowns tyB)
        | C_size(sz) -> C_size (rename_size unknowns sz)
        | Op(op) -> Op(match op with
                       | TyConstr ty' -> TyConstr (rename_ty unknowns ty')
                       | Runtime prim ->
                          Runtime(match prim with 
                          | External_fun (op,ty) -> External_fun(op,(rename_ty unknowns ty))
                          | _ -> prim)
                        | op -> op)
        in E_const (ss_const c)
    | e -> Ast_mapper.map ss e
  in
  let e'= ss e in
  (* Ast_pprint.pp_exp Format.std_formatter e'; *)e'



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
            inline @@ subst_e ~when_var:subst_ty x e1 e2
        | _ -> E_letIn(p,ty,inline e1,inline e2))

    | E_app(E_fun(p,(ty,tyB),e1),e2) ->
        has_changed := true;
        (* substitution is needed (rather than a let-binding)
           since e2 could be a function (fun x -> e3)       (* no, first order now *) (* ah ? *) *)
        inline @@ subst_ty @@ E_letIn(p,ty,e2,e1)

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
