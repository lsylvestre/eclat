open Ast
open Ast_subst

let eval_static_exp_int ~loc e =
  let exception Cannot in 
  let rec eval e =
    match e with
    | E_const (Int(n,w)) -> (n,w)
    | E_app(E_const(Op(Runtime op)),e) ->
        let app_binop (f,e1,e2) = 
          let (v1,size) = eval e1 in
          let (v2,_) = eval e1 in
          (f v1 v2, size)
        in
        (match op,e with 
         | Add,E_tuple [e1;e2] -> app_binop((+),e1,e2)
         | Sub,E_tuple [e1;e2] -> app_binop((-),e1,e2)
         | Mult,E_tuple [e1;e2] -> app_binop(( * ),e1,e2)
         | Div,E_tuple [e1;e2] -> app_binop((/),e1,e2)
         | _ -> raise Cannot)
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
let rec inline e =
  match e with
  | E_letIn(p,e1,e2) ->
      (match p,e1 with
      | P_var x,E_fun _ -> inline @@ subst_e x e1 e2
      | _ -> E_letIn(p,inline e1,inline e2))

  | E_app(E_fun(p,e1),e2) ->
      (* substitution is needed (rather than a let-binding)
         since e2 could be a function (fun x -> e3)       (* no, first order now *) (* ah ? *) *)
      inline @@ E_letIn(p,e2,e1)
  | E_lastIn(x,e1,e2) ->
      let y = gensym () in
      E_lastIn(y,inline e1,subst_e x (E_var y) (inline e2))
 
  | E_generate((p,e1),init,e_st3,loc) ->
      let (n,w) = eval_static_exp_int ~loc e_st3 in
      inline @@
      let rec loop i =
        if i < n then
          E_letIn(p,E_tuple[E_const (Int(i,w)); loop(i+1)],e1)
        else init
       in loop 0

  | E_for(x,e_st1,e_st2,e3,loc) ->
      let (n,w) = eval_static_exp_int ~loc e_st1 in
      let (m,w') = eval_static_exp_int ~loc e_st2 in
      (* assert(w = w'); *)
      inline @@
      let ignore = gensym () in
      E_letIn(P_var ignore,
              (let es = List.init (m-n+1) (fun i -> 
                 E_letIn(P_var x, E_const (Int(n+i,w)),e3)) in
              match es with
              | [] -> E_const Unit
              | [e] -> e
              | e0::es ->
                  List.fold_left (fun acc ei -> E_par(acc,ei)) e0 es),
              E_const Unit)

  | E_appLabel(e1,l,lc) ->
      inline (app_labelC e1 l lc)
 
  | e -> Ast_mapper.map inline e


let inl_pi pi =
  let main = inline pi.main in
  { pi with main }
