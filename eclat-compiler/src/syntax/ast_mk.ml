open Ast
open Types

let main_symbol = ref "main" (* default entry point (a function name) *)

(* [mk_fun p e] constructs expression (fun p -> e) *)
let mk_fun ~ty ~tyB p e =
  let ty1 = match ty with 
            | None -> new_ty_unknown()
            | Some t -> t in
  let tyB2 = match tyB with
             | None -> new_tyB_unknown() 
             | Some t -> t in
  E_fun(p, (ty1,tyB2), e)


(** [mk_fun p e] constructs expression (fun p -> e) *)
let mk_fun p e =
  E_fun(p,(new_ty_unknown(),new_tyB_unknown()), e)


(** constructs expression (fun p -> e) *)
let mk_fun_ty_annot p ty_f_opt e =
  match ty_f_opt with
  | None -> mk_fun p e
  | Some ty -> (ty_annot ~ty (mk_fun p e))

let mk_fun_ty_annot_p p ty_p_opt e =
  match ty_p_opt with
  | None -> mk_fun p e
  | Some ty -> (ty_annot ~ty:(Ty_fun(ty,new_dur_unknown(),new_tyB_unknown())) (mk_fun p e))

(* to buid expression [fun (p : t) -> (e : t')] from
   a declaration [let f (p : t) : t' =  e] *)
let mk_let_fun ~loc ~p_ty_opt ~ty_opt_ret e =
  let p,ty_f_opt =
    match p_ty_opt with
    | p,None -> p,None
    | p,Some t -> p,Some (Ty_fun(t,new_dur_unknown(),new_tyB_unknown()))
  in
  mk_fun_ty_annot p ty_f_opt (ty_annot_opt ~ty:ty_opt_ret e)
  |> mk_loc loc
(*

(*  [enforce_node p (fun x -> e)] adds a type contraint
     to the binding {p |-> (fun x -> e)} enforcing (fun x -> e)
     to be instantaneous. *)
let enforce_node (p,e) =
  p, ty_annot ~ty:(Ty_fun(new_ty_unknown(),Dur_zero,new_tyB_unknown())) e

*)
let rec mk_fix (f:x) (e:e) loc : e =
  match e with
    | E_fun(p,sigty,e1) ->
        E_fix(f,(p,sigty,e1))
    | E_deco(e1,loc) ->
        E_deco(mk_fix f e1 loc, loc)
    | E_app(E_const(Op(TyConstr t)),e1) ->
        E_app(E_const(Op(TyConstr t)),mk_fix f e1 loc)
    | _ -> Prelude.Errors.syntax_error ~msg:"recursive expression should be a function" loc

(*
let mk_letrec (f:x) (e1:e) (e2:e) loc_fun : e =
  let e1' = mk_fix f e1 loc_fun in
  E_letIn(P_var f,e1',e2)
*)