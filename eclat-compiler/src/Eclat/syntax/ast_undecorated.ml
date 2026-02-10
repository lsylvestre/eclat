open Ast

let keep_ty_flag = ref false ;;

(** [remove_deco e] removes both locations and constructs [(e : t)].
   In other word, [(e : t)] is only taking into account during typing,
   but no during code generation ! Solution : other constructs keep trace
   of such type constraints (e.g. litteral constants for integer, which include their size type)
 *)

exception Still_decorated of e

let still_decorated e =
  raise (Still_decorated e)

let rec remove_deco_pat = function
| P_unit | P_var _ as p -> p
| P_tuple ps -> P_tuple (List.map remove_deco_pat ps)
| P_tyConstr(p,ty) ->
    if !keep_ty_flag then P_tyConstr(remove_deco_pat p,ty) 
    else remove_deco_pat p

let rec remove_deco e =
  match e with
  | E_deco(e,_) ->
      remove_deco e
  | E_app(E_const(Op(TyConstr ty)),e) -> (* remove also type annotations [(e : t)] *)
      if !keep_ty_flag then E_app(E_const(Op(TyConstr ty)),remove_deco e)
      else remove_deco e  
  (* ************************************* *)
  | E_letIn(p,ty,e1,e2) ->
      E_letIn(remove_deco_pat p, Types.remove_alias_ty ty, remove_deco e1, remove_deco e2)
  | E_fun(p,(ty,tyB),e1) ->
      E_fun(remove_deco_pat p, (Types.remove_alias_ty ty,Types.remove_alias_tyB tyB), remove_deco e1)
  | E_fix(f,(p,(ty,tyB),e1)) ->
      E_fix(f,(remove_deco_pat p, (Types.remove_alias_ty ty,Types.remove_alias_tyB tyB), remove_deco e1))
  | E_reg((p,tyB,e1),e0,l) ->
      E_reg((remove_deco_pat p, Types.remove_alias_tyB tyB, remove_deco e1),remove_deco e0,l)
  | E_match(e1,hs,e2_opt) ->
      E_match(remove_deco e1, 
              List.map (fun (x,(p,ei)) -> (x,(remove_deco_pat p, remove_deco ei))) hs,
              Option.map remove_deco e2_opt)
  | E_generate((p,(ty,tyB),e1),e2,sz1,sz2,loc) ->
      E_generate((remove_deco_pat p,(Types.remove_alias_ty ty,Types.remove_alias_tyB tyB),remove_deco e1),remove_deco e2,sz1,sz2,loc)
  (* ************************************* *)
  | e -> Ast_mapper.map remove_deco e


let rec remove_all e =
  remove_deco e


let rec remove_tyconstr e =
  match e with
  | E_app(E_const(Op(TyConstr ty)),e) -> Some ty, e
  | e -> None,e


let remove_deco_pi pi =
  Map_pi.map remove_deco pi

