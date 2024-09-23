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

let rec remove_deco e =
  match e with
  | E_deco(e,_) ->
      remove_deco e
  | E_app(E_const(Op(TyConstr ty)),e) -> (* remove also type annotations [(e : t)] *)
      if !keep_ty_flag then E_app(E_const(Op(TyConstr ty)),remove_deco e)
      else remove_deco e  
  | e -> Ast_mapper.map remove_deco e


let rec remove_all e =
  remove_deco e


let rec remove_tyconstr e =
  match e with
  | E_app(E_const(Op(TyConstr ty)),e) -> Some ty, e
  | e -> None,e


let remove_deco_pi pi =
  Map_pi.map remove_deco pi

