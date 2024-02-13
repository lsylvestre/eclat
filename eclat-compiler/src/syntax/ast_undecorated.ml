open Ast

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
  | E_app(E_const(Op(TyConstr _)),e) -> (* remove also type annotations [(e : t)] *)
      remove_deco e
  | e -> Ast_mapper.map remove_deco e

let remove_deco_pi pi =
  Map_pi.map remove_deco pi

