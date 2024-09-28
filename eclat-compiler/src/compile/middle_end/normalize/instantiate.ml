open Ast
open Ast_subst
open Ast_rename


let rec instantiate e =
  match e with
  | E_exec(e1,e2,eo,_) ->
      E_exec(instantiate e1,instantiate e2,Option.map instantiate eo,gensym ())
  | E_reg((p,tyB,e1),e0,_) ->
      E_reg((p,tyB,instantiate e1),instantiate e0,gensym ())
  | e -> Ast_mapper.map instantiate e

let instantiate_pi pi =
  {pi with main = instantiate pi.main }
