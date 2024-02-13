open Ast
open Ast_subst
open Ast_rename


let rec instantiate e =
  match e with
  | E_exec(e1,e2,_) ->
      E_exec(instantiate e1,instantiate e2,gensym ())
  | e -> Ast_mapper.map instantiate e

let instantiate_pi pi =
  {pi with main = instantiate pi.main }
