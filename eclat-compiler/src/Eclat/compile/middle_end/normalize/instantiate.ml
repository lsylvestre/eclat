open Ast
open Ast_subst
open Ast_rename

let gen_label () = gensym ~prefix:"label" ()

let rec instantiate e =
  match e with
  | E_run(f,e1,_) -> E_run(f,instantiate e1,gen_label ())
  | E_exec(e1,e2,eo,_) ->
      E_exec(instantiate e1,instantiate e2,Option.map instantiate eo,gen_label ())
  | E_reg((p,tyB,e1),e0,_) ->
      E_reg((p,tyB,instantiate e1),instantiate e0,gen_label ())
  | e -> Ast_mapper.map instantiate e

let instantiate_pi pi =
  {pi with main = instantiate pi.main }
