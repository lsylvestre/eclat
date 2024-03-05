open Ast
open Ast_subst

(** [globalize e] globalizes all local *close* functions in expression [e] *)
let rec globalize_ref_e (e:e) : ((x * e) list * e) =
  Ast_mapper.accum (fun glob e ->
    match e with
    | E_letIn(P_var x,E_ref(c),e1) ->
        let ds1,e1' = glob e1 in
        Some ((x,E_ref(c))::ds1,e1')
    (* | E_ref e1 -> 
        glob (let x = Ast.gensym () in 
              E_letIn(P_var x,e,E_var x))*)
    | _ -> None
  ) e

let globalize_ref pi =
  let ds,e = globalize_ref_e pi.main in
  { pi with main=Ast_mapper.declare ds e }