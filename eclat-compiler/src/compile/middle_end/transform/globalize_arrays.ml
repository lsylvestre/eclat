open Ast
open Ast_subst

(** [globalize e] globalizes all local *close* functions in expression [e] *)
let rec globalize_arrays_e (e:e) : ((x * e) list * e) =
  Ast_mapper.accum (fun glob e ->
    match e with
    | E_letIn(P_var x,(E_local_static_array _ as ex),e1) ->
        let x' = gensym ~prefix:x () in
        let ds1,e1' = glob e1 in
        Some ((x',ex)::ds1,subst_e x (E_var x') e1')
    (* | E_ref e1 -> 
        glob (let x = Ast.gensym () in 
              E_letIn(P_var x,e,E_var x))*)
    | _ -> None
  ) e

let globalize_arrays pi =
  let ds,e = globalize_arrays_e pi.main in
  let sts = List.map (function (x,E_local_static_array(c,n)) -> x,Static_array(c,n)
                       | _ -> assert false) ds in
  { pi with main=e ; statics=pi.statics@sts }