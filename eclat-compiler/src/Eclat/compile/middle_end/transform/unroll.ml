open Ast

(*
let rec unroll env e =
  match e with
  | E_letIn(P_var f,(E_fix(g,(p,e1)) as v1),e2) ->
      let env' = (g,(v1,0))::env in
      E_letIn(P_var f,E_fix(g,(p,unroll env' e1)),unroll env e2)
  | E_app(E_const(Op(Runtime(Unroll n))), E_tuple[E_var f;e2]) ->
      (match List.assoc_opt f env with
      | None -> E_app(E_var f,e2)
      | Some (_,m) when m >= n -> Printf.printf "==> %d %d \n" m n ; E_app(E_var f,e2)
      | Some (E_fix(g,(p,e3)) as v,m) ->
          let env' = (f,(v,m+1))::env in
          E_letIn(p,e2,unroll env' e3)
      | _ -> assert false)
      (* substitution is needed (rather than a let-binding)
         since e2 could be a function (fun x -> e3)       (* no, first order now *) (* ah ? *) *)

  | e -> Ast_mapper.map (unroll env) e
*)

let unroll_pi pi = pi ;;
(*
  let main = unroll [] pi.main in
  { pi with main }
*)