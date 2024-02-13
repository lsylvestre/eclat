open Ast
open Ast_subst

(** compile pair destructuring

   e.g. expression [fun z -> let (x,y) = z in x] becomes:
          [fun z ->
             let v = z in
             let x = fst v in
             let y = snd v in x] where [v] is a fresh name.
*)

let get_tuple pos arity e =
   E_app(E_const (Op (GetTuple {pos;arity})),e)

(** [matching e] translate expression [e] in an expression where all
   let-bindings are of the form [let x = e1 in e2]. *)
let rec matching e =
  match e with
  | E_letIn(p,e1,e2) ->
      (match p with
       | P_unit ->
          let z = gensym () in
          matching @@ E_letIn(P_var z,e1,e2)
       | P_var z ->
          (* == Var case == *)
          E_letIn(P_var z,matching e1,matching e2)
       | P_tuple ps ->
          (* == Tuple case == *)
          (match e1 with
           | E_tuple(es) ->
              assert (List.compare_lengths ps es = 0); (* by static typing *)
              matching @@
              List.fold_right2 (fun p e acc -> E_letIn(p,e,acc)) ps es e2
           | _ ->
               matching @@
                  let arity = List.length ps in
                  let x = gensym () in
                  let var_x = E_var x in
                    E_letIn(P_var x, e1,
                    let rec loop i = function
                    | [] -> e2
                    | p::ps' -> E_letIn(p,get_tuple i arity var_x, (loop (i+1) ps')) in loop 0 ps)))
  | E_fun(P_var x,e) ->
      E_fun(P_var x,matching e)
  | E_fun(p,e) ->
      let x = gensym () in
      matching @@ E_fun(P_var x,E_letIn(p,E_var x,e))
  | E_fix(f,(P_var x,e)) ->
      E_fix(f,(P_var x,matching e))
  | E_fix(f,(p,e)) ->
      let x = gensym () in
      matching @@ E_fix(f,(P_var x,E_letIn(p,E_var x,e)))
  | E_match(e1,hs,eo) ->
      E_match(matching e1,List.map (fun (x,(p,ei)) ->
        match p with
        | P_var _ -> x,(p,matching ei)
        | _ -> let y = gensym () in
               x,(P_var y,matching @@ E_letIn(p,E_var y,ei))) hs,Option.map matching eo)
  | E_reg((p,e1),e0,l) ->
     let x = gensym () in
     E_reg((P_var x,matching @@ E_letIn(p,E_var x,e1)),matching e0,l)
  | e -> Ast_mapper.map matching e


let matching_pi pi =
  Map_pi.map matching pi
