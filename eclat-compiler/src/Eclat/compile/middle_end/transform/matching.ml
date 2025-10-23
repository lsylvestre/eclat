open Ast
open Ast_subst

(** compile pair destructuring

   e.g. expression [fun z -> let (x,y) = z in x] becomes:
          [fun z ->
             let v = z in
             let x = fst v in
             let y = snd v in x] where [v] is a fresh name.
*)


let get_tuple pos e tyBs =
   let open Operators in
   E_app(E_const (Op (Runtime(Get_tuple {field=pos;tyBs}))),e)

(** [matching e] translate expression [e] in an expression where all
   let-bindings are of the form [let x = e1 in e2]. *)
let rec matching e =
  match e with
  | E_letIn(p,ty,e1,e2) ->
      (match p with
       | P_unit ->
          let z = gensym () in
          E_letIn(P_unit,ty,matching e1,matching e2)
       | P_var z ->
          (* == Var case == *)
          (* if to_propagate e1 then
          matching @@ Ast_subst.subst_e z (E_var y) e2
         else *) E_letIn(P_var z,ty,matching e1,matching e2)
       | P_tuple ps ->
          (* == Tuple case == *)
          (match e1 with
           | E_tuple(es) ->
              assert (List.compare_lengths ps es = 0); (* by static typing *)
              matching @@
              List.fold_right2 (fun p e acc -> E_letIn(p,Types.new_ty_unknown(),e,acc)) ps es e2
           | _ -> E_letIn(p,ty,matching e1,matching e2))) (*
               let tyb_list = match Types.canon_ty ty with
                              | Types.Ty_base (TyB_tuple tyBs) -> tyBs
                              | Ty_tuple ts -> List.map (function 
                                                         | Types.Ty_base tyB -> tyB 
                                                         | _ -> Types.new_tyB_unknown ()) ts 
                              | _ -> List.map (fun _ -> Types.new_tyB_unknown ()) ps in
               matching @@
                  let x = gensym () in
                  let var_x = E_var x in
                    E_letIn(P_var x,ty, e1,
                    let rec loop i = function
                    | [] -> e2
                    | p::ps' -> E_letIn(p,Types.new_ty_unknown(),
                                       get_tuple i var_x tyb_list, (loop (i+1) ps')) 
                  in loop 0 ps))) *)
  | E_fun(P_var x,tysig,e) ->
      E_fun(P_var x,tysig,matching e)
  | E_fun(p,(ty,tyB),e) ->
      let x = gensym () in
      matching @@ E_fun(P_var x,(ty,tyB),E_letIn(p,ty,E_var x,e))
  | E_fix(f,(P_var x,(ty,tyB),e)) ->
      E_fix(f,(P_var x,(ty,tyB),matching e))
  | E_fix(f,(p,(ty,tyB),e)) ->
      let x = gensym () in
      matching @@ E_fix(f,(P_var x,(ty,tyB),E_letIn(p,ty,E_var x,e)))
  | E_match(e1,hs,eo) ->
      E_match(matching e1,List.map (fun (x,(p,ei)) ->
        match p with
        | P_var _ -> x,(p,matching ei)
        | _ -> let y = gensym () in
               x,(P_var y,matching @@ E_letIn(p,Types.new_ty_unknown(),E_var y,ei))) hs,Option.map matching eo)
  | E_reg((P_var x,tyB,e1),e0,l) ->
     E_reg((P_var x,tyB,matching e1),matching e0,l)
  | E_reg((p,tyB,e1),e0,l) ->
     let x = gensym ~prefix:"temp" () in
     E_reg((P_var x,tyB,matching @@ E_letIn(p,Ty_base tyB,E_var x,e1)),matching e0,l)
  (*| E_equations(p,eqs) ->
      let x = gensym () in
let eqs' = List.map (fun (pz,e) ->
             match pz with 
             | P_var z -> pz, matching e
             | _ -> let z = gensym () in
                    P_var z,matching @@ E_letIn(p,Types.new_ty_unknown(),e,Pattern.pat2exp p)) eqs in
      E_equations(P_var x,eqs' @ [P_var x,Pattern.pat2exp p])
*)  | e -> Ast_mapper.map matching e


let matching_pi pi =
  Map_pi.map matching pi
