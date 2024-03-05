open Ast
open Types

let eq_tys t1 t2 =
  (* Format.fprintf Format.std_formatter "----> %a ~? %a\n" Ast_pprint.pp_ty t1 Ast_pprint.pp_ty t2; *)
  let exception NotEqual in
  let rec compare_tyc tc1 tc2 =
    match tc1, tc2 with
    | TInt tz1, TInt tz2 -> 
        compare_ty (simplify_response_time tz1) (simplify_response_time tz2)
    | TBool,TBool -> ()
    | TUnit,TUnit -> ()
    | _ -> raise NotEqual
  and compare_ty t1 t2 =
    match canon t1,canon t2 with
    | T_var {contents=(Ty t1')},t2' -> compare_ty t1' t2'
    | t1',T_var {contents=(Ty t2')} -> compare_ty t1' t2'
    | T_var ({contents=Unknown _}),_ |
      _,T_var ({contents=Unknown _}) -> () (* will be unified in the generated code *)
    | T_const tc1, T_const tc2 ->
        compare_tyc tc1 tc2
    | T_tuple ts1, T_tuple ts2 ->
       if List.compare_lengths ts1 ts2 <> 0 then raise NotEqual;
        List.iter2 compare_ty ts1 ts2
    | T_fun{arg;dur;ret},T_fun{arg=a;dur=d;ret=r} ->
       compare_ty arg a; compare_ty ret r
    | T_size n, T_size m -> if n <> m then raise NotEqual
    | T_add _,T_add _ ->
        ()
    | T_max _,T_max _ ->
        ()
    | T_le _,T_le _ ->
        ()
    | T_response_time _, T_response_time _ -> ()
    | T_infinity,T_infinity -> ()
    | T_ref t1', T_ref t2' -> compare_ty t1' t2'
    | T_array{elem=t1';size=sz1}, 
      T_array{elem=t2';size=sz2} -> 
          compare_ty t1' t2'; compare_ty sz1 sz2
    | T_string sz1, T_string sz2 -> compare_ty sz1 sz2
    | _ -> raise NotEqual
  in
  try compare_ty t1 t2; true with NotEqual -> false

let criterion f = 
  let lty = Hashtbl.find_all Typing.signatures f in  
  (* List.iter 
    (fun t -> Format.fprintf Format.std_formatter "---=> %a\n"
       Ast_pprint.pp_ty t) lty;*)
  let ty,lty' = 
    match lty with
    | [] -> assert false
    | ty::lty' -> ty,lty' in
  List.for_all (eq_tys ty) lty'

let monomorphize_exp e =
  let rec aux ds e =
    match e with
    | E_letIn(P_var f,E_fix(_,(p,e1)),e2) -> 
        let v = E_fix(f,(p,aux ds e1)) in
        (* Printf.printf "---> %s %b\n " f (criterion f);*)
        if criterion f then E_letIn(P_var f,v,aux ds e2)
        else aux ((f,v)::ds) e2
    | E_app(E_var f,arg) ->
       if criterion f then e else
       (match List.assoc_opt f ds with
       | Some v ->
           let g = Ast.gensym ~prefix:f () in
           E_letIn(P_var g,v,E_app(E_var g,arg))
       | None -> (* tail call *) e)
    | e -> Ast_mapper.map (aux ds) e
  in 
  aux [] e


let monomorphize pi =
  if !Typing.monomorphic then pi else 
  let _ = Typing.typing_with_argument ~collect_sig:true pi [] in
  let e = monomorphize_exp pi.main in
  { pi with main = e }


