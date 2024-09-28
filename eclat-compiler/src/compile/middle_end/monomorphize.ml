open Ast
open Types

let eq_tys t1 t2 =
  (* Format.fprintf Format.std_formatter "----> %a ~? %a\n" Ast_pprint.pp_ty t1 Ast_pprint.pp_ty t2; *)
  let exception NotEqual in
  let rec compare_tyB tyB1 tyB2 =
    match canon_tyB tyB1, canon_tyB tyB2 with
    | TyB_int tz1, TyB_int tz2 -> 
        compare_size tz1 tz2
    | TyB_bool,TyB_bool -> ()
    | TyB_unit,TyB_unit -> ()
    | TyB_tuple ts1, TyB_tuple ts2 ->
       if List.compare_lengths ts1 ts2 <> 0 then raise NotEqual;
        List.iter2 compare_tyB ts1 ts2
    | TyB_var{contents=Is tyB1'}, tyB2'
    | tyB1',TyB_var{contents=Is tyB2'} ->
        compare_tyB tyB1 tyB2
    | TyB_var{contents=Unknown _}, _
    | _,TyB_var{contents=Unknown _} -> raise NotEqual
    (*| TyB_vector(sz1,tyB1),TyB_vector(sz2,tyB2) ->
        compare_size sz1 sz2;
        compare_tyB tyB1 tyB2
    | TyB_abstract(x1,sz1,tyB_list1),TyB_abstract(x2,sz2,tyB_list2) ->
        if x1 <> x2 then raise NotEqual;
        compare_size sz2 sz2;
        List.iter2 compare_tyB tyB_list1 tyB_list2*)
    | _ -> raise NotEqual
  and compare_size sz1 sz2 =
    (match canon_size sz1,canon_size sz2 with
    | Sz_lit n, Sz_lit m -> if n <> m then raise NotEqual
    | Sz_var{contents=Is sz1'}, sz2'
    | sz1',Sz_var{contents=Is sz2'} -> compare_size sz1' sz2'
    | Sz_var{contents=Unknown n},Sz_var{contents=Unknown m} -> 
        (* Printf.printf "================ %d / %d \n" n m; *)
        if n <> m then raise NotEqual
    | Sz_var{contents=Unknown _},_ 
    | _,Sz_var{contents=Unknown _} -> raise NotEqual)
  and compare_ty t1 t2 =
    match canon_ty t1,canon_ty t2 with
    | Ty_var {contents=(Is t1')},t2' -> compare_ty t1' t2'
    | t1',Ty_var {contents=(Is t2')} -> compare_ty t1' t2'
    | Ty_var ({contents=Unknown _}),_ |
      _,Ty_var ({contents=Unknown _}) -> () (* will be unified in the generated code *)
    | Ty_base tc1, Ty_base tc2 ->
        compare_tyB tc1 tc2
    | Ty_tuple ts1, Ty_tuple ts2 ->
       if List.compare_lengths ts1 ts2 <> 0 then raise NotEqual;
        List.iter2 compare_ty ts1 ts2
    | Ty_fun(arg,dur,ret),Ty_fun(a,d,r) ->
       compare_ty arg a; compare_tyB ret r

   (* | T_add _,T_add _ ->
        ()
    | T_max _,T_max _ ->
        ()
    | T_le _,T_le _ ->
        ()
    | T_response_time _, T_response_time _ -> ()
    | T_infinity,T_infinity -> ()
    | T_ref t1', T_ref t2' -> compare_ty t1' t2'*)
    | Ty_array(sz1,tyB1),
      Ty_array(sz2,tyB2) -> 
          compare_size sz1 sz2; compare_tyB tyB1 tyB2
    (* | T_string sz1, T_string sz2 -> compare_ty sz1 sz2 *)
    | _ -> raise NotEqual
  in
  try compare_ty t1 t2; true with NotEqual -> false

let criterion f = 
  let lty = Hashtbl.find_all Typing.signatures f in  
  (* List.iter 
    (fun t -> Format.fprintf Format.std_formatter "---=> %a\n"
       Types.pp_ty t) lty;*)
  match lty with
  | [] -> true
  | ty::lty' -> List.for_all (eq_tys ty) lty';;
  

let monomorphize_exp e =
  let rec aux ds e =
    match Ast_undecorated.remove_deco e with
    | E_letIn(P_var f,ty,E_fix(_,(p,sigty,e1)),e2) -> 
        let v = E_fix(f,(p,sigty,aux ds e1)) in
        (* Printf.printf "---> %s %b\n " f (criterion f); *)
        if criterion f then E_letIn(P_var f,ty,v,aux ds e2)
        else aux ((f,(ty,v))::ds) e2
    | E_app(E_var f,arg) ->
        (* Printf.printf "---> %s %b\n " f (criterion f); *)
       if criterion f then e else
       (match List.assoc_opt f ds with
       | Some (ty,v) ->
           let g = Ast.gensym ~prefix:f () in
           E_letIn(P_var g,Types.new_ty_unknown(), Inline.subst_ty ty v,E_app(E_var g,arg))
       | None -> (* tail call *) e)
    | e -> Ast_mapper.map (aux ds) e
  in 
  aux [] e


let monomorphize pi =
  if !Typing.monomorphic then pi else 
  let _ = Typing.typing_with_argument ~collect_sig:true pi [] in
  let e = monomorphize_exp pi.main in
  { pi with main = e }


