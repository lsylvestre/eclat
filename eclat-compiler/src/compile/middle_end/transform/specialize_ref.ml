open Ast

let has_changed = ref false 

let wrap_fix_in_fun e =
  let rec aux env e = match e with
  | E_letIn(P_var f, E_fix(f',(p,e1)), e2) ->
      ((* Printf.printf "-->%s / %s\n" f f'; *)
       let t = try Hashtbl.find Typing.signatures f with Not_found -> Types.unknown() (*TODO : check *) in
       let open Types in
       let targ = match canon t with T_fun{arg} -> arg | _ -> Types.unknown() (* assert false*) in
       let rec has_ref t = match canon t with 
        | T_ref _ | T_array _ -> true 
        | T_tuple ts -> List.exists has_ref ts
        | T_fun{arg} -> has_ref arg (* warning: should not happen *)
        | T_var{contents=Ty t} -> has_ref t
        | _ -> false in
        
        
        let e1' = aux ((f',(None,targ))::env) e1 in
        let v = E_fix(f',(p,e1')) in
        if has_ref t then (has_changed := true;
          aux ((f,(Some v,targ))::env) e2 
        )else 
          E_letIn(P_var f, E_fix(f',(p,e1')), aux env e2))
  | E_app(E_var g,e1) ->
     (match List.assoc_opt g env with
     | None -> E_app(E_var g,e1)
     | Some (None, targ) -> 
       let open Types in
            let rec aux2 t e = (* Printf.printf "====>--\n"; *)
             let t = match canon t with T_var{contents=Ty t} -> t | t -> t in  
             (* Format.fprintf Format.std_formatter "%s -- /////////////// %a\n" g Ast_pprint.pp_ty t;
             Format.fprintf Format.std_formatter "%s -- /////////////// %a\n" g Ast_pprint.pp_exp e;*)
             match (t,e) with
             | _,E_const _ -> e
             | (T_ref _ | T_array _),E_var _ -> E_const Unit
             | (T_ref _ | T_array _),_ -> assert false
             | T_tuple ts, E_tuple es ->
                let rec loop acce ts es =
                  match ts,es with
                  | [],_|_,[] -> 
                      E_tuple (List.rev acce)
                  | t::ts',e::es' -> 
                      let e' = aux2 t e in
                      loop (e'::acce) ts' es'
                   in loop [] ts es
              | _,e -> e
          in
     E_app(E_var g,aux2 targ e1)
     | Some (Some (E_fix(f,(p,e0))), targ) -> 
            let open Types in
            let rec aux2 t p e = 
             let t = match canon t with T_var{contents=Ty t} -> t | t -> t in
             (* Format.fprintf Format.std_formatter "-- /////////////// %a\n" Ast_pprint.pp_ty t; *)
             match (t,p,e) with
             | _,P_unit,_ -> P_unit,e
             | (T_ref _ | T_array _ ),P_var x,E_var y -> P_unit, E_const Unit
             | T_tuple ts, P_tuple ps, E_tuple es ->
                let rec loop accp acce ts ps es =
                  match ts,ps,es with
                  | [],_,_|_,[],_|_,_,[] -> 
                      P_tuple (List.rev accp), E_tuple (List.rev acce)
                  | t::ts',p::ps',e::es' -> 
                      let p',e' = aux2 t p e in
                      loop (p'::accp) (e'::acce) ts' ps' es'
                   in loop [] [] ts ps es  
             (*| T_tuple ts,P_tuple _,_ -> 
                  let xs = List.map (fun _ -> Ast.gensym ()) ts in
                  let p',e' = aux2 t p (E_tuple (List.map (fun x -> E_var x) xs)) in
                  p', E_letIn(P_tuple (List.map (fun x -> P_var x) xs),e,e')
                  (* let p',e' = aux2 t p (* (E_letIn(p,e,*)(Pattern.pat2exp p) in*)*)
             | _,p,e -> p,e
          in
          let e1_ = aux env e1 in
          let p',e1' = aux2 targ p e1_ in 
         E_letIn(p,e1_,
         E_letIn(P_var g, E_fix(f,(p',e0)),E_app(E_var g, e1')))
     | Some _ -> assert false)
  | e -> Ast_mapper.map (aux env) e
  in aux [] e

let specialize_ref pi =
  has_changed := false;
  let _ = Typing.typing_with_argument ~collect_sig:true pi [] in
  let e = wrap_fix_in_fun (Propagation.propagation pi.main) in
          (* Format.fprintf Format.std_formatter "---=> %a\n" Ast_pprint.pp_exp e; *)
  (* if !has_changed then Rename *)
  { pi with main =  e }