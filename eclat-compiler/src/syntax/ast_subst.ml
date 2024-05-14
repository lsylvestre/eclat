open Ast
open Pattern

(* capture avoiding substitution *)

let subst_ident x ex y =
  if x = y then
    (match un_annot ex with
     | E_var z -> z
     | _ -> assert false) else y

let rec subst_p x ex = function
| P_unit -> P_unit
| P_var y -> P_var (subst_ident x ex y)
| P_tuple(ps) -> P_tuple (List.map (subst_p x ex) ps)

let as_ident ex =
  match ex with
  | E_var z -> z
  | _ -> Ast_pprint.pp_exp Format.std_formatter ex; assert false (* todo: better error message *)

let subst_e x ex e =
  let rec ss e =
    match e with
    | E_var y ->
        if y = x then ex else e
    | E_letIn(p,e1,e2) ->
        if pat_mem x p then E_letIn(p, ss e1, e2)
        else E_letIn(p, ss e1, ss e2)
    | E_fun(p,e1) ->
        if pat_mem x p then e else E_fun(p,ss e1)
    | E_fix(f,(p,e1)) ->
        if x = f || pat_mem x p then e else E_fix(f,(p,ss e1))
    | E_match(e1,hs,eo) ->
        let hs' = List.map (fun (inj,(p,ei)) ->
                              let ei' = if pat_mem x p then ei else ss ei in
                              (inj,(p,ei'))) hs in
        E_match(ss e1, hs', Option.map ss eo)
    | E_reg((p,e1),e0,l) ->
        let e1' = if pat_mem x p then e1 else ss e1 in
        E_reg((p,e1'),ss e0,l)
    | E_array_length(y) ->
        let z = if x <> y then y else as_ident ex in
        E_array_length(z)
    | E_local_static_array(e1,deco) ->
        E_local_static_array(ss e1,deco)
    | E_array_get(y,e1) ->
        let z = if x <> y then y else as_ident ex in
        E_array_get(z, ss e1)
    | E_array_set(y,e1,e2) ->
        let z = if x <> y then y else as_ident ex in
        E_array_set(z, ss e1, ss e2)
    | E_local_static_matrix(e1,es,deco) ->
        E_local_static_matrix(ss e1,List.map ss es,deco)
    | E_matrix_size(y,n) ->
        let z = if x <> y then y else as_ident ex in
        E_matrix_size(z,n)
    | E_matrix_get(y,es) ->
        let z = if x <> y then y else as_ident ex in
        E_matrix_get(z,List.map ss es)
    | E_matrix_set(y,es,e2) ->
        let z = if x <> y then y else as_ident ex in
        E_matrix_set(z,List.map ss es, ss e2)
    | E_for(y,e_st1,e_st2,e3,loc) ->
       E_for(y,ss e_st1,ss e_st2,
             (if x = y then e3 else ss e3),loc)
    | E_generate((p,e1),e2,e_st3,loc) ->
        let e1' = if pat_mem x p then e1 else ss e1 in
        E_generate((p,e1'),ss e2,ss e_st3,loc)
    | E_vector_mapi(is_par,(p,e1),e2,ty) ->
        let e1' = if pat_mem x p then e1 else ss e1 in
        E_vector_mapi(is_par,(p,e1'),ss e2,ty)
    | E_int_mapi(is_par,(p,e1),e2,ty) ->
        let e1' = if pat_mem x p then e1 else ss e1 in
        E_int_mapi(is_par,(p,e1'),ss e2,ty)
    | e -> Ast_mapper.map ss e
  in
  ss e

let rec map_subst_p (ss : (x -> e -> 'a -> 'a)) (p:p) (ep:e) (o:'a) : 'a =
  let m = bindings p ep in
  SMap.fold (fun x ex o -> ss x ex o) m o

let subst_p_e p ep o =
  map_subst_p subst_e p ep o


(* 
module OtherVersion = struct

  (* variant with simultaneous substitution: seems less efficient
     than [subst_e x ex e] in practice *)

  let filter_env p env =
    let xs = vars_of_p p in
    SMap.filter (fun x _ -> not (SMap.mem x xs)) env

  let subst_e_env env e =
    let as_ident_env z env =
      match SMap.find_opt z env with
      | Some ez -> (match un_annot ez with
                   | E_var x -> x
                   | _ -> invalid_arg "subst_e_env")
      | None  -> z
    in
    let rec ss env e =
      if SMap.is_empty env then e else
      match e with
      | E_var y ->
          (try SMap.find y env with Not_found -> e)
      | E_letIn(p,e1,e2) ->
          let env' = filter_env p env in
          E_letIn(p, ss env e1, ss env' e2)
      | E_fun(p,e1) ->
          let env' = filter_env p env in
          E_fun(p,ss env' e1)
      | E_fix(f,(p,e1)) ->
          let env' = SMap.remove f (filter_env p env) in
          E_fix(f,(p,ss env' e1))
      | E_match(e1,hs,eo) ->
          let hs' = List.map (fun (inj,(p,ei)) ->
                                let env' = filter_env p env in
                                (inj,(p,ss env' ei))) hs in
          E_match(ss env e1, hs', Option.map (ss env) eo)
      | E_reg((p,e1),e0,l) ->
        let env' = filter_env p env in
        E_reg((p,ss env' e1),ss env e0,l)
      | E_set(y,e1) ->
          let z = as_ident_env y env in
          E_set(z,ss env e1)
      | ...............................
      | e -> Ast_mapper.map (ss env) e
    in
    ss env e

  let subst_p_e p ep e =
    subst_e_env (bindings p ep) e
end

*)


