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
  | _ -> assert false (* todo: better error message *)

let subst_lc x ex lc = 
  match lc with
  | St_var l' -> if x = l' 
                 then (match un_deco ex with
                       | E_const c -> St_const c
                       | E_var y -> St_var y
                       | _ -> let open Prelude.Errors in
                               error (* ~loc*) (fun fmt ->
                               Format.fprintf fmt
                                "@[<v>value %s should be statically known.@]" x))
                 else lc 
  | St_const _ -> lc

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
    | E_lastIn(y,e1,e2) ->
        if x = y then e else E_lastIn(y,ss e1,ss e2)
    | E_set(y,e1) ->
        let z = if x <> y then y else as_ident ex in
        E_set(z,ss e1)
    | E_static_array_length(y) ->
        let z = if x <> y then y else as_ident ex in
        E_static_array_length(z)
    | E_static_array_get(y,e1) ->
        let z = if x <> y then y else as_ident ex in
        E_static_array_get(z, ss e1)
    | E_static_array_set(y,e1,e2) ->
        let z = if x <> y then y else as_ident ex in
        E_static_array_set(z, ss e1, ss e2)
    | E_appLabel(e1,l,lc) ->
        let lc' = subst_lc x ex lc in
        E_appLabel(ss e1,l,lc')
    | E_for(y,e_st1,e_st2,e3,loc) ->
       E_for(y,ss e_st1,ss e_st2,
             (if x = y then e3 else ss e3),loc)
    | E_generate((p,e1),e2,e_st3,loc) ->
        let e1' = if pat_mem x p then e1 else ss e1 in
        E_generate((p,e1'),ss e2,ss e_st3,loc)
    | e -> Ast_mapper.map ss e
  in
  ss e

let rec map_subst_p (ss : (x -> e -> 'a -> 'a)) (p:p) (ep:e) (o:'a) : 'a =
  let m = bindings p ep in
  SMap.fold (fun x ex o -> ss x ex o) m o

let subst_p_e p ep o =
  map_subst_p subst_e p ep o


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
      | E_lastIn(y,e1,e2) ->
          let env' = SMap.remove y env in
          E_lastIn(y,ss env e1,ss env' e2)
      | E_set(y,e1) ->
          let z = as_ident_env y env in
          E_set(z,ss env e1)
      | e -> Ast_mapper.map (ss env) e
    in
    ss env e

  let subst_p_e p ep e =
    subst_e_env (bindings p ep) e
end


let subst_lc x lc lc' =
  match lc with
  | St_var l2 -> if l2 = x then lc' else lc
  | _ -> lc

let subst_label l1 l2 e =
  let subst_l l = if l = l1 then l2 else l in
  let rec ss e =
    match e with
    (* 
       E_reg _ | E_exec _ | E_set -> ? *)
    | E_static_array_get(l,e1) ->
        
        E_static_array_get(subst_l l,ss e1)
    | E_static_array_length(l) ->
        E_static_array_length(subst_l l)
    | E_static_array_set(l,e1,e2) ->
        E_static_array_set(subst_l l,ss e1,ss e2)
    | E_absLabel(l,e1) -> if l = l1 then e else E_absLabel(l,ss e1)
    | E_appLabel(e1,l,St_var l') -> 
        E_appLabel(ss e1,l,St_var (subst_l l'))
    | E_appLabel(e1,l,(St_const _ as lc)) -> 
        if l = l1 then e else E_appLabel(ss e1,l,lc)
    | E_for(x,e_st1,e_st2,e3,loc) ->
        E_for(x,ss e_st1, ss e_st2, ss e3,loc)
    | E_generate((p,e1),e2,e_st3,loc) ->
        (* assume l not in p ? *)
        E_generate((p,ss e1),ss e2,ss e_st3,loc)
    | e -> Ast_mapper.map ss e
  in
  ss e


let subst_const l1 c e =
  let rec ss e =
    match e with
    | E_var x -> if l1 = x then E_const c else e
    | E_absLabel(l,e1) -> if l = l1 then e else E_absLabel(l,ss e1)
    | E_appLabel(e1,l,lc) -> 
        let lc' = subst_lc l1 lc (St_const c) in
        E_appLabel(ss e1,l,lc')
    | E_for(x,e_st1,e_st2,e1,loc) ->
        E_for(x,ss e_st1, ss e_st2,ss e1,loc)
    | E_generate((p,e1),e2,e_st3,loc) ->
        (* assume l not in p ? *)
        E_generate((p,ss e1),ss e2,ss e_st3,loc)
    | e -> Ast_mapper.map ss e
  in
  ss e

let rec app_label e l l' =
  match e with
  | E_letIn(p,e1,e2) -> E_letIn(p,subst_label l l' e1,app_label e2 l l')
  | E_if(e,e1,e2) -> E_if(subst_label l l' e,app_label e1 l l',app_label e2 l l')
  | E_absLabel(l2,e1) -> 
      if l <> l2 then failwith ("app_label") else subst_label l l' e1
  | E_appLabel(e1,l1,lc) ->
     app_label (app_labelC e1 l1 lc) l l'
  | e -> Ast_mapper.map (fun e -> app_label e l l') e


and app_const e l c =
  match e with
  | E_letIn(p,e1,e2) -> E_letIn(p,subst_const l c e1,app_const e2 l c)
  | E_if(e,e1,e2) -> E_if(subst_const l c e,app_const e1 l c,app_const e2 l c)
  | E_absLabel(l2,e1) -> 
      if l <> l2 then failwith "app_const" else subst_const l2 c e1
  | E_appLabel(e1,l1,lc) ->
     app_const (app_labelC e1 l1 lc) l c
  | e -> Ast_mapper.map (fun e -> app_const e l c) e


and app_labelC e l lc =
  match lc with
  | St_var l' -> app_label e l l'
  | St_const c -> app_const e l c


