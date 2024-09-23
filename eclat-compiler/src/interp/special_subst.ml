open Ast
open Pattern

(* capture avoiding substitution that is compatible with
   new constructs [reg] and [exec] *)


let prefix id e =
  let prefix_str = String.concat "" (List.map string_of_int id) in
  let rec ren_e = function
  | E_reg((p,e1),e0,l) ->
      E_reg((p,ren_e e1),ren_e e0,prefix_str^l)
  | E_exec(e1,e2,e3,l) ->
      E_exec(ren_e e1,ren_e e2,Option.map ren_e e3,prefix_str^l)
  | e -> Ast_mapper.map ren_e e
  in
  ren_e e

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
  | _ -> 
      Prelude.Errors.error ~error_kw:"Compile-time Error"
         (fun fmt -> Format.fprintf fmt "identifier expected instead of %a" Ast_pprint.pp_exp ex)

let subst_e x ex e =
  let rec ss id e =
    match e with
    | E_deco(e,loc) ->
        E_deco(ss id e,loc)
    | E_var y ->
        if y = x then prefix id @@ ex else e
    | E_const _ -> e
    | E_if(e1, e2, e3) ->
        let e1' = ss (0::id) e1 in
        let e2' = ss (1::id) e2 in
        let e3' = ss (2::id) e3 in
        E_if(e1', e2', e3')
    | E_app(e1, e2) ->
        let e1' = ss (0::id) e1 in
        let e2' = ss (1::id) e2 in
        E_app(e1', e2')
    | E_tuple(es) ->
        let es' = List.mapi (fun i ei -> ss (i::id) ei) es in
        E_tuple es'
    | E_par(es) ->
        let es' = List.mapi (fun i ei -> ss (i::id) ei) es in
        E_par es'
    | E_letIn(p, e1, e2) ->
        let e1' = ss (0::id) e1 in
        if pat_mem x p 
        then E_letIn(p, e1', e2)
        else let e2' = ss (1::id) e2 in
             E_letIn(p, e1', e2')
    | E_fun(p, e1) ->
        if pat_mem x p 
        then e 
        else let e1' = ss (0::id) e1 in
              E_fun(p, e1')
    | E_fix(f, (p, e1)) ->
        if x = f || pat_mem x p then e else E_fix(f,(p,ss (0::id) e1))
    | E_match(e1,hs,eo) ->
        let hs' = List.mapi (fun i (inj,(p,ei)) ->
                              let ei' = if pat_mem x p then ei else ss ((i+1)::id) ei in
                              (inj,(p,ei'))) hs in
        E_match((ss (0::id) e1), hs', Option.map (fun e -> ss ((List.length hs + 1)::id) e) eo)
    | E_case(e1,hs,e_els) ->
        E_case(ss (0::id) e1,List.mapi (fun i (c,e) -> c,ss (i+1::id) e1) hs,
          ss ((List.length hs + 1)::id) e_els)
    | E_reg((p,e1),e0,l) ->
        let e1' = if pat_mem x p then e1 else ss (1::id) e1 in
        E_reg((p,e1'),ss (0::id) e0,l)
    | E_exec(e1,e2,eo,l) ->
        E_exec(ss (0::id) e1,ss (1::id) e2,Option.map (ss (2::id)) eo,l)
    | E_array_length(y) ->
        let z = if x <> y then y else as_ident ex in
        E_array_length(z)
    | E_array_make(sz,e1,loc) ->
        let e1' = ss (0::id) e1 in
        E_array_make(sz,e1',loc)
    | E_array_create _ ->
        e
    | E_array_get(y,e1) ->
        let z = if x <> y then y else as_ident ex in
        E_array_get(z, ss (0::id) e1)
    | E_array_set(y,e1,e2) ->
        let z = if x <> y then y else as_ident ex in
        E_array_set(z, ss (0::id) e1, ss (1::id) e2)
    | E_for(y,e_st1,e_st2,e3,loc) ->
       E_for(y,ss (0::id) e_st1,ss (1::id) e_st2,
             (if x = y then e3 else ss (2::id) e3),loc)
    | E_generate((p,e1),e2,e_st3,loc) ->
        let e1' = if pat_mem x p then e1 else ss (0::id) e1 in
        E_generate((p,e1'),ss (1::id) e2,ss (2::id) e_st3,loc)
    | E_vector_mapi(is_par,(p,e1),e2,ty) ->
        let e1' = if pat_mem x p then e1 else ss (0::id) e1 in
        E_vector_mapi(is_par,(p,e1'),ss (1::id) e2,ty)
    | E_int_mapi(is_par,(p,e1),e2,ty) ->
        let e1' = if pat_mem x p then e1 else ss (0::id) e1 in
        E_int_mapi(is_par,(p,e1'),ss (1::id) e2,ty)
    | E_vector(es) ->
        E_vector(List.mapi (fun i e -> ss (i::id) e) es)
    | E_ref(e1) ->
        let e1' = ss (0::id) e1 in
        E_ref e1'
    | E_get(e1) ->
        let e1' = ss (0::id) e1 in
        E_get e1'
    | E_set(e1, e2) ->
        let e1' = ss (0::id) e1 in
        let e2' = ss (1::id) e2 in
        E_set(e1', e2')
    | E_run(i,e1) ->
        let e1' = ss (0::id) e1 in
        E_run(i, e1')
  in
  ss [] e

let rec map_subst_p (ss : (x -> e -> 'a -> 'a)) (p:p) (ep:e) (o:'a) : 'a =
  let m = bindings p ep in
  SMap.fold (fun x ex o -> ss x ex o) m o

let subst_p_e p ep o =
  map_subst_p subst_e p ep o

