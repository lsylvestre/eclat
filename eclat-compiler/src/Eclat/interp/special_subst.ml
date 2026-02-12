open Ast
open Pattern

(* capture avoiding substitution that is compatible with
   new constructs [reg] and [exec] *)


let prefix id e =
  let prefix_str = String.concat "" (List.map string_of_int id) in
  let rec ren_e = function
  | E_reg((p,tyB,e1),e0,l) ->
      E_reg((p,tyB,ren_e e1),ren_e e0,prefix_str^l)
  | E_exec(e1,e2,e3,l) ->
      E_exec(ren_e e1,ren_e e2,Option.map ren_e e3,prefix_str^l)
  | E_run(f,e0,l) ->
      E_run(f,ren_e e0,prefix_str^l)
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
| P_tyConstr(p,ty) -> P_tyConstr(subst_p x ex p,ty)

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
    | E_letIn(p, ty, e1, e2) ->
        let e1' = ss (0::id) e1 in
        if pat_mem x p 
        then E_letIn(p, ty, e1', e2)
        else let e2' = ss (1::id) e2 in
             E_letIn(p, ty, e1', e2')
    | E_fun(p, sigty, e1) ->
        if pat_mem x p 
        then e 
        else let e1' = ss (0::id) e1 in
              E_fun(p, sigty, e1')
    | E_fix(f, (p, sigty, e1)) ->
        if x = f || pat_mem x p then e else E_fix(f,(p,sigty, ss (0::id) e1))
    | E_match(e1,hs,eo) ->
        let hs' = List.mapi (fun i (inj,(p,ei)) ->
                              let ei' = if pat_mem x p then ei else ss ((i+1)::id) ei in
                              (inj,(p,ei'))) hs in
        E_match((ss (0::id) e1), hs', Option.map (fun e -> ss ((List.length hs + 1)::id) e) eo)
    | E_case(e1,hs,e_els) ->
        E_case(ss (0::id) e1,List.mapi (fun i (c,e) -> c,ss (i+1::id) e1) hs,
          ss ((List.length hs + 1)::id) e_els)
    | E_reg((p,tyB,e1),e0,l) ->
        let e1' = if pat_mem x p then e1 else ss (1::id) e1 in
        E_reg((p,tyB,e1'),ss (0::id) e0,l)
    | E_exec(e1,e2,eo,l) ->
        E_exec(ss (0::id) e1,ss (1::id) e2,Option.map (ss (2::id)) eo,l)
    | E_record(b_list) ->
        E_record(List.mapi (fun i (xi,ei) -> xi,ss (i::id) ei) b_list)
    | E_record_field(e1,x,t) ->
        E_record_field(ss (0::id) e1, x,t)
    | E_record_update(e1,x2,e2,t) ->
        E_record_update(ss (0::id) e1, x2, ss (1::id) e2,t)
    | E_array_length(y,loc) ->
        let z = if x <> y then y else as_ident ex in
        E_array_length(z,loc)
    | E_array_make(sz,e1,loc) ->
        let e1' = ss (0::id) e1 in
        E_array_make(sz,e1',loc)
    | E_array_create _ ->
        e
    | E_array_get((y,loc),e1) ->
        let z = if x <> y then y else as_ident ex in
        E_array_get((z,loc), ss (0::id) e1)
    | E_array_set((y,loc),e1,e2) ->
        let z = if x <> y then y else as_ident ex in
        E_array_set((z,loc), ss (0::id) e1, ss (1::id) e2)
    | E_array_get_start((y,loc),e1) ->
        let z = if x <> y then y else as_ident ex in
        E_array_get_start((z,loc), ss (0::id) e1)
    | E_array_get_end(y,loc) ->
        let z = if x <> y then y else as_ident ex in
        E_array_get_end(z,loc)
    | E_array_set_immediate((y,loc),e1,e2) ->
        let z = if x <> y then y else as_ident ex in
        E_array_set_immediate((z,loc), ss (0::id) e1, ss (1::id) e2)
    | E_array_from_file(y,e1) ->
        let z = if x <> y then y else as_ident ex in
        E_array_from_file(z, ss (0::id) e1)
    | E_for(y,sz1,sz2,e3,loc) ->
       E_for(y,sz1,sz2,
             (if x = y then e3 else ss (2::id) e3),loc)
    | E_generate((p,tyB,e1),e2,sz3,sz4,loc) ->
        let e1' = if pat_mem x p then e1 else ss (0::id) e1 in
        E_generate((p,tyB,e1'),ss (1::id) e2,sz3,sz4,loc)
    | E_vector_mapi(is_par,(p,tyB,e1),e2,ty) ->
        let e1' = if pat_mem x p then e1 else ss (0::id) e1 in
        E_vector_mapi(is_par,(p,tyB,e1'),ss (1::id) e2,ty)
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
    | E_run(i,e1,l) ->
        let e1' = ss (0::id) e1 in
        E_run(i, e1',l)
    | E_pause (l,e1) -> 
        let e1' = ss (0::id) e1 in
        E_pause (l,e1')
    | E_sig_get y ->
        let z = if x <> y then y else as_ident ex in
        E_sig_get(z)
    | E_emit(y,e1) ->
        let e1' = ss (0::id) e1 in
        E_emit(y,e1')
    | E_sig_create(e1) ->
        let e1' = ss (0::id) e1 in
        E_sig_create(e1')
    | E_loop(e1) ->
        let e1' = ss (0::id) e1 in
        E_loop(e1')
    | E_trap _ -> e
    | E_exit(x,e1) ->
        let e1' = ss (0::id) e1 in
        E_exit(x,e1')
    | E_suspend(e1,x) ->
        let e1' = ss (0::id) e1 in
        E_suspend(e1',x)
    | E_assert(e1,loc) ->
        let e1' = ss (0::id) e1 in
        E_assert(e1',loc)
  in
  ss [] e

let rec map_subst_p (ss : (x -> e -> 'a -> 'a)) (p:p) (ep:e) (o:'a) : 'a =
  let m = bindings p ep in
  SMap.fold (fun x ex o -> ss x ex o) m o

let subst_p_e p ep o =
  map_subst_p subst_e p ep o

