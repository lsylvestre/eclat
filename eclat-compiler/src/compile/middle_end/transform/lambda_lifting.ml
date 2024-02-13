open Ast
open Pattern

(**

    Lambda-lifting [Johnsson, 1982]:

    make explicit all lexical environments
    and globalize all functions.

    Our version of the algorithmic works in two steps:
    - [lifting] adds free variables of each function definition, as a tuple,
      into the second parameter (environment) of the function definition
      and all these call. [lift] has to be repeat until all functions are close
    - [globalize] (optional) globalizes all (close) functions.
      The order of globalized bindings is important
      since global functions are *not* mutually-recursive.
**)

(** [fv ~statics ~decls e] returns the free variables of [e] that are not
    global definitions (bound in ~static and ~decls) *)
let fv ~statics e =
  SMap.filter (fun x _ -> not (SMap.mem x statics)) (Free_vars.fv e)

(** [has_changed] boolean flag setted to true each time a [lift] pass modifies
    the input expression *)
let has_changed = ref false

(* bind each function name to its lexical environment
    (as a collection of name grouped in a pattern) *)
type env = p smap

let declare ds e =
  List.fold_right (fun (x,v) e -> E_letIn(P_var x,v,e)) ds e

let rec rename_fun_e = function
  | E_letIn(P_var f,((E_fun _ | E_fix _) as phi),e2) ->
      let g = gensym ~prefix:f () in
      E_letIn(P_var g,rename_fun_e @@ phi,rename_fun_e @@ Ast_subst.subst_e f (E_var g) e2)
  | e -> Ast_mapper.map rename_fun_e e


(** [lifting ~statics ~decls e] lifts expression [e]
    considering [~statics] and [~decls] as toplevel definitions
    that should not be added to lexical environments. *)
let lifting ~statics (env:env) (e:e) : e =
  let env_filter env p =
    let xs = vars_of_p p in
    SMap.filter (fun x _ -> not @@ SMap.mem x xs) env
   in
  let rec lift env e =
    let open Ast in
    match e with
    | E_app((E_const (Op(TyConstr _))),_) ->
        assert false
    | E_app((E_const _) as e1,e2) ->
        E_app(e1,lift env e2)
    | E_app(E_var f,e2) ->
        let e2' = lift env e2 in
        (match SMap.find_opt f env with
         | None | Some (P_unit) -> E_app(E_var f,e2')
         | Some p ->
            E_app(E_var f,E_tuple[e2'; pat2exp p]))
    | E_app(e1,e2) ->
         assert (evaluated e1); lift env @@
         let f = gensym () in
         E_letIn(P_var f,e1,E_app(E_var f,e2))
    | E_letIn(P_var f,(E_fun(p,e1) as phi),e2) ->
        let e1' = lift env e1 in
        let xs = fv ~statics phi in
        let vp = (vars_of_p p) in
        let vp = xs |> SMap.filter (fun x _ ->
                             not (SMap.mem x vp) && not (SMap.mem x env)) in
        let p_env' = vp |> SMap.bindings
                        |> List.map (fun (x,_) -> P_var x)
                        |> group_ps in
        if not (SMap.is_empty (vars_of_p p_env')) then
          (has_changed := true;
           let f,e2 = (* renaming in the case a same name [f]
                         is in the environment of function [f] *)
                      if SMap.mem f vp
                      then let f' = gensym ~prefix:f () in
                           let e2' = Ast_subst.subst_e f (E_var f') e2
                           in (f',e2')
                      else f,e2 in
           let env2, ef = (SMap.add f p_env' env, E_fun(P_tuple[p;p_env'],e1')) in
           E_letIn(P_var f,ef,lift env2 e2) )
        else
           let env2 = SMap.add f p_env' env in
           E_letIn(P_var f,(E_fun(p,e1')),lift env2 e2)
    | E_letIn(P_var f,(E_fix(g,(p,e1)) as phi),e2) ->
        let xs = fv ~statics phi in
        let vp = (vars_of_p p) in
        let p_env' = xs |> SMap.filter (fun x _ ->
                            not (SMap.mem x vp) && not (SMap.mem x env) && x <> g)
                        |> SMap.bindings
                        |> List.map (fun (x,_) -> P_var x)
                        |> group_ps in
        let e1' = lift (SMap.add g p_env' env) e1 in
        let e2' = lift (SMap.add f p_env' env) e2 in
        if not (SMap.is_empty (vars_of_p p_env')) then (
          has_changed := true;
          let ef = E_fix(g,(P_tuple[p;p_env'],e1')) in
          E_letIn(P_var f,ef,e2'))
         else
          (E_letIn(P_var f,(E_fix(g,(p,e1'))),e2') )
    | E_letIn(p,e1,e2) ->
        let env' = env_filter env p in  (* todo: idem with E_match to avoid capture *)
        E_letIn(p,lift env e1,lift env' e2)
    | E_match(e1,hs,eo) ->
        let e1' = lift env e1 in
        let hs' = List.map (fun (inj,(p,ei)) ->
                     let env' = env_filter env p in
                     (inj,(p,lift env' ei))) hs in
      E_match(e1',hs',Option.map (lift env) eo)

    | E_reg((p,e1),e0,l) ->
        let env' = env_filter env p in
        E_reg((p,lift env' e1),lift env e0,l)
    | e -> Ast_mapper.map (lift env) e
  in lift env e

(** lifting has to be perform several time until reaching a fixpoint
    where [has_changed] remains false *)
let rec lift_until ~statics (e:e) =
  has_changed := false;
  let e' = lifting ~statics SMap.empty e in
  if !has_changed then lift_until ~statics e' else e'



(** [globalize e] globalizes all local *close* functions in expression [e] *)
let globalize_e (e:e) : ((x * e) list * e) =
  let rec glob e =
    let open Ast in
      let globalize_list es =
        let rec loop dss es_acc es =
          match es with
          | [] -> List.concat (List.rev dss), List.rev es_acc
          | ei::es' ->
             let (dsi,ei') = glob ei in
             loop (dsi::dss) (ei'::es_acc) es'
      in loop [] [] es
    in
    match e with
    | E_deco _ ->
        Ast_undecorated.still_decorated e
    | E_const _ | E_var _ -> [],e
    | E_tuple es ->
        let ds,es' = globalize_list es in
        ds,E_tuple(es')
    | E_app(e1,e2) ->
        let ds1,e1' = glob e1 in
        let ds2,e2' = glob e2 in
        ds1@ds2,E_app(e1',e2')
    | E_letIn(P_var f,(E_fix _ | E_fun _ as v),e2) ->
        let dsv,v = glob v in
        let ds,e2' = glob e2 in
        (dsv@[(f,v)]@ds),e2'
    | E_fix(f,(p,e1)) ->
        let ds1,e1' = glob e1 in
        ds1,E_fix(f,(p,e1'))
    | E_fun(p,e1) ->
        let ds1,e1' = glob e1 in
        ds1,E_fun(p,e1')
    | E_if(e1,e2,e3) ->
        let ds1,e1' = glob e1 in
        let ds2,e2' = glob e2 in
        let ds3,e3' = glob e3 in
        ds1@ds2@ds3,E_if(e1',e2',e3')
    | E_case(e1,hs,e_els) ->
      let ds1,e1' = glob e1 in
      let dss,hs' = List.split @@ List.map (fun (c,e) -> let ds,e' = glob e in ds,(c,e')) hs in
      let ds,e_els' = glob e_els in
      ds1@List.concat dss@ds, E_case(e1',hs',e_els')
    | E_match(e1,hs,eo) ->
      let ds1,e1' = glob e1 in
      let dss,hs' = List.split @@ List.map (fun (x,(p,e)) -> let ds,e' = glob e in ds,(x,(p,e'))) hs in
      let dsw,eo' = match eo with
                    | None -> [],eo
                    | Some ew -> let dsw,ew' = glob ew in
                                 (dsw,Some ew')
      in
      ds1@List.concat dss@dsw, E_match(e1',hs',eo')
    | E_letIn(p,e1,e2) ->
        let ds1,e1' = glob e1 in
        let ds2,e2' = glob e2 in
        ds1@ds2,E_letIn(p,e1',e2')
    | E_lastIn(x,e1,e2) ->
        let ds1,e1' = glob e1 in
        let ds2,e2' = glob e2 in
        ds1@ds2,E_lastIn(x,e1',e2')
    | E_set(x,e1) ->
        let ds1,e1' = glob e1 in
        ds1,E_set(x,e1')
    | E_static_array_get(x,e1) ->
        let ds1,e1' = glob e1 in
        ds1,E_static_array_get(x,e1')
    | E_static_array_length _ ->
        [],e
    | E_static_array_set(x,e1,e2) ->
        let ds1,e1' = glob e1 in
        let ds2,e2' = glob e2 in
        ds1@ds2,E_static_array_set(x,e1',e2')
    | E_par(e1,e2) ->
        let ds1,e1' = glob e1 in
        let ds2,e2' = glob e2 in
        ds1@ds2,E_par (e1',e2')
    | E_reg((p,e1),e0,l) ->
        let ds1,e1' = glob e1 in
        let ds0,e0' = glob e0 in
        ds0,E_reg((p,declare ds1 e1'),e0',l)
    | E_exec(e1,e0,l) ->
        let ds1,e1' = glob e1 in
        let ds0,e0' = glob e0 in
        ds0,E_exec(declare ds1 e1',e0',l)
  in glob e



(** [lambda_lifting ~statics e] lambda-lifts expression [e],
    considering [~statics] as toplevel definitions
    that should not be added to lexical environments. *)
let lambda_lifting ~statics ~globalize (e:e) : ((x * e) list * e) =
    let e_lifted = (lift_until ~statics e) in
    if globalize then globalize_e (rename_fun_e e_lifted) else ([],e_lifted)


(** [lambda_lifting_pi ~globalize:true pi] lambda-lifts program [pi]. *)
let lambda_lifting_pi ?(globalize=false) (pi:pi) : pi =
  let statics = smap_of_list pi.statics in
  let (ds,e) = lambda_lifting ~statics ~globalize pi.main in
  let main = declare ds e in
  {pi with main}
