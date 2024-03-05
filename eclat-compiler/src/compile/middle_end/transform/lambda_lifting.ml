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
  SMap.filter (fun x _ -> not (SMap.mem x statics)) (Free_vars.fv ~get_arrays:true e)

(** [has_changed] boolean flag setted to true each time a [lift] pass modifies
    the input expression *)
let has_changed = ref false

(* bind each function name to its lexical environment
    (as a collection of name grouped in a pattern) *)
type env = p smap

let rec rename_fun_e = function
  | E_letIn(P_var f,((E_fun _ | E_fix _) as phi),e2) ->
      let g = gensym ~prefix:f () in
      E_letIn(P_var g,rename_fun_e @@ phi,rename_fun_e @@ Ast_subst.subst_e f (E_var g) e2)
  | e -> Ast_mapper.map rename_fun_e e

let wrap_fix_in_fun e =
  let rec aux env e = match e with
  | E_fix(f,((P_tuple[p;penv] as pp),e1)) ->
        (match penv with
        | P_unit -> e
        | _ ->
            E_fun(pp,
              E_letIn(P_var f,
                 E_fix(f,(p,aux (f::env) e1)), 
                 E_app(E_var f,pat2exp p))))
  | E_app(E_var g,E_tuple[e1;e2]) when List.mem g env ->
       E_app(E_var g,e1)
    | e -> Ast_mapper.map (aux env) e
  in aux [] e

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


    (* | E_absLabel(l,e1) -> 
        let env' = env_filter env (P_var l) in
        E_absLabel(l,lift env' e1)*)
    | E_for(x,lc1,lc2,e1,loc) ->
        let env' = env_filter env (P_var x) in
        E_for(x,lc1,lc2,lift env' e1,loc)
    | e -> Ast_mapper.map (lift env) e
  in lift env e

(** lifting has to be perform several time until reaching a fixpoint
    where [has_changed] remains false *)
let rec lift_until ~statics (e:e) =
  has_changed := false;
  let e' = lifting ~statics SMap.empty e in
  if !has_changed then lift_until ~statics e' else e'


(** since recursive functions cannot have functions or references as parameters,
    [fix(f,fun (p,p_env) -> e)] is rewritten as 
    [fun (p,p_env) -> let f = fix f (fun p -> e) in f p]
    and function applications are rewritten accordingly.
*)
(*
let wrap_fix_in_fun e =
  let rec aux env e = match e with
  | E_fix(f,((P_tuple[p;penv] as pp),e1)) ->
        (match penv with
        | P_unit -> e
        | _ ->
            E_fun(pp,
              E_letIn(P_var f,
                 E_fix(f,(p,aux (f::env) e1)), 
                 E_app(E_var f,pat2exp p))))
  | E_app(E_var g,E_tuple[e1;e2]) when List.mem g env ->
       E_app(E_var g,e1)
    | e -> Ast_mapper.map (aux env) e
  in aux [] e
*)


(** [globalize e] globalizes all local *close* functions in expression [e] *)
let rec globalize_e ?(wrap_fix=true) (e:e) : ((x * e) list * e) =
    let open Ast_mapper in 
    accum (fun glob e ->
    match e with
    | E_letIn(P_var f,(E_fix _ | E_fun _ as v),e2) ->
        let dsv,v = glob v in
        let ds,e2' = glob e2 in
        Some ((dsv@[(f,v)]@ds),e2') 
    (* | E_fix(f,(p,e1)) ->
        let ds1,e1' = glob e1 in
        let v = E_fix(f,(p,e1')) in
        let v' = if (* !Typing.accept_ref_arg_flag || *) wrap_fix 
                 then wrap_fix_in_fun v else v in
        Some (ds1,v')*)
    | E_reg((p,e1),e0,l) ->
        let ds1,e1' = glob e1 in
        let ds0,e0' = glob e0 in
        Some (ds0,E_reg((p,declare ds1 e1'),e0',l))
    | E_exec(e1,e0,l) ->
        let ds1,e1' = glob e1 in
        let ds0,e0' = glob e0 in
        Some (ds0,E_exec(declare ds1 e1',e0',l))
    | E_absLabel (l, e1) ->
        let ds1,e1' = glob e1 in
        Some ([],E_absLabel (l, declare ds1 e1'))   (* scope is ok ? *)
    | E_appLabel (e1,l,lc) ->
        let ds1,e1' = glob e1 in
        Some ([],E_appLabel (declare ds1 e1',l,lc))   (* scope is ok ? *)
    | E_for(x,e_st1,e_st2,e3,loc) ->
        let ds1,e_st1' = glob e_st1 in
        let ds2,e_st2' = glob e_st2 in
        let ds3,e3' = glob e3 in
        Some ([],E_for(x,declare ds1 e_st1',
                       declare ds2 e_st2',
                       declare ds3 e3',loc))
    | _ -> None
  ) e

(** [lambda_lifting ~statics e] lambda-lifts expression [e],
    considering [~statics] as toplevel definitions
    that should not be added to lexical environments. *)
let lambda_lifting ~statics ~globalize (e:e) : ((x * e) list * e) =
    let e_lifted = (lift_until ~statics e) in
    (* Format.(fprintf std_formatter "---> %a\n" Ast_pprint.pp_exp e_lifted); *)
    if globalize then globalize_e (rename_fun_e e_lifted) else ([],e_lifted)


(** [lambda_lifting_pi ~globalize:true pi] lambda-lifts program [pi]. *)
let lambda_lifting_pi ?(globalize=true) (pi:pi) : pi =
  let statics = smap_of_list pi.statics in
  (* let ds_ref,e = Globalize_ref.globalize_ref_e pi.main in *)
  let (ds,e) = lambda_lifting ~statics ~globalize pi.main in
  let main = Ast_mapper.declare [] @@ Ast_mapper.declare ds e in
  {pi with main}
