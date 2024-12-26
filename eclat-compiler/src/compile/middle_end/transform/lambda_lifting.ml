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

let declare ds e =
  List.fold_right (fun (x,t,v) e -> E_letIn(P_var x,t,v,e)) ds e

let rec rename_fun_e = function
  | E_letIn(P_var f,ty,((E_fun _ | E_fix _) as phi),e2) ->
      let g = gensym ~prefix:f () in
      E_letIn(P_var g,ty,rename_fun_e @@ phi,rename_fun_e @@ Ast_subst.subst_e f (E_var g) e2)
  | e -> Ast_mapper.map rename_fun_e e


let version2 = ref false 

(** [lifting ~statics ~decls e] lifts expression [e]
    considering [~statics] and [~decls] as toplevel definitions
    that should not be added to lexical environments. *)
let lifting ~statics (env:env) (e:e) : e =
  let env_filter env p = env (*  (* NB: il faut bien renommer avant, mais ne pas filtrer à la volée car on perd la trace des fonctions placées dans des tuples qui sont déconstruits ensuite *)
    let xs = vars_of_p p in 
    SMap.filter (fun x _ -> not @@ SMap.mem x xs) env*)
   in
  let rec lift env e =
    let open Ast in
    match e with
     | E_var f ->
         if !version2 then 
        (match SMap.find_opt f env with
         | None -> E_var f
         | Some p -> 
            let x =gensym () in 
            E_fun(P_var x,(Types.new_ty_unknown(),Types.new_tyB_unknown()),E_app(E_var f,E_tuple[E_var x;pat2exp p])))
            (* E_letIn(P_var g,E_fun(P_var x,E_app(E_var f,E_tuple[E_var x;pat2exp p])), E_var g)) *)
      else e
    (*| E_app((E_const (Op(TyConstr _))),_) ->
        assert false*)
    | E_app(e1,e2) ->
        let _ty,e1' = Ast_undecorated.remove_tyconstr e1 in
        (* todo: deal with ty or emit warning *)
        (match e1' with
        | E_const _ -> E_app(e1,lift env e2)
        | E_var f -> 
            let e2' = lift env e2 in
            (match SMap.find_opt f env with
             | None | Some (P_unit) -> E_app(e1,e2')
             | Some p -> (* Printf.printf "=====>%d\n" (SMap.cardinal  (vars_of_p p)); *)
                 E_app(e1,E_tuple[e2'; pat2exp p]))
        | _ ->
         (* assert (evaluated e1);*) lift env @@
         let f = gensym () in
         E_letIn(P_var f,Types.new_ty_unknown(),e1,E_app(E_var f,e2)))
    | E_letIn(px,ty,ex,e2) ->
        let _ty,ex' = ty,(* Ast_undecorated.remove_tyconstr*) ex in
        (* todo: deal with ty or emit warning *)
        (match px,ex' with
        | P_var f,(E_fun(p,(ty1,tyB2),e1) as phi) ->
            let e1' = lift env e1 in
            let xs = fv ~statics phi in
            let vp = (vars_of_p p) in
            let vp = xs |> SMap.filter (fun x _ ->
                                 not (SMap.mem x vp) && not (SMap.mem x env)) in
            let p_env' = vp |> SMap.bindings
                            |> List.map (fun (x,_) -> (* Printf.printf "--->%s\n" x;*) P_var x)
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
               let env2, ef = (SMap.add f p_env' env, E_fun(P_tuple[p;p_env'],(Ty_tuple[ty1;Types.new_ty_unknown ()],tyB2),e1')) in
               E_letIn(P_var f,Types.new_ty_unknown (),ef,lift env2 e2) )
            else
               let env2 = SMap.add f p_env' env in
               E_letIn(P_var f,ty,(E_fun(p,(ty1,tyB2),e1')),lift env2 e2)
     
        | P_var f,(E_fix(g,(p,(ty1,tyB2),e1)) as phi) ->
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
              let ef = E_fix(g,(P_tuple[p;p_env'],(Ty_tuple[ty1;Types.new_ty_unknown ()],tyB2),e1')) in
              E_letIn(P_var f,Types.new_ty_unknown (),ef,e2'))
             else
              (E_letIn(P_var f,ty,(E_fix(g,(p,(ty1,tyB2),e1'))),e2') )
          | _ ->
              let env' = env_filter env px in  (* todo: idem with E_match to avoid capture *)
              E_letIn(px,ty,lift env ex,lift env' e2))
    | E_match(e1,hs,eo) ->
        let e1' = lift env e1 in
        let hs' = List.map (fun (inj,(p,ei)) ->
                     let env' = env_filter env p in
                     (inj,(p,lift env' ei))) hs in
      E_match(e1',hs',Option.map (lift env) eo)

    | E_reg((p,tyB,e1),e0,l) ->
        let env' = env_filter env p in
        E_reg((p,tyB,lift env' e1),lift env e0,l)
    | E_for(x,lc1,lc2,e1,loc) ->
        let env' = env_filter env (P_var x) in
        E_for(x,lc1,lc2,lift env' e1,loc)
    | E_vector_mapi(is_par,(p,tyB,e1),e2,ty) ->
        let env' = env_filter env p in
        E_vector_mapi(is_par,(p,tyB,lift env' e1),lift env e2,ty)
    | e -> Ast_mapper.map (lift env) e
  in lift env e

(** lifting has to be perform several time until reaching a fixpoint
    where [has_changed] remains false *)
let rec lift_until ~statics (e:e) =
  has_changed := false;
  let e' = lifting ~statics SMap.empty e in
  if !has_changed then lift_until ~statics e' else e'



(** [globalize e] globalizes all local *close* functions in expression [e] *)
let globalize_e (e:e) : ((x * _ * e) list * e) =
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
    | E_letIn(P_var f,ty,(E_fix _ | E_fun _ as v),e2) ->
        let dsv,v = glob v in
        let ds,e2' = glob e2 in
        (dsv@[(f,ty,v)]@ds),e2'
    | E_fix(f,(p,tysig,e1)) ->
        let ds1,e1' = glob e1 in
        let xs = vars_of_p p in
        if SMap.filter (fun x _ -> not @@ SMap.mem x xs) (Free_vars.fv_arrays e1) <> SMap.empty 
        then 
          ds1,E_fix(f,(p,tysig,e1'))
        else
        [],E_fix(f,(p,tysig,declare ds1 e1'))
    | E_fun(p,tysig,e1) ->
        let ds1,e1' = glob e1 in
        ds1,E_fun(p,tysig,e1')
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
    (* | E_letIn(P_var x,(E_array_create (e3,_) as e1),e2) ->
        (match e3 with
        | E_const _ ->
            let ds1,e1' = glob e1 in
            let ds2,e2' = glob e2 in
            ds1@[x,e1']@ds2,e2'
        | _ -> 
            let ds1,e1' = glob e1 in
            let ds2,e2' = glob e2 in
            ds1,declare ((x,e1')::ds2) e2')*)
    (* | E_letIn(P_var x,e1,e2) ->
        let ds1,e1' = glob e1 in
        let ds2,e2' = glob e2 in
        ds1@[x,e1']@ds2,e2'
    | E_letIn(P_tuple ps,E_tuple es,e2) ->
        glob (List.fold_right2 (fun p e acc -> E_letIn(p,e,acc)) ps es e2) *)
    | E_letIn(p,ty,e1,e2) ->
        let ds1,e1' = glob e1 in
        let ds2,e2' = glob e2 in
        ds1@ds2,E_letIn(p,ty,e1',e2')
    | E_ref(e1) ->
        let ds1,e1' = glob e1 in
        ds1,E_ref(e1')
    | E_get(e1) ->
        let ds1,e1' = glob e1 in
        ds1,E_get(e1')
    | E_set(e1,e2) ->
        let ds1,e1' = glob e1 in
        let ds2,e2' = glob e2 in
        ds1@ds2,E_set(e1',e2)
    | E_array_make(sz,e1,loc) ->
        let ds1,e1' = glob e1 in
        ds1,E_array_make(sz,e1',loc)
    | E_array_create _ ->
        [],e
   | E_array_length _ ->
        [],e
    | E_array_get(x,e1) ->
        let ds1,e1' = glob e1 in
        ds1,E_array_get(x,e1')
    | E_array_set(x,e1,e2) ->
        let ds1,e1' = glob e1 in
        let ds2,e2' = glob e2 in
        ds1@ds2,E_array_set(x,e1',e2')
    | E_par(es) ->
        let ds,es' = globalize_list es in
        ds,E_par(es')
    | E_reg((p,tyB,e1),e0,l) ->
        let ds1,e1' = glob e1 in
        let ds0,e0' = glob e0 in
        ds0,E_reg((p,tyB,declare ds1 e1'),e0',l)
    | E_exec(e1,e0,eo,l) ->
        let ds1,e1' = glob e1 in
        let ds0,e0' = glob e0 in
        let ds3,eo' = match eo with 
                      | None -> [],eo 
                      | Some e3 -> let ds3,e3' = glob e3 in
                    ds3,Some e3 in
        ds0@ds3,E_exec(declare ds1 e1',e0',eo',l)
    | E_for(x,e_st1,e_st2,e3,loc) ->
        let ds1,e_st1' = glob e_st1 in
        let ds2,e_st2' = glob e_st2 in
        let ds3,e3' = glob e3 in
        [],E_for(x,declare ds1 e_st1',
                   declare ds2 e_st2',
                   declare ds3 e3',loc)
         (* NB: definitions in [e_st1] and [e_st2] and [e3]
            are *not* globalized *)
    | E_generate((p,tysig,e1),e2,e_st3,loc) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      let ds3,e_st3' = glob e_st3 in
      ds2,E_generate((p,tysig,declare ds1 e1'),e2',declare ds3 e_st3',loc)
      (* NB: definitions in [e_st1] are *not* globalized *)

    | E_vector es ->
        let ds,es' = globalize_list es in
        ds,E_vector(es')
    | E_vector_mapi(is_par,(p,tyB,e1),e2,ty) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      ds2,E_vector_mapi(is_par,(p,tyB,declare ds1 e1'),e2',ty)
    | E_run(x,e) ->
      let ds,e' = glob e in
      ds,E_run(x,e')
    | E_pause e ->
      let ds,e' = glob e in
      ds,E_pause e'
  in glob e



(** [lambda_lifting ~statics e] lambda-lifts expression [e],
    considering [~statics] as toplevel definitions
    that should not be added to lexical environments. *)
let lambda_lifting ~statics ~globalize (e:e) : ((x * _ * e) list * e) =
    let e_lifted = (lift_until ~statics e) in
    if globalize then globalize_e (rename_fun_e e_lifted) else ([],e_lifted)


(** [lambda_lifting_pi ~globalize:true pi] lambda-lifts program [pi]. *)
let lambda_lifting_pi ?(globalize=true) (pi:pi) : pi =
  let statics = smap_of_list pi.statics in
  let (ds,e) = lambda_lifting ~statics ~globalize pi.main in
  let main = declare ds e in
  {pi with main}
