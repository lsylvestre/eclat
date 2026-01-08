open Ast
open Types

let add_annot e =
  let rec mapper e =
    match e with
    | E_app(E_var f,e1) ->
        let annot_f arg = 
          E_app(E_app(E_const(Op(TyConstr (Types.new_ty_unknown()))),E_var f),arg)
        in
        (* assume [e1] does not contain function calls *)
        annot_f e1
    | e -> Ast_mapper.map mapper e
  in
  mapper e

let h = Hashtbl.create 10

let collect e =
  let rec trt e =
    match e with
    | E_app(E_app(E_const(Op(TyConstr ty)), E_var f),_) ->
        let open Typing in
        let ty' = copy_ty ty in
        (match Hashtbl.find_opt h f with
        | None -> Hashtbl.add h f (`Ty ty')
        | Some `Mono -> ()
        | Some (`Ty ty0) -> 
            (try unify_ty ~loc:Prelude.dloc ty0 ty' with
             | CannotUnify_tyB _
             | CannotUnify_ty _
             | CannotUnify_dur _
             | CannotUnify_size _ -> Hashtbl.replace h f `Mono))
    | e -> Ast_mapper.iter trt e
  in
  trt e

let monomorphize_exp e =
  let rec aux ds e =
    match e with
    | E_letIn(P_var f,ty,E_fix(g,(p,sigty,e1)),e2) ->
        assert (f = g);
        (match Hashtbl.find_opt h f with
        | None -> 
            E_letIn(P_var f,ty,E_fix(g,(p,sigty,aux ds e1)),aux ds e2)
        | Some `Mono ->
            (* must be monomorphized, i.e., duplicated *)
            aux (SMap.add f (p,e1) ds) e2
        | Some (`Ty ty0) ->
            (* is monomorphic: can be shared *)
            let sigty' = extract_arg_res_fun ty0 in
            E_letIn(P_var f,ty0,E_fix(f,(p,sigty',aux ds e1)),aux ds e2))
    | E_app(E_app(E_const(Op(TyConstr ty')), E_var f),arg) ->
       (match Hashtbl.find_opt h f with
        | None -> 
            assert false (* should not happend *)
        | Some `Mono ->
            (match SMap.find_opt f ds with
            | None -> assert false
            | Some (p,e1) ->
            let sigty' = extract_arg_res_fun ty' in
            E_letIn(P_var f,ty',E_fix(f,(p,sigty',aux ds e1)),E_app(E_var f,arg)))
        | Some (`Ty ty0) -> 
            match ty0 with
            | Ty_fun(Ty_base tyB,_,_) -> E_app(E_var f,arg)
            | _ -> E_app(E_var f,arg))
    | e -> Ast_mapper.map (aux ds) e
  in 
  aux SMap.empty e

(* to monorphize the program body (pi.main),
   1/ we insert a type annotation [(f:ty) arg] 
      at each function call [f arg], with [ty] a fresh type unknown
   2/ we retype the program with these annotations
   3/ we try to unify the type annotations [ty] 
      for different calls [(f:ty) arg] to a same function [f].
      This determines if we must monomorphize [f] (denoted by `Mono)
      or not 
   4/ if a function [f] must be monomorphized, it definition is removed
      and moved/duplicated just before each call to [f].
*)
let monomorphize pi =
  Hashtbl.clear h;
  let pi = Ast_rename.rename_pi pi in
  let pi = Rename_fix.rename_pi pi in
  if !Typing.monomorphic then pi else
  let pi = {pi with main = add_annot pi.main} in
  let _ = Typing.typing_with_argument pi [] in
  collect pi.main;
  let e = monomorphize_exp pi.main in
  { pi with main = e }
