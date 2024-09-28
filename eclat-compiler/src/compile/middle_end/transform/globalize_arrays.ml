open Ast
open Ast_subst

open Types

(** [globalize e] globalizes all local *close* functions in expression [e] *)
(* all array_create must be bound by unique names *)
let rec globalize_arrays_e (e:e) : ((x * (ty * e)) list * e) =
  Ast_mapper.accum (fun glob e ->
    match e with
    | E_letIn(P_var x,ty,(E_array_create _ as ex),e1) ->
        let ds1,e1' = glob e1 in
        Some ((x,(ty,ex))::ds1,e1')
    | E_letIn(P_var x,ty,(E_array_make(sz,e0,loc) as ex),e1) ->
        let sz_lit = match Types.canon_size sz with
                     | Sz_lit v -> v
                     | _ -> assert false (* todo *) in
        let n = E_const (Int (sz_lit,Sz_lit 16)) in
        let ds1,e1' = glob e1 in
        let y = Ast.gensym ~prefix:"y" () in
        let loop = Ast.gensym ~prefix:"loop" () in
        let i = Ast.gensym ~prefix:"i" () in
        let e2 =
           E_letIn(P_var y, Types.new_ty_unknown(), e0,
           E_letIn(P_var loop, Types.new_ty_unknown(), 
            E_fix(loop,(P_var i,(Types.new_ty_unknown(), Types.new_tyB_unknown()),
              E_if(E_app(E_const(Op(Runtime(External_fun("Int.ge",Types.new_ty_unknown ())))),E_tuple[E_var i;n]),
                   e1',
                   E_letIn(P_unit,Ty_base TyB_unit,E_array_set(x,E_var i, E_var y),E_app(E_var loop,
                    E_app(E_const(Op(Runtime(External_fun("Int.add",Types.new_ty_unknown ())))),
                        E_tuple[E_var i;E_const (Int (1,Types.new_size_unknown()))])))))), 
                 E_app(E_var loop,E_const(Int(0,Sz_lit 16))))) in
        Some ((x,(ty,ex))::ds1,e2)

       
    | _ -> None
  ) e

let declare ds e =
  List.fold_right (fun (x,ty,v) e -> E_letIn(P_var x,ty,v,e)) ds e


let globalize_arrays pi =
  let ds,e = globalize_arrays_e pi.main in
  let rec loop acc = function
  | [] -> List.rev acc
  |d::ds -> let d' = 
             (match d with 
             | (x,(ty,E_array_create(sz,loc))) ->
                  x,Static_array_of(ty,loc)
             | (x,(ty,E_array_make(sz,e1,loc))) ->
                  x,Static_array_of(ty,loc)
                 (*  let c = try e2c e1 with Not_a_constant ->
                    Prelude.Errors.error ~loc (fun fmt ->
                      Format.fprintf fmt "the default element %a of this local array cannot be resolved." Ast_pprint.pp_exp e1) in
                  (match un_deco e2 with
                  | E_const (Int(n,_)) -> x,Static_array(c,n)
                  | E_array_length y -> 
                      let n = match List.assoc_opt y acc with (* all names in [acc] are different *)
                              | Some (Static_array(_,n)) -> n
                              | _ -> assert false (* compilation error *)
                      in
                      x,Static_array(c,n)*)
             | _ -> assert false) in
            loop (d'::acc) ds
  in
  { pi with main=e ; statics=loop (List.rev pi.statics) ds }
 
