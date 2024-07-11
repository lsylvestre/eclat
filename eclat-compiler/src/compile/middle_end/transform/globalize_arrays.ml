open Ast
open Ast_subst


(** [globalize e] globalizes all local *close* functions in expression [e] *)
(* all array_create must be bound by unique names *)
let rec globalize_arrays_e (e:e) : ((x * e) list * e) =
  Ast_mapper.accum (fun glob e ->
    match e with
    | E_letIn(P_var x,(E_local_static_array _ as ex),e1) ->
        let ds1,e1' = glob e1 in
        Some ((x,ex)::ds1,e1')
    | _ -> None
  ) e

let declare ds e =
  List.fold_right (fun (x,v) e -> E_letIn(P_var x,v,e)) ds e


let globalize_arrays pi =
  let ds,e = globalize_arrays_e pi.main in
  let rec loop acc = function
  | [] -> List.rev acc
  |d::ds -> let d' = 
             (match d with 
             | (x,E_local_static_array(e1,loc)) ->
                   (match un_deco e1 with
                    | E_const (Int(n,_)) -> 
                        x,Static_array_of(Ty_array(Types.Sz_lit n,Types.new_tyB_unknown()),Prelude.dloc)
                    | E_array_length(y) ->
                        let n = (match List.assoc_opt y acc with
                        | Some(Static_array_of(t,_)) -> 
                            (match Types.canon_ty t with Ty_array(Types.Sz_lit n,_) -> n
                            | _ -> assert false)
                        | Some(Static_array(_,n)) -> n 
                        | _ -> assert false
                        ) in
                        x,Static_array_of(Ty_array(Types.Sz_lit n,Types.new_tyB_unknown()),Prelude.dloc)
                    | _ -> Prelude.Errors.error ~loc (fun fmt ->
                      Format.fprintf fmt "the size of local array %a cannot be resolved." Ast_pprint.pp_exp e1))
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
 
