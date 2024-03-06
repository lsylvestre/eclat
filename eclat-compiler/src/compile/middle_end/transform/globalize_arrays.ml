open Ast
open Ast_subst

(** [globalize e] globalizes all local *close* functions in expression [e] *)
let rec globalize_arrays_e (e:e) : ((x * e) list * e) =
  Ast_mapper.accum (fun glob e ->
    match e with
    | E_letIn(P_var x,(E_local_static_array _ as ex),e1) ->
        let x' = gensym ~prefix:x () in
        let ds1,e1' = glob e1 in
        Some ((x',ex)::ds1,subst_e x (E_var x') e1')
    | E_letIn(P_var x,(E_local_static_matrix _ as ex),e1) ->
        let x' = gensym ~prefix:x () in
        let ds1,e1' = glob e1 in
        Some ((x',ex)::ds1,subst_e x (E_var x') e1')
    | _ -> None
  ) e

let globalize_arrays pi =
  let ds,e = globalize_arrays_e pi.main in
  let rec loop acc = function
  | [] -> List.rev acc
  |d::ds -> let d' = 
             (match d with 
             | (x,E_local_static_array(e1,e2,loc)) ->
                  let c = try e2c e1 with Not_a_constant ->
                    Prelude.Errors.error ~loc (fun fmt ->
                      Format.fprintf fmt "the default element %a of this local array cannot be resolved." Ast_pprint.pp_exp e1) in
                  (match un_deco e2 with
                  | E_const (Int(n,_)) -> x,Static_array(c,n)
                  | E_array_length y -> 
                      let n = match List.assoc_opt y acc with (* all names in [acc] are different *)
                              | Some (Static_array(_,n)) -> n
                              | _ -> assert false (* compilation error *)
                      in
                      x,Static_array(c,n)
                  | _ -> Prelude.Errors.error ~loc (fun fmt ->
                           Format.fprintf fmt "the size %a of this local array cannot be resolved." Ast_pprint.pp_exp e2))
             | (x,E_local_static_matrix(e1,es,loc)) ->
                  let c = try e2c e1 with Not_a_constant ->
                    Prelude.Errors.error ~loc (fun fmt ->
                      Format.fprintf fmt "the default element %a of this local array cannot be resolved." Ast_pprint.pp_exp e1) in
                  let ns = List.map (fun e -> 
                    (match un_deco e with
                  | E_const (Int(n,_)) -> n
                  | E_array_length y -> 
                      let n = match List.assoc_opt y acc with (* all names in [acc] are different *)
                              | Some (Static_array(_,n)) -> n 
                              | _ -> assert false (* compilation error *) in
                      n
                  | _ -> Prelude.Errors.error ~loc (fun fmt ->
                           Format.fprintf fmt "the size %a of this local array cannot be resolved." Ast_pprint.pp_exp e))) es
                  in 
                  x,Static_matrix(c,ns)
             | _ -> assert false) in
            loop (d'::acc) ds
  in
  { pi with main=e ; statics=loop (List.rev pi.statics) ds }