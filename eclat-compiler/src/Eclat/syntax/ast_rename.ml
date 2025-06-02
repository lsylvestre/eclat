open Ast
open Ast_subst
open Pattern

(* rename all variables that are not statics *)

(* custom symbol generator *)
module Gensym : sig val gensym : statics:x list -> x -> x end = struct

  let of_int (n:int) : x =
    "$"^string_of_int n

  let make_name (n:int) (x:x) : x =
    let x = if x = "_" then "w" else x in
    of_int n^"_"^x

  let c = ref 0

  let rename x =
    incr c;
    if String.length x > 1 && x.[0] = '$' then
      (match String.index_from_opt x 1 '_' with
      | Some i -> make_name !c (String.sub x (i+1) (String.length x - i-1))
      | None -> of_int !c)
    else make_name !c x

  let gensym ~statics x =
    if List.mem x statics then x else
    rename x 

end

open Gensym

let rename_ident ~statics x =
  gensym ~statics x

(** [rename_pat p] rename all names in the pattern [p],
  assuming that any variable is bound several times in p *)
let rec rename_pat ~statics p =
   match p with
   | P_unit -> P_unit
   | P_var x -> P_var (rename_ident ~statics x)
   | P_tuple ps -> P_tuple (List.map (rename_pat ~statics) ps)

let rename_e ~statics e =
  let rec ren_e = function
  | E_fix(f, (p, ty, e1)) ->
      let g = rename_ident ~statics f in
      let pz = rename_pat ~statics p in
      let e1_ren = subst_e f (E_var g) @@
                   subst_p_e p (pat2exp pz) e1 in
      E_fix (g,(pz, ty, ren_e e1_ren))
  | E_fun(p, ty, e1) ->
      let pz = rename_pat ~statics p in
      E_fun (pz, ty, ren_e (subst_p_e p (pat2exp pz) e1))
  | E_letIn(p,ty, e1,e2) ->
      let pz = rename_pat ~statics p in
      let ez = pat2exp pz in
      E_letIn(pz, ty, ren_e e1, ren_e @@ subst_p_e p ez e2)
  | E_match(e1,hs,eo) ->
      let hs' = List.map (fun (inj,(p,ei)) ->
                            let pz = rename_pat ~statics p in
                            let ei' = ren_e (subst_p_e p (pat2exp pz) ei) in
                            (inj,(pz,ei'))) hs in
      E_match(ren_e e1, hs',Option.map ren_e eo)
  | E_reg((p, tyB, e1),e0,l) ->
      let pz = rename_pat ~statics p in
      let e1' = ren_e (subst_p_e p (pat2exp pz) e1) in
     E_reg((pz, tyB, e1'), ren_e e0, l)
  | E_generate((p, ty, e1), e2, e3, l) ->
      let pz = rename_pat ~statics p in
      let e1' = ren_e (subst_p_e p (pat2exp pz) e1) in
     E_generate((pz, ty, e1'), ren_e e2, ren_e e3,l)
  | E_for(x,lc,lc',e,loc) ->
      let x' = rename_ident ~statics x in
      E_for(x',lc,lc',ren_e @@ subst_e x (E_var x') e,loc) (* rename lc & lc' ? *)
  | E_vector_mapi(is_par,(p, typ, e1),e2,ty) ->
      let pz = rename_pat ~statics p in
      let e1' = ren_e (subst_p_e p (pat2exp pz) e1) in
     E_vector_mapi(is_par,(pz, typ, e1'),ren_e e2,ty)
  | E_equations(p,eqs) ->
      let rec ren_p_le p ex le =
        let ren e =
          ren_e (subst_p_e p ex e)
        in
        match le with
        | Exp e' -> Exp(ren e')
        | Fby(le1, le2) -> Fby(ren_p_le p ex le1, ren_p_le p ex le2)
        | When(le1, e2) -> When(ren_p_le p ex le1, ren e2)
        | Merge(le1, le2, e3) -> Merge(ren_p_le p ex le1, ren_p_le p ex le2, ren e3)
      in
      let p_tuple = P_tuple (p :: List.map (fun (p,_) -> p) eqs) in 
      let pz = rename_pat ~statics p_tuple in
      let eqs' = List.map (fun (pj,lei) -> 
                  let pj' = exp2pat (subst_p_e p_tuple (pat2exp pz) (pat2exp pj)) in
                  let lei' = ren_p_le p_tuple (pat2exp pz) lei in
                  pj', lei') eqs in
      let p' = exp2pat (subst_p_e p_tuple (pat2exp pz) (pat2exp p)) in
      E_equations(p',eqs')
  | e -> Ast_mapper.map ren_e e
  in
  ren_e e

let rename_pi pi =
  let statics = List.map fst pi.statics in
  let main = rename_e ~statics pi.main in
  { pi with main }

