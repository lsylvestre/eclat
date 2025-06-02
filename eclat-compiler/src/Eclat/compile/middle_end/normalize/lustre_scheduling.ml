(* reuse code from Mini-LUSTRE (https://www.di.ens.fr/~pouzet/cours/mpri/) *)

open Ast

exception Causality

module S = Set.Make(String)

let s_diff s1 s2 =
  S.diff s1 s2

module Graph = Set.Make(
  struct
    type t = string * S.t * (p * le)
    let compare (x1,s1,_) (x2,s2,_) =
      let c = compare x1 x2 in
      if c<>0 then c else compare s1 s2
  end)


(** [add_vars_of_patt s patt] ajoute à l'ensemble [s] les variables
    introduites par le motif [patt]. *)
let add_vars_of_patt s p =
  SMap.fold (fun x _ s -> S.add x s) (Free_vars.fv (Pattern.pat2exp p)) S.empty


(** [add_vars_of_exp s exp] ajoute à l'ensemble [s] les variables
    dont l'expression [exp] dépend instantanément. *)
let rec add_vars_of_exp s le =
  match le with
  | Exp e -> SMap.fold (fun x _ s -> S.add x s) (Free_vars.fv e) S.empty 
  | Fby(le1,le2) -> add_vars_of_exp s le1
  | When(le1,e2) -> S.union (SMap.fold (fun x _ s -> S.add x s) (Free_vars.fv e2) S.empty) (add_vars_of_exp s le1)
  | Merge(le1,le2,e3) -> S.union (add_vars_of_exp s le1) @@
                                 (add_vars_of_exp (SMap.fold (fun x _ s -> S.add x s) (Free_vars.fv e3) S.empty) le2)
                               
let schedule_equs inputs equs =
  (* Construction du graphe de dépendance entre les variables. *)
  let g =
    List.fold_left
      (fun g (p,le) ->
	 let vp = add_vars_of_patt S.empty p in
	 let ve = add_vars_of_exp S.empty le in
	 S.fold (fun x g -> Graph.add (x,ve,(p,le)) g) vp g)
      Graph.empty equs
  in

  (* Suppression des dépendances aux entrées. *)
  let g =
    Graph.fold
      (fun (y,s,e) g -> Graph.add (y,s_diff s inputs,e) g)
      g
      Graph.empty
  in
  (* Tri topologique des equations *)
  let rec exists_loop topo g =
    if Graph.is_empty g then List.rev topo
    else
      let g1 , g2 = Graph.partition (fun (_,s,_) -> S.is_empty s) g in
      if Graph.is_empty g1 then raise Causality;
      let sv =
        Graph.fold (fun (x,_,_) s -> S.add x s) g1 S.empty
      in
      let g =
	Graph.fold
          (fun (y,s,e) g -> Graph.add (y,s_diff s sv,e) g)
          g2 Graph.empty
      in
      let topo =
	Graph.fold
          (fun (_,_,e) l -> if List.mem e l then l else e::l)
          g1 topo
      in
      exists_loop topo g
  in
  exists_loop [] g


let rec schedule_e e =
  match e with
  | E_equations(p,eqs) ->
      let locals = List.fold_left (fun acc (p,_) -> acc ++ (vars_of_p p)) SMap.empty eqs in
      let inputs = SMap.filter (fun x _ -> not (SMap.mem x locals)) (Free_vars.fv e) in
      E_equations(p,schedule_equs (SMap.fold (fun x _ s -> S.add x s) inputs S.empty) eqs)
  | _ -> Ast_mapper.map schedule_e e

let schedule_pi pi =
  {pi with main = schedule_e pi.main }
