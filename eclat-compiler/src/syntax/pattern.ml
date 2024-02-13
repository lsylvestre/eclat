open Ast

(* [pat_mem x p] returns true iff [x] is bound in [p] *)
let pat_mem (x:x) (p:p) : bool =
  let exception Found in
  (** [aux p] raise [Found] when [x] is found in [p] *)
  let rec aux = function
    | P_unit -> ()
    | P_var(y) -> if x = y then raise Found
    | P_tuple(ps) -> List.iter aux ps
  in
  try aux p; false with Found -> true

(* [vars_of_p p] returns the (free) variables used in the pattern [p] *)
let rec vars_of_p (p:p) : unit smap =
  match p with
  | P_unit -> SMap.empty
  | P_var x -> SMap.singleton x ()
  | P_tuple ps ->
    List.fold_left (fun m p -> vars_of_p p ++ m) SMap.empty ps

(* static expressions, i.e., a constant, a variable,
   or the static projection of a tuple expression *)
let rec static (e:e) : bool =
  let static_aux e =
    match e with
    | E_app (E_const (Op (GetTuple _)),e) -> static e
    | _ -> true
  in
  is_variable e || evaluated e || static_aux e

exception CannotMatch of p * e

(* [bindings p e] matches expression [e] with pattern [p]
   and returns the resulting bindings (variable x value).
   Raises [CannotMatch(p,e)] if [e] cannot be matched with [p]. *)
let rec bindings (p:p) (e:e) : e smap =
  match p,e with
  | P_unit,E_const Unit ->
      SMap.empty
  | P_unit,_ ->
      SMap.singleton (gensym ()) e
  | P_var x,e ->
      SMap.singleton x e
  | P_tuple ps,E_tuple es ->
    if (List.compare_lengths ps es <> 0) then raise (CannotMatch (p,e)) else
    List.fold_left2 (fun m p v -> bindings p v ++ m) SMap.empty ps es
  | P_tuple ps,e when static e ->
      let n = List.length ps in
      let rs = List.mapi (fun i p ->
                  bindings p (E_app (E_const (Op (GetTuple {pos=i;arity=n})),e))
                ) ps
      in
      List.fold_right (++) rs  SMap.empty
  | _ ->
     raise (CannotMatch (p,e))

(** [pat2exp p] converts the pattern [p] into an expression *)
let rec pat2exp (p:p) : e =
  match p with
  | P_unit ->
      E_const Unit
  | P_var x ->
      E_var x
  | P_tuple ps ->
      E_tuple (List.map pat2exp ps)

exception Not_a_pattern

(** [exp2pat e] converts the expression [e] into a pattern.
    Raises [Not_a_pattern] if the expression [e] cannot
    be converted into a pattern *)
let rec exp2pat (e:e) : p =
  match e with
  | E_const Unit ->
      P_unit
  | E_var x ->
      P_var x
  | E_tuple ps ->
      P_tuple (List.map exp2pat ps)
  | _ -> raise Not_a_pattern
