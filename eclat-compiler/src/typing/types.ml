type x = string


(** associative array having key of type [string] *)
module SMap = Map.Make(String)

type ty =                (** type *)
  | T_const of tconst    (** type constant *)
  | T_var of tvar ref    (** type variable *)
  | T_tuple of ty list   (** type constructor for tuples *)
  | T_fun of {
      arg:ty ;  (** functional type constructor annotated with a response time [dur] *)
      dur:ty ;  (* [arg -(dur)-> ret] is the type of a function which, given a value of *)
      ret:ty    (* type [arg], produces a value of type [ret] after no more than [dur] clock ticks *)
    }
  | T_sum of (x * ty) list
  | T_string of ty (** string parameterized by its size using a the size type [ty] *)
  | T_array of {
      elem : ty ; (** static array of elements of type [elem], *)
      size : ty   (** parameterized by its size using a the size type [size] *)
    }
  | T_static of ty
  (* sized types for check response time and static datastructures *)
  | T_size of int      (** n *)
  | T_infinity         (** plus infinite *)
  | T_add of ty * ty   (** t + t'  *)
  | T_max of ty * ty   (** max(t,t') *)
  | T_le of ty * ty    (** t such that t <= t' *)
  | T_forall of x * ty * ty

and tconst = (** type constant *)
  | TBool      (** boolean type [bool] *)
  | TInt of ty (** integer type [int<ty>]
                 where [ty] is a size type denoting the size of the interger.
                 All integers are signed. *)
  | TUnit      (** unit type [unit] *)

and tvar =              (** type variable *)
  | Unknown of int      (** type unknown 'a identified by a unique integer *)
  | Ty of ty            (* instantiated type variable *)



let tint tz = T_const (TInt tz)
let tbool = T_const TBool
let tunit = T_const TUnit
let fun_ty t1 n t2 =
  T_fun { arg = t1;
          dur = n;
          ret = t2 }

(* instantaneous function type *)
let (==>) t1 t2 =
  fun_ty t1 (T_size 0) t2

let unknown =
  let c = ref 0 in
  fun () ->
    let ty = T_var (ref (Unknown (!c))) in
    incr c; ty


let simplify_size_constraints t =
  let rec simpl t = match t with
  | T_size _ | T_infinity -> t
  | T_add(t1,t2) ->
      (match simpl t1, simpl t2 with
      | T_add(ta,tb),t -> simpl @@ T_add(ta,T_add(tb,t))
      | T_size n,T_add(T_size m,tb) -> simpl @@ T_add(T_size (n+m),tb)
      | T_var _ as t1',t2'
      | t2',(T_var _ as t1') -> T_add(t2',t1')
      | T_size n,T_size m -> T_size (n+m)
      | T_infinity,T_size _
      | T_infinity,T_infinity
      | T_size _ , T_infinity -> T_infinity
      | t,T_size 0 | T_size 0,t -> t
      | t1',t2' -> T_add(t1',t2'))
  | T_max(t1,t2) ->
      (match simpl t1, simpl t2 with
      | T_size n,T_size m -> T_size (max n m)
      | T_infinity,T_size _
      | T_infinity,T_infinity
      | T_size _ , T_infinity -> T_infinity
      | t,T_size 0 | T_size 0,t -> t
      | t1',t2' -> T_max(t1',t2'))
  | T_le _ -> failwith "todo T_le"
  | T_var({contents=Ty t'} as v) ->
      let t2 = simpl t' in
      v := Ty t2; t2
  | T_var{contents=Unknown _} ->
      t
  | _ -> assert false (* hill kinded *)
  in simpl t

(** [canon t] put the type [t] in canonical form by replacing
  instantiated variables free in [t] by their definition,
  themselves put in canonical form. *)
let rec canon t =
  match t with
  | T_const(TInt t) -> T_const(TInt (canon t))
  | T_const _ -> t
  | T_var({contents=Ty t'} as v) ->
      let t2 = canon t' in
      v := Ty t2; t2
  | T_var{contents=Unknown _} ->
      t
  | T_tuple (ts) ->
      T_tuple (List.map canon ts)
  | T_fun{arg;dur;ret} ->
      T_fun{ arg = canon arg;
             dur = canon dur;
             ret = canon ret }
  | T_string tz -> T_string (canon tz)
  | T_sum cs -> T_sum (List.map (fun (x,t) -> (x,canon t)) cs)
  | T_array {elem=t;size=tz} -> T_array {elem=canon t;size=canon tz}
  | T_static t -> T_static (canon t)
  | T_forall(x,t1,t2) -> T_forall(x,canon t1,canon t2)
  | (T_size _ | T_infinity | T_add _ | T_max _ | T_le _) as t -> simplify_size_constraints t


let find_ctor x sums =
  (* find number and type of ctor [x] in the sum type definition [sum] *)
  let find_ctor_t x sum =
    let rec aux n = function
    | [] -> None
    | (y,t)::sum' -> if x = y then Some (n,t) else aux (n+1) sum' in aux 0 sum
  in
  let rec aux = function
    | [] -> raise Not_found (* bad typed *)
    | (_,sum)::sums' -> (match find_ctor_t x sum with
                         | Some (n,t) -> (n,sum, t)
                         | None -> aux sums') in
  aux sums
