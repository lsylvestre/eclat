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
  | T_ref of ty   (* mutable reference cell *)
  | T_array of {
      elem : ty ; (** static array of elements of type [elem], *)
      size : ty   (** parameterized by its size using a the size type [size] *)
    }
  | T_matrix of {
      elem : ty ; (** static matrix of elements of type [elem], *)
      size : ty   (** parameterized by its size using a the size type [size] as tuple *)
    }
  | T_static of ty
  (* sized types for check response time and static datastructures *)
  | T_size of int      (** n *)
  | T_response_time of int
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
  fun_ty t1 (T_response_time 0) t2

let unknown =
  let c = ref 0 in
  fun () ->
    let ty = T_var (ref (Unknown (!c))) in
    incr c; ty


let simplify_response_time t =
  let rec simpl t = match t with
  | T_size _ -> t
  | T_response_time _ | T_infinity -> t
  | T_add(t1,t2) ->
      (match simpl t1, simpl t2 with
      | T_add(ta,tb),t -> simpl @@ T_add(ta,T_add(tb,t))
      | T_response_time n,T_add(T_response_time m,tb) -> 
          simpl @@ T_add(T_response_time (n+m),tb)
      | T_var _ as t1',t2'
      | t2',(T_var _ as t1') -> T_add(t2',t1')
      | T_response_time n,T_response_time m -> T_response_time (n+m)
      | T_infinity,T_response_time _
      | T_infinity,T_infinity
      | T_response_time _ , T_infinity -> T_infinity
      | t,T_response_time 0 | T_response_time 0,t -> t
      | t1',t2' -> T_add(t1',t2'))
  | T_max(t1,t2) ->
      (match simpl t1, simpl t2 with
      | T_response_time n,T_response_time m -> 
          T_response_time (max n m)
      | T_infinity,T_response_time _
      | T_infinity,T_infinity
      | T_response_time _ , T_infinity -> T_infinity
      | t,T_response_time 0 | T_response_time 0,t -> t
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
  | T_ref(t) -> T_ref (canon t)
  | T_array {elem=t;size=tz} -> T_array {elem=canon t;size=canon tz}
  | T_matrix {elem=t;size=tz} -> T_matrix {elem=canon t;size=canon tz}
  | T_static t -> T_static (canon t)
  | T_forall(x,t1,t2) -> T_forall(x,canon t1,canon t2)
  | T_size _ -> t
  | (T_response_time _ | T_infinity | T_add _ | T_max _ | T_le _) as t -> simplify_response_time t


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



module Ty = struct
  type 'a var = 'a var_content ref
  and 'a var_content = Unknown of int
                     | Is of 'a
  type ty = Ty_var of ty var
          | Ty_base of tyB
          | Ty_tuple of ty list
          | Ty_fun of ty * dur * tyB
          | Ty_ref of tyB
          | Ty_array of size * tyB
          | Ty_matrix of size list * tyB
  and tyB = TyB_var of tyB var
          | TyB_int of size
          | TyB_bool
          | TyB_unit
          | TyB_tuple of tyB list
          | TyB_string of size
  and size = Sz_var of size var
           | Sz_lit of int
  and dur = Dur_var of dur var
          | Dur_zero
          | Dur_one
          | Dur_max of dur * dur

  let rec canon_ty = function
  | Ty_var ({contents=Is ty} as v) ->
      let ty' = canon_ty ty in
      v := Is ty'; ty'
  | Ty_var {contents=Unknown _} as ty -> ty
  | Ty_base tyB -> Ty_base(canon_tyB tyB)
  | Ty_tuple ty_list ->
      (let exception E in
      let ty_list' = List.map canon_ty ty_list in
      try
        Ty_base (TyB_tuple (List.map (function
                                   | Ty_base tyB -> tyB
                                   | _ -> raise E) ty_list))
      with E ->
        Ty_tuple (ty_list'))
  | Ty_fun(ty,dur,tyB) ->
      Ty_fun(canon_ty ty,
             canon_dur dur,
             canon_tyB tyB)
  | Ty_ref(tyB) ->
      Ty_ref(canon_tyB tyB)
  | Ty_array(sz,tyB) ->
      Ty_array(canon_size sz, canon_tyB tyB)
  | Ty_matrix(sz_list,tyB) ->
      Ty_matrix(List.map canon_size sz_list,canon_tyB tyB)
  and canon_tyB = function
  | TyB_var ({contents=Is tyB} as v) ->
      let tyB' = canon_tyB tyB in
      v := Is tyB'; tyB'
  | TyB_var {contents=Unknown _} as tyB -> tyB
  | TyB_int sz -> TyB_int (canon_size sz)
  | TyB_bool -> TyB_bool
  | TyB_unit -> TyB_unit
  | TyB_tuple tyB_list ->
      TyB_tuple (List.map canon_tyB tyB_list)
  | TyB_string sz ->
      TyB_string (canon_size sz)
  and canon_size = function
  | Sz_var ({contents=Is sz} as v) ->
    let sz' = canon_size sz in
    v := Is sz'; sz'
  | Sz_var {contents=Unknown _} as sz -> sz
  | Sz_lit _ as n -> n
  and canon_dur = function
  | Dur_var ({contents=Is d} as v) ->
    let d' = canon_dur d in
    v := Is d'; d'
  | Dur_var {contents=Unknown _} as d -> d
  | Dur_zero -> Dur_zero
  | Dur_one -> Dur_one
  | Dur_max(Dur_max(Dur_zero,d1),d2)
  | Dur_max(Dur_max(d1,Dur_zero),d2)
  | Dur_max(Dur_max(d1,d2),Dur_zero) ->
      canon_dur (Dur_max(d1,d2))
  | Dur_max(Dur_max(Dur_one,d1),d2)
  | Dur_max(Dur_max(d1,Dur_one),d2)
  | Dur_max(Dur_max(d1,d2),Dur_one) ->
      Dur_one
  | Dur_max(Dur_zero,d) | Dur_max(d,Dur_zero) ->
      canon_dur d
  | Dur_max(Dur_one,_)|Dur_max(_,Dur_one) ->
      Dur_one
  | Dur_max(d1,d2) ->
       (match canon_dur d1,canon_dur d2 with
       | Dur_zero,d | d,Dur_zero -> d
       | Dur_one,_ | _,Dur_one -> Dur_one
       | d1,d2 -> Dur_max(d1,d2)
     )

  let pp_dur fmt (d:dur) : unit =
    let open Format in
    let rec pp fmt d =
    match canon_dur d with
    | Dur_var{contents=Unknown n} -> fprintf fmt "d%d" n
    | Dur_var{contents=Is d} -> pp fmt d
    | Dur_zero -> fprintf fmt "0"
    | Dur_one -> fprintf fmt "1"
    | Dur_max(d1,d2) -> fprintf fmt "max(%a,%a)" pp d1 pp d2
    in pp fmt d


  let pp_size fmt (sz:size) : unit =
    let open Format in
    let rec pp fmt sz =
    match canon_size sz with
    | Sz_var{contents=Unknown n} -> fprintf fmt "~z%d" n
    | Sz_var{contents=Is sz} -> pp fmt sz
    | Sz_lit k -> fprintf fmt "%d" k
    in pp fmt sz

let pp_tuple fmt pp vs =
  let open Format in
  fprintf fmt "(";
  pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ", ")
        pp fmt vs;
  fprintf fmt ")"

  let pp_tyB fmt (tyB:tyB) : unit =
    let open Format in
    let rec pp fmt tyB =
    match canon_tyB tyB with
    | TyB_var{contents=Unknown n} -> fprintf fmt "`b%d" n
    | TyB_var{contents=Is tyB} -> pp fmt tyB
    | TyB_bool -> fprintf fmt "bool"
    | TyB_unit -> fprintf fmt "unit"
    | TyB_int sz -> fprintf fmt "int<%a>" pp_size sz
    | TyB_tuple tyB_list -> pp_tuple fmt pp tyB_list
    | TyB_string sz -> fprintf fmt "string<%a>" pp_size sz
   in pp fmt tyB

  let pp_ty fmt (ty:ty) : unit =
    let open Format in
    let rec pp fmt ty =
    match canon_ty ty with
    | Ty_var{contents=Unknown n} -> fprintf fmt "'a%d" n
    | Ty_var{contents=Is ty} -> pp fmt ty
    | Ty_base tyB -> pp_tyB fmt tyB
    | Ty_tuple ty_list -> pp_tuple fmt pp ty_list
    | Ty_fun(ty1,d,tyB2) ->
        fprintf fmt "(%a -{%a}-> %a)" pp ty1 pp_dur d pp_tyB tyB2
    | Ty_ref tyB -> fprintf fmt "ref<%a>" pp_tyB tyB
    | Ty_array(sz,tyB) ->
        fprintf fmt "array<%a,%a>" pp_size sz pp_tyB tyB
    | Ty_matrix(sz_list,tyB) ->
        fprintf fmt "matrix<";
        fprintf fmt "(";
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt "*")
          pp_size fmt sz_list;
        fprintf fmt ",%a>" pp_tyB tyB
    in pp fmt ty


let new_unknown : unit -> int =
  let n = ref 0 in
  fun () -> n := !n + 1; !n

let new_size_unknown() =
  Sz_var (ref (Unknown (new_unknown ())))

let new_dur_unknown() =
  Dur_var (ref (Unknown (new_unknown ())))

let new_tyB_unknown() =
  TyB_var (ref (Unknown (new_unknown ())))

let new_ty_unknown() =
  Ty_var (ref (Unknown (new_unknown ())))


(* module V = struct
  type tag = Ty | Tyb | Dur | Sz
  type t = tag * int
  let compare = Stdlib.compare
end *)

module Vs = Set.Make(Int)
type scheme = Forall of (Vs.t * ty)

 exception Found

let rec occur_size v sz =
 let rec occ = function
  | Sz_var {contents=Unknown v'} ->
      if v = v' then raise Found
  | Sz_var {contents=Is sz} -> occ sz
  | Sz_lit _ -> ()
  in occ sz

let rec occur_dur v d =
 let rec occ = function
  | Dur_var {contents=Unknown v'} ->
      if v = v' then raise Found
  | Dur_var {contents=Is d} -> occ d
  | Dur_zero | Dur_one -> ()
  | Dur_max(d1,d2) -> occ d1; occ d2
  in occ d

let rec occur_tyB v tyB =
  let rec occ = function
  | TyB_var {contents=Unknown v'} ->
      if v = v' then raise Found
  | TyB_var {contents=Is tyB} -> occ tyB
  | TyB_bool | TyB_unit -> ()
  | TyB_int sz -> occur_size v sz
  | TyB_tuple tyB_list ->
      List.iter occ tyB_list
  | TyB_string sz -> occur_size v sz
  in occ tyB

let rec occur_ty v ty =
  let rec occ = function
  | Ty_var {contents=Unknown v'} ->
      if v = v' then raise Found
  | Ty_var {contents=Is ty} -> occ ty
  | Ty_base tyB ->
      occur_tyB v tyB
  | Ty_tuple ty_list ->
      List.iter occ ty_list
  | Ty_fun(ty1,d,tyB2) ->
      occ ty1;
      occur_dur v d;
      occur_tyB v tyB2
  | Ty_ref tyB ->
      occur_tyB v tyB
  | Ty_array(sz,tyB) ->
      occur_size v sz;
      occur_tyB v tyB
  | Ty_matrix(sz_list,tyB) ->
      List.iter (occur_size v) sz_list;
      occur_tyB v tyB
  in occ ty

let test_occur occ o =
  try occ o; false with Found -> true

let vars_of_size ?(s=Vs.empty) sz =
  let rec vars s = function
    | Sz_var {contents=Unknown n} -> Vs.add n s
    | Sz_var {contents=Is sz} -> vars s sz
    | Sz_lit _ -> s
  in vars s sz

let vars_of_dur ?(s=Vs.empty) d =
  let rec vars s = function
    | Dur_var {contents=Unknown n} -> Vs.add n s
    | Dur_var {contents=Is sz} -> vars s sz
    | Dur_zero | Dur_one -> s
    | Dur_max(d1,d2) -> vars (vars s d1) d2
  in vars s d

let vars_of_tyB ?(s=Vs.empty) tyB =
  let rec vars s = function
    | TyB_var {contents=Unknown n} -> Vs.add n s
    | TyB_var {contents=Is tyB} -> vars s tyB
    | TyB_bool | TyB_unit -> s
    | TyB_int sz -> vars_of_size ~s:s sz
    | TyB_tuple tyB_list ->
      List.fold_left vars s tyB_list
    | TyB_string sz -> vars_of_size ~s:s sz
  in
  vars s tyB

let vars_of_ty ?(s=Vs.empty) ty =
  let rec vars s = function
  | Ty_var {contents=Unknown n} -> Vs.add n s
  | Ty_var {contents=Is ty} -> vars s ty
  | Ty_base t -> vars_of_tyB ~s t
  | Ty_tuple ty_list ->
      List.fold_left vars s ty_list
  | Ty_fun(ty1,d,tyB2) ->
      let s1 = vars s ty1 in
      let s2 = vars_of_tyB ~s:s1 tyB2 in
      vars_of_dur ~s:s2 d
  | Ty_ref tyB -> vars_of_tyB ~s tyB
  | Ty_array (sz,tyB) ->
      let s1 = vars_of_size ~s:s sz in
      vars_of_tyB ~s:s1 tyB
  | Ty_matrix(sz_list,tyB) ->
      let s1 = List.fold_left (fun s sz -> vars_of_size ~s sz) s sz_list in
      vars_of_tyB ~s:s1 tyB
in vars s ty

  let free_vars_of_type (bv,t) =
    Vs.diff (vars_of_ty t) bv


let instance (Forall(vs,ty)) =
  (*   Vs.iter (fun n -> Printf.printf "===> %d\n" n) vs;*)
  let unknowns = Hashtbl.create (Vs.cardinal vs) in
  Vs.iter (fun n -> Hashtbl.add unknowns n (new_unknown())) vs;
  let rec inst_size = function
  | Sz_var {contents=Unknown n} as sz ->
          (try let n' = Hashtbl.find unknowns n in
               Sz_var {contents=Unknown n'}
           with Not_found -> sz)
  | Sz_var {contents=Is sz} ->
      inst_size sz
  | Sz_lit _ as k -> k in

  let rec inst_dur = function
  | Dur_var {contents=Unknown n} as d ->
          (try let n' = Hashtbl.find unknowns n in
               Dur_var {contents=Unknown n'}
           with Not_found -> d)
  | Dur_var {contents=Is d} ->
      inst_dur d
  | Dur_zero | Dur_one as d -> d
  | Dur_max(d1,d2) ->
      Dur_max(inst_dur d1,inst_dur d2) in
  let rec inst_tyB = function
  | TyB_var {contents=Unknown n} as tyB ->
          (try let n' = Hashtbl.find unknowns n in
                TyB_var {contents=Unknown n'}
           with Not_found -> tyB)
  | TyB_var {contents=Is tyB} ->
      inst_tyB tyB
  | TyB_bool | TyB_unit as tyB -> tyB
  | TyB_int sz -> TyB_int (inst_size sz)
  | TyB_tuple tyB_list ->
      TyB_tuple (List.map inst_tyB tyB_list)
  | TyB_string sz -> TyB_string (inst_size sz) in

  let rec inst_ty = function
  | Ty_var {contents=Unknown n} as ty ->
          (try let n' = Hashtbl.find unknowns n in
               Ty_var {contents=Unknown n'}
           with Not_found -> ty)
  | Ty_var {contents=Is ty} ->
      inst_ty ty
  | Ty_base tyB ->
      Ty_base(inst_tyB tyB)
  | Ty_tuple ty_list ->
      Ty_tuple(List.map inst_ty ty_list)
  | Ty_fun(ty1,d,tyB2) ->
      Ty_fun(inst_ty ty1,inst_dur d,inst_tyB tyB2)
  | Ty_ref(tyB) ->
      Ty_ref(inst_tyB tyB)
  | Ty_array(sz,tyB) ->
      Ty_array(inst_size sz, inst_tyB tyB)
  | Ty_matrix(sz_list,tyB) ->
      Ty_matrix(List.map inst_size sz_list, inst_tyB tyB)
  in
  inst_ty ty

  let free_vars_of_type_env l =
    List.fold_left (fun vs (x,(Forall (v,t))) ->
                  Vs.union vs (free_vars_of_type (v,t)) )
     Vs.empty l

  let generalize r ty =
    match canon_ty ty with
    | Ty_var{contents=Is (Ty_fun _)} | Ty_fun _ ->
        let fvg = free_vars_of_type_env r in
        Forall(free_vars_of_type (fvg,ty),ty)
    | ty -> Forall(Vs.empty,ty)

end
