type x = string


(** associative array having key of type [string] *)
module SMap = Map.Make(String)

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


type 'a var = 'a var_content ref
and 'a var_content = Unknown of int
                   | Is of 'a

type size = Sz_var of size var
          | Sz_lit of int

type tyB = TyB_var of tyB var
         | TyB_int of size
         | TyB_bool
         | TyB_unit
         | TyB_tuple of tyB list
         | TyB_sum of (tag * tyB) list
         | TyB_string of size
         | TyB_size of size
         | TyB_abstract of x * size list * tyB list (* size in number of bits *)

and tag = string

type dur = Dur_var of dur var
        | Dur_zero
        | Dur_one
        | Dur_max of dur * dur

type ty = Ty_var of ty var
        | Ty_base of tyB
        | Ty_tuple of ty list
        | Ty_fun of ty * dur * tyB
        | Ty_ref of tyB
        | Ty_array of size * tyB

let dur_add d1 d2 =
  Dur_max(d1,d2)

let rec canon_size = function
| Sz_var ({contents=Is sz} as v) ->
  let sz' = canon_size sz in
  v := Is sz'; sz'
| Sz_var {contents=Unknown _} as sz -> sz
| Sz_lit _ as n -> n

let rec canon_tyB = function
| TyB_var ({contents=Is tyB} as v) ->
    let tyB' = canon_tyB tyB in
    v := Is tyB'; tyB'
| TyB_var {contents=Unknown _} as tyB -> tyB
| TyB_int sz -> TyB_int (canon_size sz)
| TyB_bool -> TyB_bool
| TyB_unit -> TyB_unit
| TyB_tuple tyB_list ->
    TyB_tuple (List.map canon_tyB tyB_list)
| TyB_size(sz) ->
    TyB_size(canon_size sz)
| TyB_sum(ctors) ->
    TyB_sum(List.map (fun (tag,tyB) -> (tag,canon_tyB tyB)) ctors)
| TyB_string sz ->
    TyB_string (canon_size sz)
| TyB_abstract (x,szs,tyB_list) ->
    TyB_abstract (x,List.map canon_size szs, List.map canon_tyB tyB_list)

let rec canon_dur = function
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
     let d1,d2 = canon_dur d1,canon_dur d2 in
     (match d1,d2 with
     | Dur_zero,d | d,Dur_zero -> d
     | Dur_one,_ | _,Dur_one -> Dur_one
     | Dur_var {contents=(Unknown n)},
       Dur_var {contents=(Unknown m)} ->
         if n = m then d1 else Dur_max(d1,d2)
     | _ -> Dur_max(d1,d2)
   )

let rec canon_ty = function
| Ty_var ({contents=Is ty} as v) ->
    let ty' = canon_ty ty in
    (*match ty' with
    | Ty_base(TyB_var{contents=Unknown n}) ->
      v := Unknown n; ty'
    | _ ->
*) v := Is ty'; ty'
| Ty_var {contents=Unknown _} as ty -> ty
| Ty_base tyB -> Ty_base(canon_tyB tyB)
| Ty_tuple ty_list ->
    (let exception E in
    let ty_list_candidates = List.map canon_ty ty_list in
    try
      Ty_base (TyB_tuple (List.map (function
                                 | Ty_base tyB -> tyB
                                 | _ -> raise E) ty_list_candidates))
    with E ->
      Ty_tuple (ty_list_candidates))
| Ty_fun(ty,dur,tyB) ->
    Ty_fun(canon_ty ty,
           canon_dur dur,
           canon_tyB tyB)
| Ty_ref(tyB) ->
    Ty_ref(canon_tyB tyB)
| Ty_array(sz,tyB) ->
    Ty_array(canon_size sz, canon_tyB tyB)


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
      ~pp_sep:(fun fmt () -> fprintf fmt " * ")
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
  | TyB_size(sz) ->
      fprintf fmt "size<%a>" pp_size sz
  | TyB_sum(ctors) ->
     fprintf fmt "(";
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt " | ")
        (fun fmt (x,t) ->
           fprintf fmt "%s of %a" x pp t) fmt ctors;
        fprintf fmt ")"
  | TyB_string sz -> fprintf fmt "string<%a>" pp_size sz
  | TyB_abstract (x,szs,tyB_list) -> 
      if tyB_list <> [] then 
        (fprintf fmt "(";
         pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp fmt tyB_list;
         fprintf fmt ") ") else ();

         (fprintf fmt "%s<" x;
         pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_size fmt szs;
         fprintf fmt ">")
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
  in pp fmt ty


let new_unknown : unit -> int =
let n = ref 0 in
fun () -> n := !n + 1; !n

let new_unknown_generic() =
  (ref (Unknown (new_unknown ())))

let find_unsafe 
    (unknowns : (int,'a var_content ref) Hashtbl.t) 
    (n : int) : 'b var_content ref = 
    (* TODO: avoid Obj. (it is difficult 
      to do it differently while preserving sharing) *)
    Obj.magic @@ Hashtbl.find unknowns n

let new_size_unknown() =
Sz_var (ref (Unknown (new_unknown ())))

let new_dur_unknown() =
Dur_var (ref (Unknown (new_unknown ())))

let new_tyB_unknown() =
TyB_var (ref (Unknown (new_unknown ())))

let new_ty_unknown() =
Ty_var (ref (Unknown (new_unknown ())))


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
| TyB_size sz -> occur_size v sz
| TyB_tuple tyB_list ->
    List.iter occ tyB_list
| TyB_sum ctors ->
    List.iter (fun (_,tyB) -> occ tyB) ctors
| TyB_string sz -> occur_size v sz
| TyB_abstract(_,szs,tyB_list) ->
    List.iter (occur_size v) szs; List.iter occ tyB_list
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
  | TyB_size sz -> vars_of_size ~s sz
  | TyB_sum ctors ->
      List.fold_left (fun s (_,tyB) -> vars s tyB) s ctors
  | TyB_string sz -> vars_of_size ~s:s sz
  | TyB_abstract(_,szs,tyB_list) -> 
      let s = List.fold_left (fun s -> vars_of_size ~s) s szs in
      List.fold_left vars s tyB_list
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
in vars s ty

let free_vars_of_type (bv,t) =
  Vs.diff (vars_of_ty t) bv


let instance (Forall(vs,ty)) =
  let unknowns = Hashtbl.create (Vs.cardinal vs) in
  Vs.iter (fun n -> Hashtbl.add unknowns n (new_unknown_generic())) vs;
  let rec inst_size = function
  | Sz_var {contents=Unknown n} as sz ->
          (try Sz_var(find_unsafe unknowns n)
           with Not_found -> sz)
  | Sz_var {contents=Is sz} ->
      inst_size sz
  | Sz_lit _ as k -> k in

  let rec inst_dur = function
  | Dur_var {contents=Unknown n} as d ->
          (try Dur_var(find_unsafe unknowns n)
           with Not_found -> d)
  | Dur_var {contents=Is d} ->
      inst_dur d
  | Dur_zero | Dur_one as d -> d
  | Dur_max(d1,d2) ->
      Dur_max(inst_dur d1,inst_dur d2) in
  let rec inst_tyB = function
  | TyB_var {contents=Unknown n} as tyB ->
          (try TyB_var(find_unsafe unknowns n)
           with Not_found -> tyB)
  | TyB_var {contents=Is tyB} ->
      inst_tyB tyB
  | TyB_bool | TyB_unit as tyB -> tyB
  | TyB_int sz -> TyB_int (inst_size sz)
  | TyB_tuple tyB_list ->
      TyB_tuple (List.map inst_tyB tyB_list)
  | TyB_size sz ->
      TyB_size(inst_size sz)
  | TyB_sum ctors ->
      TyB_sum (List.map (fun (tag,tyB) -> tag,inst_tyB tyB) ctors)
  | TyB_string sz -> TyB_string (inst_size sz)
  | TyB_abstract(x,szs,tyB_list) ->
      TyB_abstract (x,List.map inst_size szs,List.map inst_tyB tyB_list)
  in

  let rec inst_ty = function
  | Ty_var {contents=Unknown n} as ty ->
          (try Ty_var(Obj.magic @@ Hashtbl.find unknowns n)
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
  in
  inst_ty ty

  let free_vars_of_type_env l =
    List.fold_left (fun vs (x,(Forall (v,t))) ->
                  Vs.union vs (free_vars_of_type (v,t)) )
     Vs.empty l

let generalize r ty =
  let ty = canon_ty ty in
  match ty with
  | Ty_var{contents=Is (Ty_fun _)} | Ty_fun _ ->
      let fvg = free_vars_of_type_env r in
      let vs = free_vars_of_type (fvg,ty) in
      (* Format.fprintf Format.std_formatter
                   "@[<v>gives: %a @]"
                   Prelude.Errors.(emph_pp green pp_ty) ty; *)
      (* Vs.iter (fun n -> Printf.printf "           ??===> %d\n" n) vs; *)
      Forall(vs,ty)
  | ty -> Forall(Vs.empty,ty)


let rec as_tyB ~loc ty = 
  match ty with
  | Ty_base tyB -> tyB
  | Ty_tuple(tys) -> TyB_tuple(List.map (as_tyB ~loc) tys)
  | Ty_var({contents=Unknown n} as r) -> TyB_var(Obj.magic r)
  | _ -> Prelude.Errors.raise_error ~loc ()
                 ~msg:"basic type expected"


let rec no_unknown_in_ty t =
  assert false (* todo *)











let rec rename_size unknowns = function
| Sz_var {contents=Unknown n} as sz ->
        (try Sz_var(find_unsafe unknowns n)
         with Not_found -> sz)
| Sz_var ({contents=Is sz} as v) ->
    v := Is (rename_size unknowns sz); Sz_var v
| Sz_lit _ as k -> k

let rec rename_dur unknowns = function
| Dur_var {contents=Unknown n} as d ->
        (try Dur_var(find_unsafe unknowns n)
         with Not_found -> d)
| Dur_var {contents=Is d} ->
    rename_dur unknowns d
| Dur_zero | Dur_one as d -> d
| Dur_max(d1,d2) ->
    Dur_max(rename_dur unknowns d1,
            rename_dur unknowns d2) 

let rec rename_tyB unknowns = function
| TyB_var {contents=Unknown n} as tyB ->
        (try TyB_var(find_unsafe unknowns n)
         with Not_found -> tyB)
| TyB_var {contents=Is tyB} ->
    rename_tyB unknowns tyB
| TyB_bool | TyB_unit as tyB -> tyB
| TyB_int sz -> TyB_int (rename_size unknowns sz)
| TyB_tuple tyB_list ->
    TyB_tuple (List.map (rename_tyB unknowns) tyB_list)
| TyB_size sz ->
    TyB_size(rename_size unknowns sz)
| TyB_sum ctors ->
    TyB_sum (List.map (fun (tag,tyB) -> tag,rename_tyB unknowns tyB) ctors)
| TyB_string sz -> TyB_string (rename_size unknowns sz)
| TyB_abstract(x,szs,tyB_list) ->
    TyB_abstract (x,List.map (rename_size unknowns) szs,List.map (rename_tyB unknowns) tyB_list)


let rec rename_ty unknowns = function
| Ty_var {contents=Unknown n} as ty ->
        (try Ty_var(Obj.magic @@ Hashtbl.find unknowns n)
         with Not_found -> ty)
| Ty_var {contents=Is ty} ->
    rename_ty unknowns ty
| Ty_base tyB ->
    Ty_base(rename_tyB unknowns tyB)
| Ty_tuple ty_list ->
    Ty_tuple(List.map (rename_ty unknowns) ty_list)
| Ty_fun(ty1,d,tyB2) ->
    Ty_fun(rename_ty unknowns ty1,rename_dur unknowns d,rename_tyB unknowns tyB2)
| Ty_ref(tyB) ->
    Ty_ref(rename_tyB unknowns tyB)
| Ty_array(sz,tyB) ->
    Ty_array(rename_size unknowns sz, rename_tyB unknowns tyB)


