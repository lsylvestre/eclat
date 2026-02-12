let one_hot_encoding_flag = ref true


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
and unknown = {id:int;mutable name:string option}
and 'a var_content = Unknown of unknown
                   | Is of 'a

type size = Sz_var of size var
          | Sz_lit of int
          | Sz_add of size * int
          | Sz_twice of size
type tyB = TyB_var of tyB var
         | TyB_int of size
         | TyB_bool
         | TyB_unit
         | TyB_tuple of tyB list
         | TyB_sum of (tag * tyB) list
         | TyB_string of size
         | TyB_abstract of x * size list * tyB list (* size in number of bits *)
         | TyB_alias of x * size list * tyB list
         | TyB_record of {fields:tyB SMap.t;row:tyB}
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
        | Ty_signal of tyB
        | Ty_trap of tyB
        | Ty_size of size
        | Ty_alias of x * size list * ty list
let dur_add d1 d2 =
  Dur_max(d1,d2)

let rec canon_size = function
| Sz_var ({contents=Is sz} as v) ->
  let sz' = canon_size sz in
  v := Is sz'; sz'
| Sz_var {contents=Unknown _} as sz -> sz
| Sz_lit _ as n -> n
| Sz_add(sz,0) -> canon_size sz
| Sz_add(sz,n) ->
   (match canon_size sz with
    | Sz_lit m -> Sz_lit(n+m)
    | sz -> Sz_add (sz,n))
| Sz_twice(sz) -> 
    (match canon_size sz with
    | Sz_lit m -> Sz_lit(2*m)
    | sz -> Sz_twice (sz))

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
| TyB_sum(ctors) ->
    TyB_sum(List.map (fun (tag,tyB) -> (tag,canon_tyB tyB)) ctors)
| TyB_string sz ->
    TyB_string (canon_size sz)
| TyB_abstract (x,sz_list,tyB_list) ->
    TyB_abstract (x,List.map canon_size sz_list, List.map canon_tyB tyB_list)
| TyB_alias (x,sz_list,tyB_list) ->
    TyB_alias (x,List.map canon_size sz_list, List.map canon_tyB tyB_list)
| TyB_record {fields=b_list;row} ->
    let extra,row' = match canon_tyB row with
                      | TyB_record{fields=bs';row=row'} ->
                           bs',row'
                      | v -> SMap.empty, v in
    TyB_record {fields=SMap.union (fun x t1 t2 -> Some t1) (* (TyB_as(tyB1,tyB2))) *)
    (SMap.map canon_tyB b_list) extra;row=row'}

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
     | Dur_var {contents=(Unknown{id=n;name=_})},
       Dur_var {contents=(Unknown{id=m;name=_})} ->
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
| Ty_signal(tyB) ->
    Ty_signal(canon_tyB tyB)
| Ty_trap(tyB) ->
    Ty_trap(canon_tyB tyB)
| Ty_size(sz) ->
    Ty_size(canon_size sz)
| Ty_alias (x,sz_list,ty_list) ->
    Ty_alias (x,List.map canon_size sz_list, List.map canon_ty ty_list)

type alias_entry = 
| Alias of (ty * x list * x list) * Prelude.loc
| Abstract of (string * size list * tyB list * int list) * Prelude.loc

let global_type_declarations : (x,alias_entry) Hashtbl.t = Hashtbl.create 10

let pp_dur fmt (d:dur) : unit =
  let open Format in
  let rec pp fmt d =
  match canon_dur d with
  | Dur_var{contents=Unknown{id=n;name}} ->
      (match name with
      | None -> fprintf fmt "$%d" n
      | Some x -> fprintf fmt "$%s%d" x n)
  | Dur_var{contents=Is d} -> pp fmt d
  | Dur_zero -> fprintf fmt "0"
  | Dur_one -> fprintf fmt "1"
  | Dur_max(d1,d2) -> fprintf fmt "max(%a,%a)" pp d1 pp d2
  in pp fmt d


let pp_size fmt (sz:size) : unit =
  let open Format in
  let rec pp fmt sz =
  match canon_size sz with
  | Sz_var{contents=Unknown{id=n;name}} ->
       (match name with
        | None -> fprintf fmt "?%d" n
        | Some x -> fprintf fmt "?%s%d" x n)
  | Sz_var{contents=Is sz} -> pp fmt sz
  | Sz_lit k -> fprintf fmt "%d" k
  | Sz_add(sz,n) -> fprintf fmt "(%a + %d)" pp sz n
  | Sz_twice(sz) -> fprintf fmt "(2 * %a)" pp sz
  in pp fmt sz

let pp_tuple fmt pp vs =
let open Format in
fprintf fmt "(";
pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt " * ")
      pp fmt vs;
fprintf fmt ")"

let pp_tyX_ident fmt pp_tyX x sz_list tyX_list =
  let open Format in
  let n = List.compare_length_with tyX_list 1 in
  if n > 0 then fprintf fmt "(";
  pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_tyX fmt tyX_list;
  if n > 0 then fprintf fmt ")"; 
  if tyX_list <> [] then fprintf fmt " ";
  (fprintf fmt "%s" x;
  match sz_list with
  | [] -> ()
  | _ -> fprintf fmt "<";
         pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_size fmt sz_list;
       fprintf fmt ">")

let pp_tyB fmt (tyB:tyB) : unit =
  let open Format in
  let rec pp fmt tyB =
  match canon_tyB tyB with
  | TyB_var{contents=Unknown{id=n;name}} ->
       (match name with
        | None -> fprintf fmt "~%d" n
        | Some x -> fprintf fmt "~%s%d" x n)
  | TyB_var{contents=Is tyB} -> pp fmt tyB
  | TyB_bool -> fprintf fmt "bool"
  | TyB_unit -> fprintf fmt "unit"
  | TyB_int sz -> fprintf fmt "int<%a>" pp_size sz
  | TyB_tuple tyB_list -> pp_tuple fmt pp tyB_list
  | TyB_sum(ctors) ->
     fprintf fmt "(";
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt " | ")
        (fun fmt (x,tyB) ->
           fprintf fmt "%s of %a" x pp tyB) fmt ctors;
        fprintf fmt ")"
  | TyB_string sz -> fprintf fmt "string<%a>" pp_size sz
  | TyB_abstract (x,sz_list,tyB_list) ->
      (* fprintf fmt "abstract:"; *)
      pp_tyX_ident fmt pp x sz_list tyB_list
 | TyB_alias (x,sz_list,tyB_list) ->
      pp_tyX_ident fmt pp x sz_list tyB_list
 | TyB_record{fields=b_list; row} ->
      fprintf fmt "{";
      pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt "; ")
          (fun fmt (x,tyB) ->
             Prelude.Errors.(emph blue fmt x);
             fprintf fmt " : %a" pp tyB) fmt (SMap.bindings b_list);
      if canon_tyB row <> TyB_unit then ( 
        fprintf fmt "; ";
        Prelude.Errors.(emph_pp green pp fmt row));
      fprintf fmt "}"
 in pp fmt tyB

let pp_ty fmt (ty:ty) : unit =
  let open Format in
  let rec pp fmt ty =
  match canon_ty ty with
  | Ty_var{contents=Unknown{id=n;name}} ->
       (match name with
        | None -> fprintf fmt "'%d" n
        | Some x -> fprintf fmt "'%s%d" x n)
  | Ty_var{contents=Is ty} -> pp fmt ty
  | Ty_base tyB -> pp_tyB fmt tyB
  | Ty_tuple ty_list -> pp_tuple fmt pp ty_list
  | Ty_fun(ty1,d,tyB2) ->
      (match d with
      | Dur_zero -> fprintf fmt "(%a => %a)" pp ty1 pp_tyB tyB2
      | Dur_one -> fprintf fmt "(%a -> %a)" pp ty1 pp_tyB tyB2
      | _ -> fprintf fmt "(%a -{%a}-> %a)" pp ty1 pp_dur d pp_tyB tyB2)
  | Ty_ref tyB -> fprintf fmt "%a ref" pp_tyB tyB
  | Ty_array(sz,tyB) ->
      fprintf fmt "%a array<%a>" pp_tyB tyB pp_size sz
  | Ty_signal(tyB) ->
      fprintf fmt "%a signal" pp_tyB tyB
  | Ty_trap(tyB) ->
      fprintf fmt "%a trap" pp_tyB tyB
  | Ty_size(sz) ->
      fprintf fmt "size<%a>" pp_size sz
  | Ty_alias (x,sz_list,ty_list) ->
      pp_tyX_ident fmt pp x sz_list ty_list
  in pp fmt ty


let new_unknown : unit -> int =
let n = ref 0 in
fun () -> n := !n + 1; !n

let new_unknown_generic ?name () =
  (ref (Unknown{id=new_unknown ();name}))

let find_unsafe 
    (unknowns : (int,'a var_content ref) Hashtbl.t) 
    (u : unknown) : 'b var_content ref = 
    (* TODO: avoid Obj. (it is difficult 
      to do it differently while preserving sharing) *)
    Obj.magic @@ Hashtbl.find unknowns u.id

let new_size_unknown ?name () =
Sz_var (ref (Unknown {id=new_unknown ();name}))

let new_dur_unknown ?name () =
Dur_var (ref (Unknown {id=new_unknown ();name}))

let new_tyB_unknown ?name () =
TyB_var (ref (Unknown {id=new_unknown ();name}))

let new_ty_unknown ?name () =
Ty_var (ref (Unknown {id=new_unknown ();name}))

type kind = T | B | S | D

module Vs = Map.Make(struct
                       type t = unknown 
                       let compare {id=n;_} {id=m;_} = compare n m
                     end)
type scheme = Forall of (kind Vs.t * ty)

exception Found

let rec occur_size v sz =
let rec occ = function
| Sz_var {contents=Unknown{id=v';name=_}} ->
    if v = v' then raise Found
| Sz_var {contents=Is sz} -> occ sz
| Sz_lit _ -> ()
| Sz_add(sz,_) -> occ sz
| Sz_twice(sz) -> occ sz
in occ sz

let rec occur_dur v d =
let rec occ = function
| Dur_var {contents=Unknown{id=v';name=_}} ->
    if v = v' then raise Found
| Dur_var {contents=Is d} -> occ d
| Dur_zero | Dur_one -> ()
| Dur_max(d1,d2) -> occ d1; occ d2
in occ d

let rec occur_tyB v tyB =
  let rec occ = function
  | TyB_var {contents=Unknown{id=v';name}} ->
      if v = v' then raise Found
  | TyB_var {contents=Is tyB} -> occ tyB
  | TyB_bool | TyB_unit -> ()
  | TyB_int sz -> occur_size v sz
  | TyB_tuple tyB_list ->
      List.iter occ tyB_list
  | TyB_sum ctors ->
      List.iter (fun (_,tyB) -> occ tyB) ctors
  | TyB_string sz -> occur_size v sz
  | TyB_abstract (_, sz_list, tyB_list)
  | TyB_alias    (_, sz_list, tyB_list) ->
      List.iter (occur_size v) sz_list; List.iter occ tyB_list
  | TyB_record{fields=b_list;row} ->
      SMap.iter (fun x tyB -> occ tyB) b_list;
      occ row
in occ tyB

let rec occur_ty v ty =
  let rec occ = function
  | Ty_var {contents=Unknown{id=v';name}} ->
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
  | Ty_signal(tyB) ->
      occur_tyB v tyB
  | Ty_trap(tyB) ->
      occur_tyB v tyB
  | Ty_size sz ->
      occur_size v sz
  | Ty_alias(_, sz_list, ty_list) ->
    List.iter (occur_size v) sz_list; List.iter occ ty_list
  in occ ty

let test_occur occ o =
  try occ o; false with Found -> true

let vars_of_size ?(s=Vs.empty) sz =
  let rec vars s = function
    | Sz_var {contents=Unknown u} -> Vs.add u S s
    | Sz_var {contents=Is sz} -> vars s sz
    | Sz_lit _ -> s
    | Sz_add(sz,_) -> vars s sz
    | Sz_twice(sz) -> vars s sz
  in vars s sz

let vars_of_dur ?(s=Vs.empty) d =
  let rec vars s = function
    | Dur_var {contents=Unknown u} ->
        Vs.add u D s
    | Dur_var {contents=Is sz} -> vars s sz
    | Dur_zero | Dur_one -> s
    | Dur_max(d1,d2) -> vars (vars s d1) d2
  in vars s d

let vars_of_tyB ?(s=Vs.empty) tyB =
  let rec vars s = function
    | TyB_var {contents=Unknown u} ->
        Vs.add u B s
    | TyB_var {contents=Is tyB} -> vars s tyB
    | TyB_bool | TyB_unit -> s
    | TyB_int sz -> vars_of_size ~s:s sz
    | TyB_tuple tyB_list ->
        List.fold_left vars s tyB_list
    | TyB_sum ctors ->
        List.fold_left (fun s (_,tyB) -> vars s tyB) s ctors
    | TyB_string sz -> vars_of_size ~s:s sz
    | TyB_abstract(_, sz_list, tyB_list)
    | TyB_alias   (_, sz_list, tyB_list) -> 
        let s = List.fold_left (fun s -> vars_of_size ~s) s sz_list in
        List.fold_left vars s tyB_list
    | TyB_record{fields=b_list;row} ->
        let s = SMap.fold (fun x tyB s -> vars s tyB) b_list s in
        vars s row
  in
  vars s tyB

let vars_of_ty ?(s=Vs.empty) ty =
  let rec vars s = function
  | Ty_var {contents=Unknown u} ->
      Vs.add u T s
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
      let s1 = vars_of_size ~s sz in
      vars_of_tyB ~s:s1 tyB
  | Ty_signal (tyB) ->
      vars_of_tyB ~s:s tyB
  | Ty_trap (tyB) ->
      vars_of_tyB ~s:s tyB
  | Ty_size sz ->
      vars_of_size ~s sz
  | Ty_alias (_, sz_list, ty_list) -> 
      let s = List.fold_left (fun s -> vars_of_size ~s) s sz_list in
      List.fold_left vars s ty_list
  in vars s ty

let vs_diff vs1 vs2 =
  Vs.filter (fun v k -> not (Vs.mem v vs2)) vs1

let free_vars_of_type (bv,t) =
  vs_diff (vars_of_ty t) bv

let rec instance ?(new_unknown=new_unknown_generic) s = 
  instance_aux ~new_unknown s 
and instance_aux ~new_unknown
                  (Forall(vs,ty)) =
  let unknowns = Hashtbl.create (Vs.cardinal vs) in
  Vs.iter (fun u _ -> let name = u.name in
        Hashtbl.add unknowns u.id (new_unknown ?name ())) vs;
  let rec inst_size = function
  | Sz_var {contents=Unknown u} as sz ->
          (try Sz_var (find_unsafe unknowns u)
           with Not_found -> sz)
  | Sz_var {contents=Is sz} ->
      inst_size sz
  | Sz_lit _ as k -> k
  | Sz_add(sz,n) -> Sz_add (inst_size sz,n)
  | Sz_twice(sz) -> Sz_twice(inst_size sz)
  in
  let rec inst_dur = function
  | Dur_var {contents=Unknown u} as d ->
          (try Dur_var (find_unsafe unknowns u)
           with Not_found -> d)
  | Dur_var {contents=Is d} ->
      inst_dur d
  | Dur_zero | Dur_one as d -> d
  | Dur_max(d1,d2) ->
      Dur_max(inst_dur d1,inst_dur d2)
  in
  let rec inst_tyB = function
  | TyB_var {contents=Unknown u} as tyB ->
          (try TyB_var(find_unsafe unknowns u)
           with Not_found -> tyB)
  | TyB_var {contents=Is tyB} ->
      inst_tyB tyB
  | (TyB_bool | TyB_unit) as tyB -> tyB
  | TyB_int sz -> TyB_int (inst_size sz)
  | TyB_tuple tyB_list ->
      TyB_tuple (List.map inst_tyB tyB_list)
  | TyB_sum ctors ->
      TyB_sum (List.map (fun (tag,tyB) -> tag,inst_tyB tyB) ctors)
  | TyB_string sz -> TyB_string (inst_size sz)
  | TyB_abstract (x,sz_list,tyB_list) ->
      TyB_abstract (x,List.map inst_size sz_list,List.map inst_tyB tyB_list)
  | TyB_alias (x,sz_list,tyB_list) ->
      TyB_alias (x,List.map inst_size sz_list,List.map inst_tyB tyB_list)
  | TyB_record{fields=b_list;row} ->
      TyB_record{fields=SMap.map inst_tyB b_list;
                 row=inst_tyB row}
  in

  let rec inst_ty = function
  | Ty_var {contents=Unknown u} as ty ->
          (try Ty_var(find_unsafe unknowns u)
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
  | Ty_signal(tyB) ->
      Ty_signal(inst_tyB tyB)
  | Ty_trap(tyB) ->
      Ty_trap(inst_tyB tyB)
  | Ty_size sz ->
      Ty_size(inst_size sz) 
  | Ty_alias (x,sz_list,ty_list) ->
      Ty_alias (x,List.map inst_size sz_list,List.map inst_ty ty_list)
  in
  inst_ty ty

let rec rename_size unknowns = function
| Sz_var {contents=Unknown u} as sz ->
        (try Sz_var(find_unsafe unknowns u)
         with Not_found -> sz)
| Sz_var ({contents=Is sz} as _v) ->
    (* _v := Is ( *) rename_size unknowns sz (* ); Sz_var _v *)
| Sz_lit _ as k -> k
| Sz_add(sz,n) -> Sz_add (rename_size unknowns sz, n)
| Sz_twice(sz) -> Sz_twice(rename_size unknowns sz)

let rec rename_dur unknowns = function
| Dur_var {contents=Unknown u} as d ->
        (try Dur_var(find_unsafe unknowns u)
         with Not_found -> d)
| Dur_var ({contents=Is d} as _v) ->
    rename_dur unknowns d
| Dur_zero | Dur_one as d -> d
| Dur_max(d1,d2) ->
    Dur_max(rename_dur unknowns d1,
            rename_dur unknowns d2) 

let rec rename_tyB unknowns = function
| TyB_var {contents=Unknown u} as tyB ->
        (try TyB_var(find_unsafe unknowns u)
         with Not_found -> tyB)
| TyB_var ({contents=Is tyB} as _v) ->
    rename_tyB unknowns tyB
| TyB_bool | TyB_unit as tyB -> tyB
| TyB_int sz -> TyB_int (rename_size unknowns sz)
| TyB_tuple tyB_list ->
    TyB_tuple (List.map (rename_tyB unknowns) tyB_list)
| TyB_sum ctors ->
    TyB_sum (List.map (fun (tag,tyB) -> tag,rename_tyB unknowns tyB) ctors)
| TyB_string sz -> TyB_string (rename_size unknowns sz)
| TyB_abstract(x,sz_list,tyB_list) ->
    TyB_abstract (x,List.map (rename_size unknowns) sz_list,List.map (rename_tyB unknowns) tyB_list)
| TyB_alias(x,sz_list,tyB_list) ->
    TyB_alias (x,List.map (rename_size unknowns) sz_list,List.map (rename_tyB unknowns) tyB_list)
| TyB_record{fields=b_list;row} ->
    TyB_record{fields=SMap.map (rename_tyB unknowns) b_list;
               row=rename_tyB unknowns row}

let rec rename_ty unknowns = function
| Ty_var {contents=Unknown u} as ty ->
        (try Ty_var(find_unsafe unknowns u)
         with Not_found -> ty)
| Ty_var ({contents=Is ty} as _v) ->
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
| Ty_signal(tyB) ->
    Ty_signal(rename_tyB unknowns tyB)
| Ty_trap(tyB) ->
    Ty_trap(rename_tyB unknowns tyB)
| Ty_size sz ->
    Ty_size(rename_size unknowns sz)
| Ty_alias(x,sz_list,ty_list) ->
    Ty_alias (x,List.map (rename_size unknowns) sz_list,List.map (rename_ty unknowns) ty_list)

let free_vars_of_type_env l =
  List.fold_left (fun vs (x,(Forall (v,t))) ->
                Vs.union (fun v k1 k2 -> assert (k1=k2); Some k1) 
                  vs (free_vars_of_type (v,t)) )
   Vs.empty l

let rec subst_size vsize = function
| Sz_var {contents=Unknown u} as sz ->
    (match u.name with
    | None -> sz
    | Some x ->
       (match SMap.find_opt x vsize with
        | None -> sz
        | Some sz' -> sz'))
| Sz_var ({contents=Is sz} as _v) ->
    subst_size vsize sz
| Sz_lit _ as k -> k
| Sz_add(sz,n) -> Sz_add (subst_size vsize sz, n)
| Sz_twice(sz) -> Sz_twice(subst_size vsize sz)

let rec as_tyB ~loc ty = 
  match ty with
  | Ty_base tyB -> tyB
  | Ty_tuple(tys) -> TyB_tuple(List.map (as_tyB ~loc) tys)
  | Ty_var({contents=Unknown v} as r) -> 
      TyB_var(Obj.magic r)
  | Ty_alias(x,sz_list,ty_list) -> 
      let ty' = alias_instance ~loc x sz_list ty_list in
      (match canon_ty ty' with
       | Ty_base _ -> 
          TyB_alias(x,sz_list,List.map (as_tyB ~loc) ty_list)
       | _ -> Prelude.Errors.raise_error ~loc ()
                 ~msg:"basic type expected")
  | _ -> Prelude.Errors.raise_error ~loc ()
                 ~msg:"basic type expected"


and subst_tyB vsize va = function
| TyB_var {contents=Unknown u} as tyB ->
    (match u.name with
    | None -> tyB
    | Some x ->
       (match SMap.find_opt x va with
        | None -> tyB
        | Some ty -> as_tyB ~loc:Prelude.dloc ty))
| TyB_var {contents=Is tyB} ->
    subst_tyB vsize va tyB
| TyB_bool | TyB_unit as tyB -> tyB
| TyB_int sz -> TyB_int (subst_size vsize sz)
| TyB_tuple tyB_list ->
    TyB_tuple (List.map (subst_tyB vsize va) tyB_list)
| TyB_sum ctors ->
    TyB_sum (List.map (fun (tag,tyB) -> tag,subst_tyB vsize va tyB) ctors)
| TyB_string sz -> TyB_string (subst_size vsize sz)
| TyB_abstract(x,sz_list,tyB_list) ->
    TyB_abstract (x,List.map (subst_size vsize) sz_list,List.map (subst_tyB vsize va) tyB_list)
| TyB_alias(x,sz_list,tyB_list) ->
    TyB_alias (x,List.map (subst_size vsize) sz_list,List.map (subst_tyB vsize va) tyB_list)
| TyB_record{fields=b_list;row} ->
    TyB_record{fields=SMap.map (subst_tyB vsize va) b_list;
               row=subst_tyB vsize va row}

and subst_ty vsize va = function
| Ty_var {contents=Unknown u} as ty ->
    (match u.name with
    | None -> ty
    | Some x ->
       (match SMap.find_opt x va with
        | None -> ty
        | Some ty' -> ty'))
| Ty_var {contents=Is ty} ->
    subst_ty vsize va ty
| Ty_base tyB ->
    Ty_base(subst_tyB vsize va tyB)
| Ty_tuple ty_list ->
    Ty_tuple(List.map (subst_ty vsize va) ty_list)
| Ty_fun(ty1,d,tyB2) ->
    Ty_fun(subst_ty vsize va ty1,d,subst_tyB vsize va tyB2)
| Ty_ref(tyB) ->
    Ty_ref(subst_tyB vsize va tyB)
| Ty_array(sz,tyB) ->
    Ty_array(subst_size vsize sz, subst_tyB vsize va tyB)
| Ty_signal(tyB) ->
    Ty_signal(subst_tyB vsize va tyB)
| Ty_trap(tyB) ->
    Ty_trap(subst_tyB vsize va tyB)
| Ty_size sz ->
    Ty_size(subst_size vsize sz)
| Ty_alias(x,sz_list,ty_list) ->
    Ty_alias (x,List.map (subst_size vsize) sz_list,List.map (subst_ty vsize va) ty_list)

and alias_instance ?(loc=Prelude.dloc) x sz_list ty_list =
  match Hashtbl.find global_type_declarations x with
  | Alias ((ty',sz_argvs,ty_argsvs),_) ->
      if List.compare_lengths sz_argvs sz_list <> 0 then
         Prelude.Errors.error ~loc (fun fmt -> 
          Format.fprintf fmt "instantiation of type %s expect %d type parameters." x (List.length ty_argsvs));
      if List.compare_lengths ty_argsvs ty_list <> 0 then
          Prelude.Errors.error ~loc (fun fmt -> 
          Format.fprintf fmt "instantiation of type %s expect %d size parameters." x (List.length ty_argsvs));
      let sz_argvs' = List.fold_left2 (fun m vx sz -> 
                        SMap.add vx sz m) SMap.empty sz_argvs sz_list in
      let ty_argsvs' = List.fold_left2 (fun m vx ty -> 
                        SMap.add vx ty m) SMap.empty ty_argsvs ty_list in
      let ty'' = subst_ty sz_argvs' ty_argsvs' ty' in
      ty''
  | _ -> assert false (* not an alias *)


let generalize ?(only_functions=true) r ty =
  let ty = canon_ty ty in
  let gen_aux () = 
      let fvg = free_vars_of_type_env r in
      let vs = free_vars_of_type (fvg,ty) in
      (* rename_scheme *)(Forall(vs,ty))
  in
  if only_functions then
    match ty with
    | Ty_var{contents=Is (Ty_fun _)} | Ty_fun _ ->
        gen_aux ()
    | ty -> Forall(Vs.empty,ty)
  else gen_aux ()

let copy_ty ty =
  instance (generalize ~only_functions:false [] ty)

let rebase scm = (* type variable renumbering from 1, 2, 3 ... *)
  let c = ref 0 in
  let new_unknown ?name () =
    let name = match name with
               | Some _ -> name
               | None -> Some "v" in
    (ref (Unknown{id=(incr c; !c);name})) in
  (generalize ~only_functions:false [] (instance ~new_unknown scm))

let pp_scheme fmt scm =
  let (Forall(vs,ty)) = rebase scm in
  let open Format in 
  if Vs.cardinal vs > 0 then (
  Prelude.Errors.(emph bold fmt "forall ");
  Vs.iter (fun u k ->
             (match k with
             | T -> fprintf fmt "'"
             | B -> fprintf fmt "~"
             | S -> fprintf fmt "?"
             | D -> fprintf fmt "$");
             match u.name with
             | None -> fprintf fmt "%d " u.id
             | Some s -> fprintf fmt "%s%d " s u.id) vs;
  Prelude.Errors.(emph bold fmt " . "));
  pp_ty fmt ty

let rec is_tyB ty =
  (* alias for basic type should be put 
     in the form [Ty_base(TyB_alias _)] before *)
  match canon_ty ty with
  | Ty_base _ -> true
  | Ty_tuple(tys) -> List.for_all is_tyB tys
  | Ty_var({contents=Is ty}) -> is_tyB ty
  | Ty_var _ -> true
  | _ -> false

let rec extract_arg_res_fun ty =
  (* what to do with alias ? *)
  match canon_ty ty with
  | Ty_fun(ty1,d,tyB) -> (ty1,tyB)
  | _ -> assert false
  
let rec extract_ty_fun ty =
  (* what to do with alias ? *)
  match canon_ty ty with
  | Ty_fun(ty1,d,tyB) -> (ty1,d,tyB)
  | _ -> assert false

let contains_fun ty =
  let exception Find in
  let rec aux = function
  | Ty_var {contents=Is ty1} -> aux ty1
  | Ty_var _ -> ()
  | Ty_base _ -> () 
  | Ty_tuple ty_list -> List.iter aux ty_list
  | Ty_fun _ -> raise Find
  | Ty_ref _
  | Ty_array _
  | Ty_signal _
  | Ty_trap _
  | Ty_size _ -> ()
  | Ty_alias(x,_,ty_list) -> assert false (* todo *)
  in
  try aux ty; false 
  with Find -> true ;;


let rec no_unknown_in_ty t =
  assert false (* todo *)


let size_sz sz =
    let sz' = canon_size sz in
    let rec aux = function
    | Sz_lit n -> n
    | Sz_add (sz,n) -> aux sz + n
    | Sz_twice(sz) -> 2 * aux sz
    | _ -> 32
    in aux(sz')

let compute_tag_size cs =
  let n = 
    if !one_hot_encoding_flag
    then List.length cs
    else int_of_float @@ Float.ceil @@ Float.log2 @@ float @@ List.length cs
  in
  max 1 n
  
(*
let size_tyB tyB =
  let rec loop tyB = 
    match canon_tyB tyB with
    | TyB_var _ -> 32
    | TyB_bool | TyB_unit -> 1
    | TyB_int sz -> size_sz sz
    | TyB_tuple ts -> List.fold_left (+) 0 (List.map loop ts)
    | TyB_sum sum -> compute_tag_size sum + List.fold_left (max) 0 (List.map (fun (_,tyB) -> loop tyB) sum)
    | TyB_string sz -> size_sz sz * 8
    | _ -> Format.(fprintf std_formatter "[%a]" pp_tyB tyB); assert false (* todo *)
  in loop tyB *)

exception Find
let alias_find_exn_tyB x tyB =
  let rec alias tyB = 
    match tyB with
    | TyB_var {contents=Unknown _} ->
        ()
    | TyB_var {contents=Is tyB} -> alias tyB
    | TyB_bool | TyB_unit 
    | TyB_int _ -> ()
    | TyB_tuple tyB_list ->
        List.iter alias tyB_list
    | TyB_sum ctors ->
        List.iter (fun (_,tyB) -> alias tyB) ctors
    | TyB_string _ -> ()
    | TyB_abstract(y,_,tyB_list)
    | TyB_alias (y, _, tyB_list) -> 
        if x = y then raise Find
        else List.iter alias tyB_list
    | TyB_record{fields=b_list;row} ->
        SMap.iter (fun _ tyB -> alias tyB) b_list;
        alias row
  in
  alias tyB

let alias_find_tyB x tyB =
  try alias_find_exn_tyB x tyB; false with Find -> true



let alias_find_ty x ty =
  let exception Find in
  let rec alias ty = 
    match ty with
    | Ty_var {contents=Is ty1} -> alias ty1
    | Ty_var _ -> ()
    | Ty_base tyB -> alias_find_exn_tyB x tyB
    | Ty_tuple ty_list -> List.iter alias ty_list
    | Ty_fun(tya,_,tyB) ->
        alias tya;
        alias_find_exn_tyB x tyB
    | Ty_ref(tyB)
    | Ty_array(_,tyB)
    | Ty_signal(tyB) 
    | Ty_trap(tyB) -> alias_find_exn_tyB x tyB
    | Ty_size _ -> ()
    | Ty_alias(y,_,ty_list) ->
        if x = y then raise Find
        else List.iter alias ty_list
  in
  try alias ty; false with Find -> true

let rec remove_alias_tyB tyB =
  match tyB with
  | TyB_var {contents=Unknown _} -> tyB
  | TyB_var {contents=Is tyB'} ->
      remove_alias_tyB tyB'
  | TyB_bool 
  | TyB_unit
  | TyB_int _ -> tyB
  | TyB_tuple tyB_list ->
      TyB_tuple (List.map remove_alias_tyB tyB_list)
  | TyB_sum ctors ->
      TyB_sum (List.map (fun (tag,tyB) -> tag,remove_alias_tyB tyB) ctors)
  | TyB_string _ -> tyB
  | TyB_abstract(x,sz_list,tyB_list) ->
      TyB_abstract (x,sz_list,List.map remove_alias_tyB tyB_list)
  | TyB_alias(y,sz_list,tyB_list) ->
      remove_alias_tyB @@
      let ty = alias_instance y sz_list (List.map (fun t -> Ty_base t) tyB_list) in
      as_tyB ~loc:Prelude.dloc ty
  | TyB_record{fields=b_list;row} ->
      TyB_record{fields=SMap.map remove_alias_tyB b_list;
                 row=remove_alias_tyB row}

and remove_alias_ty ty =
  match ty with
  | Ty_var {contents=Unknown _} -> ty
  | Ty_var {contents=Is ty'} ->
      remove_alias_ty ty'
  | Ty_base tyB ->
      Ty_base(remove_alias_tyB tyB)
  | Ty_tuple ty_list ->
      Ty_tuple(List.map remove_alias_ty ty_list)
  | Ty_fun(ty1,d,tyB2) ->
      Ty_fun(remove_alias_ty ty1,d,remove_alias_tyB tyB2)
  | Ty_ref(tyB) ->
      Ty_ref(remove_alias_tyB tyB)
  | Ty_array(sz,tyB) ->
      Ty_array(sz, remove_alias_tyB tyB)
  | Ty_signal(tyB) ->
      Ty_signal(remove_alias_tyB tyB)
  | Ty_trap(tyB) ->
      Ty_trap(remove_alias_tyB tyB)
  | Ty_size _ -> ty
  | Ty_alias(x,sz_list,ty_list) ->
      remove_alias_ty @@
      alias_instance x sz_list (List.map remove_alias_ty ty_list)
