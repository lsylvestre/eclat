let one_hot_encoding_flag = ref true
let show_wcet_exp_flag = ref false

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

type size = Sz_var of size var (* can be quantified *)
          | Sz_lit of int
          | Sz_add of size * int
          | Sz_twice of size
          | Sz_pow2 of size

type tyB = TyB_var of tyB var (* can be quantified *)
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

type dur = Dur_var of dur var (* can be quantified *)
        | Dur_int of int
        | Dur_top
        | Dur_max of dur * dur
        | Dur_xor of dur * dur
        | Dur_add of dur * dur
        | Dur_mulDiv of size * dur * size
        | Dur_shared of label * dur (* shared resources to be sequentialized *)

and label = Label_var of label var (* can be quantified *)
          | Label_name of x        (* location of a shared resource *)

type ty = Ty_var of ty var (* can be quantified *)
        | Ty_base of tyB
        | Ty_tuple of ty list
        | Ty_fun of ty * dur * tyB
        | Ty_ref of tyB
        | Ty_array of size * tyB * label
        | Ty_signal of tyB
        | Ty_trap of tyB
        | Ty_size of size
        | Ty_alias of x * size list * ty list

module IMap = Map.Make(Int)

let rec canon_size = function
| Sz_var ({contents=Is sz} as v) ->
  let sz' = canon_size sz in
  v := Is sz'; sz'
| Sz_var {contents=Unknown _} as sz -> sz
| Sz_lit _ as n -> n
| Sz_add(sz,0) -> canon_size sz
| Sz_add(sz,n) ->
   (match canon_size sz with
    | Sz_lit m as sz' -> 
        let k = n+m in 
        if k >= n (* avoid overflow *)
        then Sz_lit(k)
        else Sz_add(sz',n)
    | sz -> Sz_add (sz,n))
| Sz_twice(sz) -> 
    (match canon_size sz with
    | Sz_lit m as sz' -> 
        let k = 2*m in 
        if k >= m (* avoid overflow *)
        then Sz_lit(k)
        else sz'
    | sz -> Sz_twice (sz))
| Sz_pow2(sz) ->
    (match canon_size sz with
    | Sz_lit m ->
        if m >= 63 (* avoid overflow *)
        then Sz_pow2(sz)
        else Sz_lit(2 lsl (m-1))
    | sz -> Sz_pow2 (sz))

let rec canon_label = function
| Label_var ({contents=Is l} as v) ->
    let l' = canon_label l in
    v := Is l'; l'
| l -> l

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
| Dur_var {contents=Unknown _}
| Dur_int _
| Dur_top as d -> d
| Dur_max(d1,d2) ->
    let d1,d2 = canon_dur d1,canon_dur d2 in
    (match d1,d2 with
    | Dur_top,_ | _,Dur_top -> Dur_top
    | Dur_int n,Dur_int m -> Dur_int (max n m)
    | Dur_int 0,d | d,Dur_int 0 -> d
    | Dur_var {contents=Unknown{id=_n;name=_}},
      Dur_var {contents=Unknown{id=_m;name=_}} ->
        (** note: do not return [d1] (even if [_n = _m])
            because of sharing in d1 **)
        Dur_max(d1,d2)
    (*| _,Dur_max(d3,d4) -> Dur_max(Dur_max(d1,d3),d4)*) (** left associative **)
    | _ -> Dur_max(d1,d2))
| Dur_xor(d1,d2) ->
    let d1,d2 = canon_dur d1,canon_dur d2 in
    (match d1,d2 with
    | Dur_top,_ | _,Dur_top -> Dur_top
    | Dur_int n,Dur_int m -> Dur_int (max n m)
    | Dur_int 0,d | d,Dur_int 0 -> d
    | Dur_var {contents=Unknown{id=n;name=_}},
      Dur_var {contents=Unknown{id=m;name=_}} ->
        if n = m then d1 else Dur_xor(d1,d2)
    (* | _,Dur_xor(d3,d4) -> Dur_xor(Dur_xor(d1,d3),d4)*) (** left associative **)
    | _ -> Dur_xor(d1,d2))
| Dur_add(d1,d2) ->
    let d1,d2 = canon_dur d1,canon_dur d2 in
    (match d1,d2 with
    | Dur_top,_ | _,Dur_top -> Dur_top
    | Dur_int 0,d | d,Dur_int 0 -> d
    | Dur_int m,Dur_int n -> Dur_int(m + n)
    (* | _,Dur_add(d3,d4) -> Dur_add(Dur_add(d1,d3),d4) *)(** left associative **)
    | Dur_mulDiv(sz,Dur_int 1,Sz_lit 1), Dur_int n
    (* ******** optimization *********)
    | Dur_int n, Dur_mulDiv(sz,Dur_int 1,Sz_lit 1) -> 
        Dur_mulDiv(Sz_add(sz,n),Dur_int 1,Sz_lit 1)
    (* *******************************)
    | Dur_int _,_ -> Dur_add(d2,d1)
    | _ -> Dur_add(d1,d2))
| Dur_mulDiv(sz1,d2,sz3) ->
    (* let sz1,d2,sz3 = canon_size sz1,canon_dur d2,canon_size sz3 in
    (match sz1,d2,sz3 with
     | _,Dur_top,_ -> Dur_top
     | Sz_lit 0,_,_ -> Dur_int 0
     | _,Dur_int 0,_ -> Dur_int 0
     | Sz_lit 1,_,Sz_lit 1 -> d2
     | Sz_lit n,Dur_int m,Sz_lit k -> 
          if n mod k = 0 then Dur_int ((n/k)*m) else Dur_int ((n/k+1)*m)
     | _ -> *)Dur_mulDiv(sz1,d2,sz3)
| Dur_shared(l,dx) ->
    Dur_shared(canon_label l,canon_dur dx)


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
| Ty_array(sz,tyB,l) ->
    Ty_array(canon_size sz, canon_tyB tyB, canon_label l)
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
  | Sz_pow2(sz) -> fprintf fmt "(2^%a)" pp sz
  in pp fmt sz

let rec pp_label fmt (l:label) : unit =
  let open Format in
  match l with
  | Label_var{contents=Is l} ->
      pp_label fmt l
  | Label_var{contents=Unknown{id=n;name}} ->
      (match name with
       | None -> fprintf fmt "&%d" n
       | Some x -> fprintf fmt "&%s%d" x n)
  | Label_name x -> fprintf fmt "%s" x

let pp_dur fmt (d:dur) : unit =
  let open Format in
  let rec pp fmt d =
  match canon_dur d with
  | Dur_var{contents=Unknown{id=n;name}} ->
      (match name with
      | None -> fprintf fmt "$%d" n
      | Some x -> fprintf fmt "$%s%d" x n)
  | Dur_var{contents=Is d} -> pp fmt d
  | Dur_int n -> fprintf fmt "%d" n
  | Dur_top -> fprintf fmt "⊤"
  | Dur_max(d1,d2) -> fprintf fmt "max(%a,%a)" pp d1 pp d2
  | Dur_xor(d1,d2) -> fprintf fmt "xor(%a,%a)" pp d1 pp d2
  | Dur_add(d1,d2) -> fprintf fmt "%a + %a" pp d1 pp d2
  | Dur_mulDiv(sz1,d2,sz3) -> 
       if sz3 = Sz_lit 1 then () else fprintf fmt "(";
      if d2 = Dur_int 1 then pp_size fmt sz1 else (
       fprintf fmt "%a * " pp_size sz1;
     (match d2 with
     | Dur_int _ | Dur_top -> pp fmt d2
     | _ -> fprintf fmt "(%a)" pp d2));
      if sz3 = Sz_lit 1 then () else fprintf fmt " / %a)" pp_size sz3
  | Dur_shared(l,dx) ->
                  fprintf fmt "{%a:%a}" pp_label l pp dx
  in pp fmt d

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
      | Dur_int(0) -> fprintf fmt "(%a => %a)" pp ty1 pp_tyB tyB2
      (* | Dur_top -> fprintf fmt "(%a -> %a)" pp ty1 pp_tyB tyB2 *)
      | Dur_top | Dur_int _ ->
          fprintf fmt "(%a -[%a]-> %a)" pp ty1 pp_dur d pp_tyB tyB2
      | _ ->
          if !show_wcet_exp_flag then
             fprintf fmt "(%a -%a-> %a)" pp ty1 
                Prelude.Errors.(emph_pp blue (fun fmt d -> Format.fprintf fmt "[%a]" pp_dur d)) d 
                pp_tyB tyB2
          else 
             fprintf fmt "(%a -> %a)" pp ty1 pp_tyB tyB2)
  | Ty_ref tyB -> fprintf fmt "%a ref" pp_tyB tyB
  | Ty_array(sz,tyB,l) ->
      fprintf fmt "%a array<%a>" pp_tyB tyB pp_size sz;
      if !show_wcet_exp_flag then
          Prelude.Errors.(emph_pp blue (fun fmt l -> Format.fprintf fmt "[%a]" pp_label l)) fmt l
  | Ty_signal(tyB) ->
      fprintf fmt "%a signal" pp_tyB tyB
  | Ty_trap(tyB) ->
      fprintf fmt "%a trap" pp_tyB tyB
  | Ty_size(sz) ->
      fprintf fmt "<<%a>>" pp_size sz
  | Ty_alias (x,sz_list,ty_list) ->
      pp_tyX_ident fmt pp x sz_list ty_list
  in pp fmt ty


let new_unknown : unit -> int =
  let n = ref 0 in
  fun () -> n := !n + 1; !n

let new_label_id : unit -> int =
  let n = ref max_int in
  fun () -> n := !n - 1; !n

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

let new_label_unknown ?name () =
  Label_var (ref (Unknown {id=new_label_id ();name}))


type kind = T | B | S | D | L

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
| Sz_pow2(sz) -> occ sz
in occ sz

let occur_label v l =
  let rec occ = function
  | Label_var {contents=Unknown{id=v';name}} ->
      if v = v' then raise Found
  | Label_var {contents=Is l'} -> occ l'
  | _ -> ()
  in occ l

let rec occur_dur v d =
let rec occ = function
| Dur_var {contents=Unknown{id=v';name=_}} ->
    if v = v' then raise Found
| Dur_var {contents=Is d} -> occ d
| Dur_int _ | Dur_top -> ()
| Dur_max(d1,d2) -> occ d1; occ d2
| Dur_xor(d1,d2) -> occ d1; occ d2
| Dur_add(d1,d2) -> occ d1; occ d2
| Dur_mulDiv(sz1,d2,sz3) -> occur_size v sz1; occ d2; occur_size v sz3
| Dur_shared(l,dx) -> occur_label v l; occ dx
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
  | Ty_array(sz,tyB,l) ->
      occur_size v sz;
      occur_tyB v tyB;
      occur_label v l
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
    | Sz_pow2(sz) -> vars s sz
  in vars s sz

let vars_of_label ?(s=Vs.empty) l =
  let rec vars s = function
  | Label_var {contents=Unknown u} ->
      Vs.add u L s
  | Label_var {contents=Is l'} -> vars s l'
  | _ -> s
  in vars s l

let vars_of_dur ?(s=Vs.empty) d =
  let rec vars s = function
    | Dur_var {contents=Unknown u} ->
        Vs.add u D s
    | Dur_var {contents=Is sz} -> vars s sz
    | Dur_int _ | Dur_top -> s
    | Dur_max(d1,d2) -> vars (vars s d1) d2
    | Dur_xor(d1,d2) -> vars (vars s d1) d2
    | Dur_add(d1,d2) -> vars (vars s d1) d2
    | Dur_mulDiv(sz1,d2,sz3) -> 
        let s = vars (vars_of_size ~s sz1) d2 in
        vars_of_size ~s sz3
    | Dur_shared(l,dx) -> vars (vars_of_label ~s l) dx
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
    | TyB_alias (_, sz_list, tyB_list) -> 
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
  | Ty_array (sz,tyB,l) ->
      let s1 = vars_of_size ~s sz in
      let s2 = vars_of_tyB ~s:s1 tyB in
      vars_of_label ~s:s2 l
  | Ty_signal (tyB) ->
      vars_of_tyB ~s:s tyB
  | Ty_trap (tyB) ->
      vars_of_tyB ~s:s tyB
  | Ty_size sz ->
      vars_of_size ~s sz
  | Ty_alias (x, sz_list, ty_list) -> 
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
  | Sz_pow2(sz) -> Sz_pow2(inst_size sz)
  in

  let rec inst_label = function
  | Label_var {contents=Unknown u} as l ->
          (try Label_var(find_unsafe unknowns u)
           with Not_found -> l)
  | Label_var {contents=Is l} ->
      inst_label l
  | l -> l
  in

  let rec inst_dur = function
  | Dur_var {contents=Unknown u} as d ->
          (try Dur_var (find_unsafe unknowns u)
           with Not_found -> d)
  | Dur_var {contents=Is d} ->
      inst_dur d
  | Dur_int _ | Dur_top as d -> d
  | Dur_max(d1,d2) ->
      Dur_max(inst_dur d1,inst_dur d2)
  | Dur_xor(d1,d2) ->
      Dur_xor(inst_dur d1,inst_dur d2)
  | Dur_add(d1,d2) ->
      Dur_add(inst_dur d1,inst_dur d2)
  | Dur_mulDiv(sz1,d2,sz3) ->
      Dur_mulDiv(inst_size sz1,inst_dur d2,inst_size sz3)
  | Dur_shared(l,dx) ->
      Dur_shared(inst_label l,inst_dur dx) 
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
  | Ty_array(sz,tyB,l) ->
      Ty_array(inst_size sz, inst_tyB tyB,inst_label l)
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
| Sz_pow2(sz) -> Sz_pow2(rename_size unknowns sz)

let rec rename_dur unknowns = function
| Dur_var {contents=Unknown u} as d ->
        (try Dur_var(find_unsafe unknowns u)
         with Not_found -> d)
| Dur_var ({contents=Is d} as _v) ->
    rename_dur unknowns d
| Dur_int _ | Dur_top as d -> d
| Dur_max(d1,d2) ->
    Dur_max(rename_dur unknowns d1,
            rename_dur unknowns d2) 
| Dur_xor(d1,d2) ->
    Dur_xor(rename_dur unknowns d1,
            rename_dur unknowns d2) 
| Dur_add(d1,d2) ->
    Dur_add(rename_dur unknowns d1,
            rename_dur unknowns d2) 
| Dur_mulDiv(sz1,d2,sz3) ->
    Dur_mulDiv(rename_size unknowns sz1,
               rename_dur unknowns d2,
               rename_size unknowns sz3) 
| Dur_shared(x,dx) ->
    Dur_shared(x,rename_dur unknowns dx)

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

let rec rename_label unknowns = function
| Label_var {contents=Unknown u} as l ->
        (try Label_var(find_unsafe unknowns u)
         with Not_found -> l)
| Label_var {contents=Is l'} ->
    rename_label unknowns l'
| l -> l 

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
| Ty_array(sz,tyB,l) ->
    Ty_array(rename_size unknowns sz, rename_tyB unknowns tyB, rename_label unknowns l)
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
| Sz_pow2(sz) -> Sz_pow2(subst_size vsize sz)

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

and subst_label ll = function
| Label_var {contents=Unknown u} as l ->
    (match IMap.find_opt u.id ll with
        | None -> l
        | Some l -> l)
    (*match u.name with
    | None -> l
    | Some x ->
       (match SMap.find_opt x ll with
        | None -> l
        | Some l -> l)*)
| l -> l

and subst_dur vsize dd ll = function
  | Dur_var {contents=Unknown u} as d ->
    (match u.name with
    | None -> d
    | Some x ->
       (match SMap.find_opt x dd with
        | None -> d
        | Some d' -> d'))
  | Dur_int _ | Dur_top as d -> d
  | Dur_var{contents=Is d'} -> subst_dur vsize dd ll d'
  | Dur_max(d1,d2) -> Dur_max(subst_dur vsize dd ll d1,subst_dur vsize dd ll d2)
  | Dur_xor(d1,d2) -> Dur_xor(subst_dur vsize dd ll d1,subst_dur vsize dd ll d2)
  | Dur_add(d1,d2) -> Dur_add(subst_dur vsize dd ll d1,subst_dur vsize dd ll d2)
  | Dur_mulDiv(sz,d2,sz') -> Dur_mulDiv(subst_size vsize sz,subst_dur vsize dd ll d2,subst_size vsize sz')
  | Dur_shared(l,d2) -> Dur_shared(subst_label ll l,subst_dur vsize dd ll d2)


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

and subst_ty vsize va dd ll = function
| Ty_var {contents=Unknown u} as ty ->
    (match u.name with
    | None -> ty
    | Some x ->
       (match SMap.find_opt x va with
        | None -> ty
        | Some ty' -> ty'))
| Ty_var {contents=Is ty} ->
    subst_ty vsize va dd ll ty
| Ty_base tyB ->
    Ty_base(subst_tyB vsize va tyB)
| Ty_tuple ty_list ->
    Ty_tuple(List.map (subst_ty vsize va dd ll) ty_list)
| Ty_fun(ty1,d,tyB2) ->
    Ty_fun(subst_ty vsize va dd ll ty1,subst_dur vsize dd ll d,subst_tyB vsize va tyB2)
| Ty_ref(tyB) ->
    Ty_ref(subst_tyB vsize va tyB)
| Ty_array(sz,tyB,l) ->
    Ty_array(subst_size vsize sz, subst_tyB vsize va tyB,subst_label ll l)
| Ty_signal(tyB) ->
    Ty_signal(subst_tyB vsize va tyB)
| Ty_trap(tyB) ->
    Ty_trap(subst_tyB vsize va tyB)
| Ty_size sz ->
    Ty_size(subst_size vsize sz)
| Ty_alias(x,sz_list,ty_list) ->
    Ty_alias (x,List.map (subst_size vsize) sz_list,List.map (subst_ty vsize va dd ll) ty_list)

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
      let ty'' = subst_ty sz_argvs' ty_argsvs' SMap.empty IMap.empty ty' in
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

let copy_dur d =
  match canon_ty @@ copy_ty (Ty_fun(Ty_base TyB_unit,d,TyB_unit)) with
  | Ty_fun(_,d',_) -> d'
  | _ -> assert false

let rebase scm = (* type variable renumbering from 1, 2, 3 ... *)
  let c = ref 0 in
  let new_unknown ?name () =
    let name = match name with
               | Some _ -> name
               | None -> Some "v" in
    (ref (Unknown{id=(incr c; !c);name})) in
  (generalize ~only_functions:false [] (instance ~new_unknown scm))

let rebase_ty ty =
  let (Forall(_,ty')) = rebase(generalize ~only_functions:false [] ty) in
  ty'

let rebase_duration d =
  match canon_ty @@ rebase_ty (Ty_fun(Ty_base TyB_unit,d,TyB_unit)) with
  | Ty_fun(_,d',_) -> d'
  | _ -> assert false

let rebase_tyB tyB =
  match rebase_ty (Ty_base tyB) with
  | Ty_base tyB' -> tyB'
  | _ -> assert false 

let pp_scheme fmt scm =
  let (Forall(vs0,_)) = scm in
  let (Forall(vs,ty)) = rebase scm in
  let open Format in 
  if Vs.cardinal vs0 > 0 then (
  Prelude.Errors.(emph bold fmt "forall ");
  let pp_u fmt u = 
    match u.name with
    | None -> fprintf fmt "%d " u.id
    | Some s -> fprintf fmt "%s%d " s u.id
  in
  Vs.iter (fun u k ->
             (match k with
             | T -> fprintf fmt "'%a" pp_u u
             | B -> fprintf fmt "~%a" pp_u u
             | S -> fprintf fmt "?%a" pp_u u
             | D -> fprintf fmt "$%a" pp_u u
             | L -> if !show_wcet_exp_flag then (
          Prelude.Errors.(emph_pp blue (fun fmt u -> Format.fprintf fmt "&%a" pp_u u)) fmt u))) vs;
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
    | Sz_pow2(sz) -> 2 lsl (aux sz - 1)
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
    | Ty_array(_,tyB,_)
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
  | Ty_array(sz,tyB,l) ->
      Ty_array(sz, remove_alias_tyB tyB,l)
  | Ty_signal(tyB) ->
      Ty_signal(remove_alias_tyB tyB)
  | Ty_trap(tyB) ->
      Ty_trap(remove_alias_tyB tyB)
  | Ty_size _ -> ty
  | Ty_alias(x,sz_list,ty_list) ->
      remove_alias_ty @@
      alias_instance x sz_list (List.map remove_alias_ty ty_list)

let sum_seq m_seq = 
  SMap.fold (fun x d s -> Dur_add(d,s)) m_seq (Dur_int 0)

let rec accesses = function
| Dur_var {contents=Is d} -> accesses d
| Dur_var {contents=Unknown _}
| Dur_int _ 
| Dur_top -> SMap.empty 
| Dur_shared(l,dx) ->
    (match canon_label l with 
    | Label_name x -> SMap.singleton x dx
    | _ -> assert false)
| Dur_max(d1,d2)
| Dur_add(d1,d2) -> SMap.union (fun _ d1 d2 -> Some (Dur_add(d1,d2))) (accesses d1) (accesses d2)
| Dur_xor(d1,d2) -> SMap.union (fun _ d1 d2 -> Some (Dur_max(d1,d2))) (accesses d1) (accesses d2)
| Dur_mulDiv(sz,d,_) -> accesses d |> SMap.map (fun di -> Dur_mulDiv(sz,di,Sz_lit 1))

(** [contains_label_variables d] returns [true] if duration [d] contains
    a shared resource {&v:d} where [&v] is a label variable *)
let contains_label_variables d =
  let exception Find in
  let rec aux = function
  | Dur_var {contents=Unknown _} ->
      raise Find (* conservative: this unknown may contain a label *)
  | Dur_int _ 
  | Dur_top -> ()
  | Dur_var {contents=Is d} -> aux d
  | Dur_shared(l,dl) -> (match canon_label l with
                         | Label_var _ -> raise Find
                         | _ -> aux dl)
  | Dur_max(d1,d2)
  | Dur_xor(d1,d2)
  | Dur_add(d1,d2) -> aux d1; aux d2
  | Dur_mulDiv(_,d',_) -> aux d'
  in try aux d;false with Find -> true 

(** [sequentialize_accesses_in_dur ~xs d] requires that [contains_label_variables d] is [true] *)
let rec sequentialize_accesses_in_dur ?(xs=SMap.empty) = function
| Dur_var {contents=Unknown _}
| Dur_int _ 
| Dur_top as d -> d
| Dur_var {contents=Is d} -> sequentialize_accesses_in_dur ~xs d
| Dur_shared(l,dx) -> 
    (match canon_label l with 
     | Label_name x -> if SMap.mem x xs then Dur_int 0 else dx
     | _ -> assert false)
| Dur_max(d1,d2) ->
    let a1 = accesses d1 in
    let a2 = accesses d2 in
    let a_inter = SMap.filter_map (fun x d -> 
      if SMap.mem x a2 then (Some (Dur_add(d,SMap.find x a2))) else None) a1 in
    let a_inter' = SMap.union (fun _ _ d -> Some d) a_inter xs in
    let d1' = sequentialize_accesses_in_dur ~xs:a_inter' d1 in
    let d2' = sequentialize_accesses_in_dur ~xs:a_inter' d2 in
    let n = sum_seq @@ SMap.filter_map (fun x d -> if SMap.mem x xs then None else Some d) a_inter in
    Dur_add(Dur_max(d1',d2'),n)
| Dur_xor(d1,d2) ->
    let a1 = accesses d1 in
    let a2 = accesses d2 in
    let a_inter = SMap.filter_map (fun x d -> 
      if SMap.mem x a2 then (Some (Dur_max(d,SMap.find x a2))) else None) a1 in
    let a_inter' = SMap.union (fun _ _ d -> Some d) a_inter xs in
    let n = sum_seq @@ SMap.filter_map (fun x d -> if SMap.mem x xs then None else Some d) a_inter in
    let d1' = sequentialize_accesses_in_dur ~xs:a_inter' d1 in
    let d2' = sequentialize_accesses_in_dur ~xs:a_inter' d2 in
    Dur_add(Dur_xor(d1',d2'),n)
| Dur_add(d1,d2) ->
    let d1' = sequentialize_accesses_in_dur ~xs d1 in
    let d2' = sequentialize_accesses_in_dur ~xs d2 in
    Dur_add(d1',d2')
| Dur_mulDiv(sz,d1,sz3) ->
    let a1 = accesses d1 in
    let a1' = SMap.union (fun _ _ d -> Some d) a1 xs in
    let d1' = sequentialize_accesses_in_dur ~xs:a1' d1 in
    let n = sum_seq @@ SMap.map (fun d -> Dur_mulDiv(sz,d,Sz_lit 1)) a1 in
    Dur_add(Dur_mulDiv(sz,d1',sz3),n)


(** [simpl_dur_wcet ~keep_sharing d] 
    symbolically evaluates the duration [d] 
    assuming that [contains_label_variables d] is [true] *)
let rec simpl_dur_wcet ?(keep_sharing=false) d =
  d |> function
| Dur_var {contents=Unknown _}
| Dur_int _
| Dur_top as d -> d 
| Dur_var {contents=Is d} -> simpl_dur_wcet ~keep_sharing d
| Dur_shared(l,dy) ->
    (match l with
    | Label_name y ->
        if not(keep_sharing) && y <> "%" then simpl_dur_wcet ~keep_sharing dy
        else Dur_shared(l,(* simpl_dur_wcet ~keep_sharing*) dy)
    | _ -> assert false)
| Dur_add(d1,d2) ->
    (match simpl_dur_wcet ~keep_sharing d1,simpl_dur_wcet ~keep_sharing d2 with
    | Dur_top,_ | _,Dur_top -> Dur_top
    | Dur_int n,Dur_int m -> Dur_int(n + m)
    | d1',d2' -> Dur_add(d1',d2'))
| Dur_max(d1,d2) ->
    (match simpl_dur_wcet ~keep_sharing d1, simpl_dur_wcet ~keep_sharing d2 with
    | Dur_top,_ | _,Dur_top -> Dur_top
    | Dur_int n,Dur_int m -> Dur_int(max n m)
    | d1',d2' -> Dur_max(d1',d2'))
| Dur_xor(d1,d2) ->
    (match simpl_dur_wcet ~keep_sharing d1,simpl_dur_wcet ~keep_sharing d2 with
    | Dur_top,_ | _,Dur_top -> Dur_top
    | Dur_int n,Dur_int m -> Dur_int(max n m)
    | d1',d2' -> Dur_xor(d1',d2'))
| Dur_mulDiv(sz2,d1,sz3) ->
    (match (simpl_dur_wcet ~keep_sharing @@ canon_dur d1), canon_size sz2,canon_size sz3 with
     | Dur_int n, Sz_lit m, Sz_lit k -> 
        if m mod k = 0 then Dur_int(n*(m/k)) else Dur_int(n*((m/k)+k-1))
     | d1',sz',sz2' -> Dur_mulDiv(sz',d1',sz2'))

(** [simpl_dur_wcet_with_rebase ~keep_sharing d] 
    symbolically evaluates any duration [d] *)
let simpl_dur_wcet_with_rebase ?(keep_sharing=false) d =
  if contains_label_variables d then d else (
    let d' = sequentialize_accesses_in_dur d in
    simpl_dur_wcet ~keep_sharing d'
  )

let labels_in_d d =
  let rec aux s = function
  | Dur_var {contents=Unknown _}
  | Dur_int _ 
  | Dur_top -> s
  | Dur_var {contents=Is d} -> aux s d
  | Dur_shared(l,dl) ->
     (match canon_label l with
     | Label_name x ->
        let s1 = SMap.add x () s in 
        aux s1 dl
     | _ -> aux s dl)
  | Dur_max(d1,d2)
  | Dur_xor(d1,d2)
  | Dur_add(d1,d2) -> let s1 = aux s d1 in
                      aux s1 d2
  | Dur_mulDiv(_,d',_) -> aux s d'
  in aux SMap.empty d

let var_labels_in_d d =
  let rec aux s = function
  | Dur_var {contents=Unknown _} -> s
  | Dur_int _ 
  | Dur_top -> s
  | Dur_var {contents=Is d} -> aux s d
  | Dur_shared(l,dl) -> 
     (match canon_label l with 
      | Label_name _ -> s
      | Label_var {contents=Is _} -> s
      | Label_var {contents=Unknown u} ->
          let s1 = Vs.add u () s in 
          aux s1 dl)
  | Dur_max(d1,d2)
  | Dur_xor(d1,d2)
  | Dur_add(d1,d2) -> let s1 = aux s d1 in
                      aux s1 d2
  | Dur_mulDiv(_,d',_) -> aux s d'
  in aux Vs.empty d

let size_var_in_d d =
  let rec aux s = function
  | Dur_var {contents=Unknown _} -> s
  | Dur_int _ 
  | Dur_top -> s
  | Dur_var {contents=Is d} -> aux s d
  | Dur_shared(_,dl) -> 
     aux s dl
  | Dur_max(d1,d2)
  | Dur_xor(d1,d2)
  | Dur_add(d1,d2) -> let s1 = aux s d1 in
                      aux s1 d2
  | Dur_mulDiv(sz1,d',sz2) ->
      let s1 = vars_of_size ~s sz1 in
      let s2 = vars_of_size ~s:s1 sz2 in
      aux s2 d'
  in aux Vs.empty d


module Check_durations = struct

  let rec assign us vs =
    match vs with
    | [] -> [[]]
    | v::vs ->
        let rest_assignments = assign us vs in
        List.concat (
          List.map (fun u ->
            List.map (fun rest -> (u,v)::rest) rest_assignments
          ) us
        )

  let () = Random.self_init ();;

  let random_size_for_duration_checking() =
    Sz_lit(Random.int 10 + 42)

  (** to check that [d1 <= d2], we instantiate [d1] and [d2] with all possible assignements
      of locations (labels) among those occurring in [d1] and [d2].
      This is sufficient to check all possibilities, as long as their is no size variables in
      [d1] or [d2]. If size variables occur, these are instantiated with random size
      given by [random_size_for_duration_checking()], allowing to find counter-examples;
      then, a warning is emitted to show the proof obligation (assumed to be true). **)
  let check f d1 d2 =
     let vs = List.map fst @@ SMap.bindings @@ labels_in_d d1 in
     let vs = if vs = [] then ["%l"] else vs in
     let us = List.map fst @@ Vs.bindings @@ var_labels_in_d d1 in
     let aa = assign vs us in
     
     let szs = Vs.union (fun _ v _ -> Some v) (size_var_in_d d1) (size_var_in_d d2) in
     List.iter (fun a ->
        
        let vsize = Vs.fold (fun {name} _ acc -> 
                            match name with 
                            | None -> acc
                            | Some x -> SMap.add x (random_size_for_duration_checking()) acc
                          ) szs  SMap.empty in

        let ll = List.fold_left (fun acc (x,v) -> IMap.add v.id (Label_name x) acc) IMap.empty a in
        let d1' = subst_dur vsize SMap.empty ll d1 in
        let d2' = subst_dur vsize SMap.empty ll d2 in
        f d1' d2') aa;

     let ok = (Vs.cardinal szs = 0) in
     ok
end    