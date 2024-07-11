open Fsm_syntax
open Format

let ram_inference = ref false
let memory_initialization = ref false
let intel_max10_target = ref false
let intel_xilinx_target = ref false
let single_read_write_lock_flag = ref true

let mealy_flag = ref true


let size_ty t =
  (* we must canonising [t] to prevent it from being considered as a type variable *)
  Fsm_typing.(size_ty (canon t))

(** [size_const c] returns the number of bits of constant [c] *)
let rec size_const c =
  match c with
  | Unit | Bool _ ->
      1
  | Int {value=_;tsize} ->
      size_ty tsize
  | Enum _ ->
      assert false (* cannot infer enum size *)
  | CTuple cs | CVector cs ->
      List.fold_left (fun s c -> s + size_const c) 0 cs
  | CSize _ -> 
      Prelude.Errors.warning (fun fmt ->
        Format.fprintf fmt
          "Size literal detected in the generated code: this is not an immediate value");
      1 (* or 0 ? *)
  | String s -> String.length s * 8
  | C_encode(_,n) ->
      n

(* [reserved x] returns [true] iff [x] is a VHDL keyword
   or a reserved identifier (e.g., reset) *)
let reserved : string -> bool =
  let tbl = Hashtbl.create 20 in
  let () =
    List.iter (fun x -> Hashtbl.add tbl x ()) @@
      [ (*"result"; "argument"; *)"_"; "reset"; "others" ; "value"; "clk"; "loop"; "exit";"next";"rdy";"wait"]
      (* todo: complete with other VHDL keywords *)
  in
  (fun x -> Hashtbl.mem tbl x)

(** [norm_ident x] convert [x] to a valid VHDL identifier *)
let norm_ident x =
  let is_azAZ_19 c =
    (c >= '0' && c <= '9') ||
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') || c = '_'
  in
  let exception E in
  try
    if reserved x || (String.length x > 0 && x.[0] = '_') then raise E;
    String.iter (fun c -> if not (is_azAZ_19 c) then raise E) x;
    x
  with E -> "\\" ^ x ^"\\" (* extended VHDL identifier: anything between backslash *)

(** code generator for identifiers *)
let pp_ident fmt (x:x) : unit =
    fprintf fmt "%s" (norm_ident x)

let pp_state fmt (x:x) : unit =
    pp_ident fmt (String.uppercase_ascii x)

let const_zero nbits =
  let bits_in_hexa = nbits / 4 in
    let bits_in_binary = nbits mod 4 in
    let make ?(hexa=false) n =
      "\""^(String.make n '0')^"\""
    in
    match bits_in_binary,bits_in_hexa  with
    | 0,n -> "X"^make n
    | n,0 -> make n
    | n,m -> make n^"& X"^make m


let int2bin ~int_size =
  (* from https://discuss.ocaml.org/t/pretty-printing-binary-ints/9062 *)
  let buf = Bytes.create int_size in
  fun n ->
    for i = 0 to int_size - 1 do
      let pos = int_size - 1 - i in
      Bytes.set buf pos (if i <= 63 && (n land (1 lsl i) != 0) then '1' else '0') (* I add i <= 63 to avoid incorrect value due to overflow *)
    done;
    Bytes.to_string buf

(* lock-based support for concurrent memory accesses *)
let ptr_taken x = "$"^x^"_lock" 

let decl_locks ?(init=false) fmt x =
  (* if !single_read_write_lock_flag then *)
    fprintf fmt "variable %a : value(0 to 0)%s;@," pp_ident (ptr_taken x)
      (if init then " := (others => '0')" else "")
(* ******************************** *)

let pp_tuple fmt pp vs =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " & ") pp fmt vs

let pp_vector fmt pp vs =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " & ") pp fmt vs


(** code generator for constants *)
let rec pp_c fmt c =
  match c with
  | Unit -> fprintf fmt "eclat_unit"
  | Int {value=n;tsize} ->
      let is_neg = n < 0 in
      let n = abs n in
      let sz = size_ty tsize in
      if is_neg then fprintf fmt "eclat_neg(";
      if sz < 16 then
        fprintf fmt "\"%s\"" (int2bin ~int_size:sz n)
      else
        (let v = Printf.sprintf "%x" n in (* dislay in hexa directly *)
         let l_pad = sz - String.length v * 4 in
         if l_pad = 0 then fprintf fmt "X\"%s\"" v else
         fprintf fmt "%s & X\"%s\"" (const_zero l_pad) v);
      if is_neg then fprintf fmt ")";
  | Bool b ->
      (* notice: in VHDL, eclat_true(0) is valid, but "1"(0) is invalid. *)
      fprintf fmt "%s" (if b then "eclat_true" else "eclat_false")
  | Enum x -> pp_ident fmt x
  | CTuple(cs) ->
       pp_tuple fmt pp_c cs
  | CVector(cs) ->
      pp_vector fmt pp_c cs
  | String s -> fprintf fmt "of_string(\"%s\")" s
  | CSize n -> fprintf fmt "%d" n
  | C_encode(c,n) ->
      fprintf fmt "%a & %s" pp_c c (const_zero (n - size_const c))

(** code generator for tuples deconstruction *)
let rec pp_tuple_access fmt (i:int) ty (a:a) : unit =

  let rec tuple_access i ty_a a =
      (* compute bounds of the value to be accessed at index [i_to_find]
         among projections of types ts *)
    let slice_bounds i_to_find ts =
      let rec aux j acc = function
      | [] -> assert false
      | t::ts' -> if i_to_find = j then acc,t,ts' else aux (j+1) (t::acc) ts' in
      aux 0 [] ts
      in
      let open Fsm_typing in
      match a,canon ty_a with
      | A_tuple aas,TTuple ts ->
          `Atom(List.nth aas i)
      | A_call(GetTuple(j,_,ty2),a2),TTuple ts ->
          let ts_before,t,_ = slice_bounds i ts in
          (match tuple_access j ty2 a2 with
          | `Slice(a,i1,i2) ->
                let z = size_ty (TTuple ts_before) in
                let tz = size_ty t in
                `Slice(a,z+i1,z+i1+tz-1)
          | `Atom _ as a -> a)
      | _,TTuple ts ->
            let ts_before,t,ts_after = slice_bounds i ts in
            let z' = size_ty (TTuple ts_after) in
            let tot = size_ty (TTuple ts) in
            let j = tot-z'-size_ty t in
            let k = tot - z'-1 in
            `Slice(a,j,k)
      | _,ty2 -> Printf.printf "---> %s\n" (string_of_ty ty_a) ;
                assert false
  in

  match tuple_access i ty a with
  | `Slice(a,j,k) ->
      if j = k then
        (* this case is use to avoid a strange failure (a GHDL bug ?)
           during simulation (overflow detected)
           when using slice x(j to k) of size 1 (i.e., j = k) *)
        fprintf fmt "\"\"&%a(%d)" pp_a a j
      else
      let pp_slice fmt (j,k) =
        fprintf fmt "%d to %d" j k
      in
      fprintf fmt "%a(%a)" pp_a a pp_slice (j,k)
  | `Atom(a) -> pp_a fmt a


(** code generator for call of operator *)
and pp_call fmt (op,a) =
  match op with
  | GetTuple(i,_,ty) -> pp_tuple_access fmt i ty a
  | Runtime(Size_of_val(ty,size_int)) -> 
     let n = size_ty (Fsm_typing.translate_ty ty) in
     pp_c fmt (Int{value=n;tsize=Fsm_typing.translate_size size_int})
  | Runtime(Vector_make) ->
      (match a with
      | A_tuple[a1; a2] -> fprintf fmt "eclat_vector_make(%a,%a)" pp_a a1 pp_a a2
      | _ -> assert false)
  | Runtime(Vector_length (sz,sz_res)) ->
      (match Types.canon_size sz with
      | Sz_lit n -> pp_c fmt (Int {value=n;tsize=(Fsm_typing.translate_size sz_res)})
      | _ -> Types.pp_size Format.std_formatter (Types.canon_size sz); assert false)
   | Runtime(Vector_get t) ->
      fprintf fmt "eclat_vector_get(%a,%d)" pp_a a Fsm_typing.(size_ty (translate_tyB t))
   | Runtime(Vector_update t) ->
      fprintf fmt "eclat_vector_update(%a,%d)" pp_a a Fsm_typing.(size_ty (translate_size t))
  | Runtime p -> Operators.gen_op fmt p pp_a a
  | _ -> fprintf fmt "@[%a(%a)@]" pp_op op pp_a a

(** code generator for operator *)
and pp_op fmt = function
| If -> fprintf fmt "eclat_if"
| Runtime p -> assert false (* deal with in pp_call*)
| TyConstr _ -> fprintf fmt "eclat_id"
| GetTuple (i,_,_) -> assert false (* special case, defined below (see tuple_access) *)

(** code generator for atoms (i.e. combinatorial expression) *)
(* assumes that the let-bindings of atoms are not nested *)
and pp_a fmt = function
| A_const c -> pp_c fmt c
| A_var x -> fprintf fmt "%a" pp_ident x
| A_call(op,a) ->
   pp_call fmt (op,a)
| A_letIn(x,a1,a2) ->
   assert false (* flattening needed before *) (* fprintf fmt "@[%a := %a;@,%a@]" pp_ident x pp_a a1 pp_a a2*)
| A_tuple aas -> pp_tuple fmt pp_a aas
| A_vector aas -> pp_vector fmt pp_a aas
| A_string_get(s,i) ->
    fprintf fmt "@[%a(to_integer(unsigned(%s&\"000\")) to to_integer(unsigned(%s&\"000\"))+7)@]" pp_ident s i i
| A_buffer_get(xb) ->
    pp_ident fmt ("$"^xb^"_value")
| A_ptr_taken(x) ->
    pp_ident fmt (ptr_taken x)
| A_ptr_write_taken(x) ->
    pp_ident fmt (ptr_taken x)
| A_buffer_length(x,tz) ->
    fprintf fmt  "std_logic_vector(to_unsigned(%a'length,%d))" pp_ident x (size_ty tz)
| A_encode(y,ty,n) ->
   fprintf fmt "%a%s" pp_ident y (let m = size_ty ty in if n = m then "" else "&"^const_zero (n-m))
| A_decode(y,ty) ->
   fprintf fmt "%a(0 to %d)" pp_ident y (size_ty ty - 1)

