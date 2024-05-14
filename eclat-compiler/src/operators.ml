
let flag_no_assert = ref false
let flag_no_print = ref false


(* instantaneous primitives which do not require an encoding *)
type op =
  | Add | Sub | Mult | Div | Mod
  | Lt | Le | Gt | Ge | Eq | Neq
  | And | Or | Xor | Not | Abs
  | Land | Lor | Lxor | Lsl | Lsr | Asr

  | Resize_int of int
  | Tuple_of_int of int
  | Int_of_tuple of int
  | Size_of_val of Types.ty * Types.ty 
  | String_length

  | GetBit
  | UpdateBit

  | Unroll of int (* experimental *)

  | Vector_make
  | Vector_length of Types.ty * Types.ty
  | Vector_get of Types.ty
  | Vector_update of Types.ty

  (* for simulation only *)
  | Print | Print_string | Print_int | Print_newline | Assert


let combinational p =
  match p with
  | Print | Print_string | Print_int | Print_newline | Assert -> false
  | _ -> true

let ty_op op =
  let open Types in
  match op with
  | Abs ->
      let sz = unknown() in
      let t = tint sz in
      t ==> t
  | Add|Sub|Mult|Div|Mod|Land|Lor|Lxor|Lsl|Lsr|Asr ->
      let sz = unknown() in
      let t = tint sz in
      (T_tuple[t;t]) ==> t
  | Lt|Gt|Le|Ge|Eq|Neq ->
      let sz = unknown() in
      let t = tint sz in
      (T_tuple[t;t]) ==> tbool
  | Not ->
      tbool ==> tbool
  | And|Or|Xor ->
      (T_tuple[tbool;tbool]) ==> tbool
  | Resize_int k ->
      let sz = unknown() in
      (tint sz) ==> tint (T_size k)
  | Tuple_of_int k ->
      let t = T_tuple (List.init k (fun _ -> tbool)) in
      tint (T_size k) ==> t
  | Int_of_tuple k ->
      let t = T_tuple (List.init k (fun _ -> tbool)) in
      t ==> tint (T_size k)
  | GetBit ->
      let sz = unknown() in
      let t = tint sz in
      (T_tuple [t;tint (T_size(32))]) ==> tbool
  | UpdateBit ->
      let sz = unknown() in
      let t = tint sz in
      (T_tuple [t;tint (T_size(32));tbool]) ==> t
  | Unroll _ ->
      let v = unknown() in
      let d = unknown() in
      let v' = unknown() in
      fun_ty (T_tuple[fun_ty v d v'; v]) d v'
  | Vector_make ->
      let v = unknown() in
      let v' = unknown() in
      (T_tuple[v; v']) ==> T_vector{size=v;elem=v'}
  | Vector_length (sz,sz_res) ->
      let v = unknown() in
      (T_vector{size=sz;elem=v}) ==> tint sz_res
  | Vector_get v ->
      let sz = unknown() in
      (T_tuple[T_vector{size=sz;elem=v}; tint (T_size 32)]) ==> v
  | Vector_update sz ->
      let v = unknown() in
      let t = T_vector{size=sz;elem=v} in
      (T_tuple[t; tint (T_size 32);v]) ==> t
  | Size_of_val (ty,tsz) ->
      ty ==> tint tsz
  | Print ->
      (unknown()) ==> tunit
  | Print_string ->
      (T_string (unknown())) ==> tunit
  | Print_int ->
      (tint (unknown())) ==> tunit
  | Print_newline ->
      tunit ==> tunit
  | Assert ->
      tbool ==> tunit
  | String_length ->
      (* enforce result to be a 16-bit integer *)
      let tz_int = T_size 16 in
      (T_string(unknown ())) ==> (tint tz_int)


(* improved typing with level of types *)
let ty_op2 op =
  let open Types.Ty in
  match op with
  | Abs ->
      let sz = new_size_unknown() in
      let tyB = TyB_int sz in
      Ty_fun(Ty_base tyB,Dur_zero,tyB)
  | Add|Sub|Mult|Div|Mod|Land|Lor|Lxor|Lsl|Lsr|Asr ->
      let sz = new_size_unknown() in
      let tyB = TyB_int sz in
      Ty_fun(Ty_base (TyB_tuple[tyB;tyB]),Dur_zero,tyB)
  | Lt|Gt|Le|Ge|Eq|Neq ->
      let sz = new_size_unknown() in
      let tyB = TyB_int sz in
      Ty_fun(Ty_base (TyB_tuple[tyB;tyB]),Dur_zero,TyB_bool)
  | Not ->
      Ty_fun(Ty_base TyB_bool, Dur_zero, TyB_bool)
  | And|Or|Xor ->
      Ty_fun(Ty_base (TyB_tuple[TyB_bool;TyB_bool]), Dur_zero, TyB_bool)
  | Resize_int k ->
      let sz = new_size_unknown() in
      let tyB = TyB_int sz in
      let tyB_k = TyB_int (Sz_lit k) in
      Ty_fun(Ty_base tyB,Dur_zero,tyB_k)
  | Tuple_of_int k ->
      let tyB = TyB_tuple (List.init k (fun _ -> TyB_bool)) in
      Ty_fun(Ty_base (TyB_int (Sz_lit k)),Dur_zero,tyB)
  | Int_of_tuple k ->
      let tyB = TyB_tuple (List.init k (fun _ -> TyB_bool)) in
      Ty_fun(Ty_base tyB,Dur_zero, TyB_int (Sz_lit k))
  | GetBit ->
      let sz = new_size_unknown() in
      let tyB = TyB_int sz in
      Ty_fun(Ty_base(TyB_tuple[tyB;TyB_int (Sz_lit(32))]),Dur_zero,TyB_bool)
  | UpdateBit ->
      let sz = new_size_unknown() in
      let tyB = TyB_int sz in
      Ty_fun(Ty_base(TyB_tuple[tyB;TyB_int (Sz_lit(32));TyB_bool]),Dur_zero,tyB)
  | Unroll _ ->
      let v = new_ty_unknown() in
      let d = new_dur_unknown() in
      let v' = new_tyB_unknown() in
      Ty_fun((Ty_tuple[Ty_fun(v,d,v'); v]),d,v')
  | Print ->
      let tyB = new_tyB_unknown() in
      Ty_fun(Ty_base tyB,Dur_zero,TyB_unit)
  | Print_string ->
      let sz = new_size_unknown() in
      Ty_fun(Ty_base (TyB_string sz),Dur_zero,TyB_unit)
  | Print_int ->
      let sz = new_size_unknown() in
      Ty_fun(Ty_base (TyB_int sz),Dur_zero,TyB_unit)
  | Print_newline ->
      Ty_fun(Ty_base TyB_unit,Dur_zero,TyB_unit)
  | Assert ->
      Ty_fun(Ty_base TyB_bool,Dur_zero,TyB_unit)
  | Vector_make ->
      let sz = new_size_unknown() in
      let v = new_tyB_unknown() in
      Ty_fun(Ty_base (TyB_tuple[TyB_size sz;v]),Dur_zero,TyB_vector(sz,v))
  | Vector_length (_sz,_sz_res) ->
      let sz = new_size_unknown() in (* todo: unify sz and _sz, iem for _sz_res *)
      let v = new_tyB_unknown() in
      let sz_int = new_size_unknown() in
      Ty_fun(Ty_base(TyB_vector(sz,v)),Dur_zero,TyB_int sz_int)
  | Vector_get _ ->
      let sz = new_size_unknown() in
      let v = new_tyB_unknown() in
      Ty_fun(Ty_base (TyB_tuple[TyB_vector(sz,v);
                                TyB_int (Sz_lit 32)]),Dur_zero,v)
  | Vector_update _ ->
      let sz = new_size_unknown() in
      let v = new_tyB_unknown() in
      let t = TyB_vector(sz,v) in
      Ty_fun(Ty_base (TyB_tuple[t;TyB_int (Sz_lit 32);v]),Dur_zero,t)
  | Size_of_val _ ->
      Ty_fun(new_ty_unknown(),Dur_zero,TyB_int (new_size_unknown()))
  | String_length ->
      (* enforce result to be a 16-bit integer *)
      let sz = new_size_unknown() in
      Ty_fun(Ty_base (TyB_string(sz)),Dur_zero,TyB_int (Sz_lit 16))


(** pretty printer for operators *)
let pp_op fmt (op:op) : unit =
  Format.fprintf fmt "%s" @@
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Le -> "<="
  | Lt -> "<"
  | Ge -> ">="
  | Gt -> ">"
  | Eq -> "=="
  | Neq -> "<>"
  | Not -> "not"
  | Abs -> "abs"
  | And -> "&&"
  | Or -> "or"
  | Mod -> "mod"
  | Div -> "/"
  | Xor  -> "xor"
  | Land -> "land"
  | Lor -> "lor"
  | Lxor -> "lxor"
  | Lsl -> "lsl"
  | Lsr -> "lsr"
  | Asr -> "asr"
  | Resize_int k -> "resize_int<" ^ string_of_int k ^ ">"
  | Tuple_of_int k -> "tuple_of_int<" ^ string_of_int k ^ ">"
  | Int_of_tuple k -> "int_of_tuple<" ^ string_of_int k ^ ">"
  | GetBit -> "get_bit"
  | UpdateBit -> "update_bit"
  | Vector_make -> "vector_make"
  | Vector_length _ -> "vector_length"
  | Vector_get _ -> "vector_get"
  | Vector_update _ -> "vector_update"
  | Unroll n -> "unroll" ^ string_of_int n ^ ">"
  | Size_of_val _ -> "size_of_val"
  | Print -> "print"
  | Print_string -> "print_string"
  | Print_int -> "print_int"
  | Print_newline -> "print_newline"
  | Assert -> "assert"
  | String_length -> "string_length"



(** code generator for operators *)
let gen_op fmt (op:op) pp a : unit =
  let open Format in
  let funcall fmt s = fprintf fmt "%s(%a)" s pp a in
  let procall fmt s = fprintf fmt "%s(%a)" s pp a in
  let skip_when b fmt f s =
    if b then fprintf fmt "eclat_skip(eclat_unit)"
    else f fmt s 
  in
  match op with
  | Add -> funcall fmt "eclat_add"
  | Sub -> funcall fmt "eclat_sub"
  | Mult -> funcall fmt "eclat_mult"
  | Eq -> funcall fmt "eclat_eq"
  | Neq -> funcall fmt "eclat_neq"
  | Lt ->  funcall fmt "eclat_lt"
  | Le -> funcall fmt "eclat_le"
  | Gt -> funcall fmt "eclat_gt"
  | Ge -> funcall fmt "eclat_ge"
  | And -> funcall fmt "eclat_and"
  | Or -> funcall fmt "eclat_or"
  | Xor -> funcall fmt "eclat_xor"
  | Not -> funcall fmt "eclat_not"
  | Abs -> funcall fmt "eclat_abs"
  | Div -> funcall fmt "eclat_div"
  | Mod -> funcall fmt "eclat_mod"
  | Land -> funcall fmt "eclat_land"
  | Lor -> funcall fmt "eclat_lor"
  | Lxor -> funcall fmt "eclat_lxor"
  | Lsl -> funcall fmt "eclat_lsl"
  | Lsr -> funcall fmt "eclat_lsr"
  | Asr -> funcall fmt "eclat_asr"
  | Resize_int k ->
      fprintf fmt "eclat_resize(%a,%d)" pp a k
  | Tuple_of_int _ ->
      pp fmt a
  | Int_of_tuple _ ->
      pp fmt a
  | GetBit -> 
      funcall fmt "eclat_getBit"
  | UpdateBit -> 
      funcall fmt "eclat_updateBit"
  | Unroll _ -> assert false (* should be eliminated before *)
  | Vector_make ->
      assert false (* special case *)
  | Vector_length _ -> 
      assert false (* special case *)
  | Vector_get _ ->
     assert false (* special case *)
  | Vector_update _ ->
      assert false (* special case *)
  | Size_of_val _ ->
      assert false (* special case *)
  | Print ->
      skip_when !flag_no_print fmt procall "eclat_print"
  | Print_string ->
      skip_when !flag_no_print fmt procall "eclat_print_string"
  | Print_int ->
      skip_when !flag_no_print fmt procall "eclat_print_int"
  | Print_newline ->
      skip_when !flag_no_print fmt procall "eclat_print_newline"
  | Assert ->
      skip_when !flag_no_assert fmt (fun fmt () -> 
          fprintf fmt 
              "assert %a = eclat_true report \"assertion failed\" severity error"
              pp a) ()
  | String_length ->
      procall fmt "eclat_string_length"
