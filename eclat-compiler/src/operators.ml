
let flag_no_assert = ref false
let flag_no_print = ref false


(* instantaneous primitives which do not require an encoding *)
type op =
  | Resize_int of Types.size
  | Tuple_of_int of int
  | Int_of_tuple of int
  | Size_of_val of Types.ty * Types.size  (* ty * size_int *)
  | String_length

  | GetBit
  | UpdateBit

  | Unroll of int (* experimental *)

  | Vector_create of Types.size

(*  | Vector_length of Types.size * Types.size  (* size * size_int *)
  | Vector_get of Types.tyB
  | Vector_update of Types.size*)

  (* for simulation only *)
  | Print | Print_string | Print_int | Print_newline | Assert

  | Bvect_of_int
  | Int_of_bvect

  | External_fun of string * Types.ty

let combinational ~externals p =
  match p with
  | Print | Print_string | Print_int | Print_newline | Assert -> false
  | External_fun (x,_) -> (match List.assoc_opt x (snd externals) with
                           | Some (_,(_,_,b)) -> not(b)
                           | None -> true)
  | _ -> true


let vect_ sz v =
  Types.TyB_abstract("vect",[sz],[v]) ;;

(* improved typing with level of types *)
let ty_op ~externals op =
  let open Types in
  match op with
  | Resize_int k ->
      let sz = new_size_unknown() in
      let tyB = TyB_int sz in
      let tyB_k = TyB_int k in
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
  | Vector_create sz ->
      let v = new_tyB_unknown() in
      Ty_fun(Ty_base (v),Dur_zero,vect_ sz v)
 (* | Vector_length (sz,sz_res) ->
      let v = new_tyB_unknown() in
      let sz_int = sz_res in
      Ty_fun(Ty_base(vect_ sz v),Dur_zero,TyB_int sz_int)
  | Vector_get v ->
      let sz = new_size_unknown() in
      Ty_fun(Ty_base (TyB_tuple[vect_ sz v;
                                TyB_int (Sz_lit 32)]),Dur_zero,v)
  | Vector_update sz ->
      let v = new_tyB_unknown() in
      let t = vect_ sz v in
      Ty_fun(Ty_base (TyB_tuple[t;TyB_int (Sz_lit 32);v]),Dur_zero,t) *)
  | Size_of_val _ ->
      Ty_fun(new_ty_unknown(),Dur_zero,TyB_int (new_size_unknown()))
  | String_length ->
      (* enforce result to be a 16-bit integer *)
      let sz = new_size_unknown() in
      Ty_fun(Ty_base (TyB_string(sz)),Dur_zero,TyB_int (Sz_lit 16))
  | Bvect_of_int ->
       let sz = new_size_unknown() in
       Ty_fun(Ty_base(TyB_int(sz)),Dur_zero,vect_ sz TyB_bool)
  | Int_of_bvect ->
       let sz = new_size_unknown() in
       Ty_fun(Ty_base(vect_ sz TyB_bool),Dur_zero,TyB_int(sz))
  | External_fun (x,_) ->
    (match List.assoc_opt x (snd externals) with
    | Some (t,_) -> t
    | None -> Prelude.Errors.raise_error ~msg:("unbound operator "^x) ())

(** pretty printer for operators *)
let pp_op fmt (op:op) : unit =
  match op with
  | Resize_int sz -> Format.fprintf fmt "resize_int<%a>" Types.pp_size sz
  | Tuple_of_int k -> Format.fprintf fmt "%s" @@ "tuple_of_int<" ^ string_of_int k ^ ">"
  | Int_of_tuple k -> Format.fprintf fmt "%s" @@ "int_of_tuple<" ^ string_of_int k ^ ">"
  | GetBit -> Format.fprintf fmt "%s" @@ "get_bit"
  | UpdateBit -> Format.fprintf fmt "%s" @@ "update_bit"
  | Vector_create sz -> Format.fprintf fmt "vector_create<%a>" Types.pp_size sz
  (*| Vector_length _ -> Format.fprintf fmt "%s" @@ "vector_length"
  | Vector_get _ -> Format.fprintf fmt "%s" @@ "vector_get"
  | Vector_update _ -> Format.fprintf fmt "%s" @@ "vector_update"*)
  | Unroll n -> Format.fprintf fmt "%s" @@ "unroll" ^ string_of_int n ^ ">"
  | Size_of_val _ -> Format.fprintf fmt "%s" @@ "size_of_val"
  | Print -> Format.fprintf fmt "%s" @@ "print"
  | Print_string -> Format.fprintf fmt "%s" @@ "print_string"
  | Print_int -> Format.fprintf fmt "%s" @@ "print_int"
  | Print_newline -> Format.fprintf fmt "%s" @@ "print_newline"
  | Assert -> Format.fprintf fmt "%s" @@ "assert"
  | String_length -> Format.fprintf fmt "%s" @@ "string_length"
  | Bvect_of_int -> Format.fprintf fmt "%s" @@ "Bvect_of_int"
  | Int_of_bvect -> Format.fprintf fmt "%s" @@ "int_of_bvect"
  | External_fun (x,_) -> Format.fprintf fmt "%s" @@ x


(** code generator for operators *)
let gen_op fmt (op:op) pp a : unit =
  let open Format in
  let funcall fmt s = fprintf fmt "%s(%a)" s pp a in
  let procall fmt s = fprintf fmt "%s(clk,%a)" s pp a in
  let skip_when b fmt f s =
    if b then fprintf fmt "eclat_skip(eclat_unit)"
    else f fmt s 
  in
  match op with
  | Resize_int _ ->
      assert false (* special case *)  
  | Tuple_of_int _ ->
      pp fmt a (* should add a primitive call (identity ?) *)
  | Int_of_tuple _ ->
      pp fmt a (* should add a primitive call (identity ?) *)
  | GetBit -> 
      funcall fmt "eclat_getBit"
  | UpdateBit -> 
      funcall fmt "eclat_updateBit"
  | Unroll _ -> assert false (* should be eliminated before *)
  | Vector_create _ ->
      assert false (* special case *)  
  (*| Vector_length _ -> 
      assert false (* special case *)
  | Vector_get _ ->
     assert false (* special case *)
  | Vector_update _ ->
      assert false (* special case *)*)
  | Size_of_val _ ->
      assert false (* special case *)
  | Print ->
      skip_when !flag_no_print fmt procall "work.Print.print_value"
  | Print_string ->
      skip_when !flag_no_print fmt procall "work.Print.print_string"
  | Print_int ->
      skip_when !flag_no_print fmt procall "work.Int.print"
  | Print_newline ->
      skip_when !flag_no_print fmt procall "work.Print.print_newline"
  | Assert ->
      skip_when !flag_no_assert fmt funcall "work.Assertion.ok"
  | String_length ->
      procall fmt "eclat_string_length"
  | Bvect_of_int -> funcall fmt "eclat_bvect_of_int"
  | Int_of_bvect -> funcall fmt "eclat_int_of_bvect"
  | External_fun (x,_) -> funcall fmt (Printf.sprintf "work.%s" x)
