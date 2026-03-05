
let flag_no_assert = ref false
let flag_no_print = ref false

module SMap = Map.Make(String) (* todo: share this definition *)

(* instantaneous primitives which do not require an encoding *)
type op =
  | Tuple_of_int of int (* macro converting tuple of booleans to int *)
  | Int_of_tuple of int (* macro converting int to tuple of booleans *)
  | Unroll of int (* experimental *)
  | External_fun of string * Types.ty

let combinational ~externals p =
  match p with
  | External_fun (x,_) -> (match SMap.find_opt x externals with
                           | Some (_,(_,_,pure,_)) -> not(pure)
                           | None -> true)
  | _ -> true


let vect_ sz v =
  Types.TyB_abstract("vect",[sz],[v]) ;;

let char_ =
  Types.TyB_abstract("char",[],[]) ;;

(* improved typing with level of types *)
let ty_op ~externals op =
  let open Types in
  match op with
  | Tuple_of_int k ->
      let tyB = TyB_tuple (List.init k (fun _ -> TyB_bool)) in
      Ty_fun(Ty_base (TyB_int (Sz_lit k)),Dur_int 0,tyB)
  | Int_of_tuple k ->
      let tyB = TyB_tuple (List.init k (fun _ -> TyB_bool)) in
      Ty_fun(Ty_base tyB,Dur_int 0, TyB_int (Sz_lit k))
  | Unroll _ ->
      let v = new_ty_unknown() in
      let d = new_dur_unknown() in
      let v' = new_tyB_unknown() in
      Ty_fun((Ty_tuple[Ty_fun(v,d,v'); v]),d,v')
  | External_fun (x,_) ->
    (match SMap.find_opt x externals with
    | Some (t,_) -> t
    | None -> Prelude.Errors.raise_error ~msg:("unbound operator "^x) ())

(** pretty printer for operators *)
let pp_op fmt (op:op) : unit =
  match op with
  | Tuple_of_int k -> Format.fprintf fmt "%s" @@ "tuple_of_int<" ^ string_of_int k ^ ">"
  | Int_of_tuple k -> Format.fprintf fmt "%s" @@ "int_of_tuple<" ^ string_of_int k ^ ">"
  | Unroll n -> Format.fprintf fmt "%s" @@ "unroll" ^ string_of_int n ^ ">"
  | External_fun (x,_) -> Format.fprintf fmt "%s" @@ x


(** code generator for operators *)
let gen_op ~operators fmt (op:op) pp a : unit =
  let open Format in
  let funcall fmt s = fprintf fmt "%s(%a)" s pp a in
  let procall fmt s = fprintf fmt "%s(clk,%a)" s pp a in
  match op with
  | Tuple_of_int _ ->
      pp fmt a (* should add a primitive call (identity ?) *)
  | Int_of_tuple _ ->
      pp fmt a (* should add a primitive call (identity ?) *)
  | Unroll _ -> assert false (* should be eliminated before *)
  | External_fun (x,_) ->
      (match SMap.find_opt x operators with
       | Some (t,(_,_,is_imp,_)) ->
          if is_imp 
          then procall fmt (Printf.sprintf "work.%s" x)
          else funcall fmt (Printf.sprintf "work.%s" x)
       | None -> Prelude.Errors.raise_error ~msg:("unbound operator "^x) ())

      
