
(* name refering to the parameter of function [f] *)
let formal_param_of_fun f =
  (f^"_arg")

(* name used to identify instances of function [f] when called *)
let instance_id_of_fun f =
  f^"_id"

(* name refering to the result of function [f] *)
let result_of_fun f =
  f^"_result"

let instance_enum_const k =
  "I"^string_of_int k


let state_var_type st =
  "t_"^st

(* VHDL type name of the instances [I1], ... [In] associated
   to the Finite State Machine whose state variable is [st] *)
let instances_type st =
  "t_"^st

let extract_annot q =
  let len = String.length q in
  String.sub q 1 (len-1)

let mark_return f =
  "#"^f

let is_return f =
  f.[0] = '#'

let return_name q_annot =
  extract_annot q_annot
