let addsub((neg_b, a, b) : bool * int<'N> * int<'N>) :int<'N> =
  let x = (a lsl 1) lor 1 in
  let y = if neg_b then ((lnot b) lsl 1) lor 1
                   else (b lsl 1) lor 0 in
  resize_int<'N>((x+y) asr 1) ;;

type aluop = Sub of unit 
           | Add of unit
           | Lsl of unit
           | Asr of unit ;;

let alu(op,a,b) =
  let neg_b = match op with Sub() -> true | _ -> false in
  let v = addsub(neg_b,a,b) in
  match op with
  | Add() -> v
  | Sub() -> v
  | Lsl() -> a lsl b
  | Asr() -> a asr b ;;



(*
  let test_alu () =
    print_int (alu(Add(),(6:int<32>),5));
    print_newline ();;
*)