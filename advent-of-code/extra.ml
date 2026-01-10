
(* type char = char *)
let default_char () = '0' ;;

type bytes = string ref ;;
let default_bytes () = Bytes.create 0 ;;

module Eclat_char = struct
  let code_ c = Int64.of_int @@ Char.code c
  let chr_ n = Char.chr @@ Int64.to_int n
  let print_ c = output_char stdout c
end
module Eclat_bytes = struct
  let make_ (_sz_arg,sz_res) (c) = Bytes.make (sz_res/8) c
  let len_ b = Int64.of_int @@ Bytes.length b
  let get_ _ (by,i) = Bytes.get by (Int64.to_int i)
  let print_ by = 
    for i = 0 to Bytes.length by - 1 do
      Eclat_char.print_ (Bytes.get by i)
    done 
  let form_vect_ v =
    Bytes.init (Array.length v) (fun i -> Array.get v i) ;;
  let to_vect_ b =
    Array.init (Bytes.length b) (fun i -> Bytes.get b i) ;;
end


module Eclat_IOFile = struct
  let read_file_ (_sz_arg,sz_res) name =
    let ic = open_in ("../"^name) in
    let s = In_channel.input_all ic in
    close_in ic;
    let res = Bytes.create (sz_res/8) in
    String.iteri (fun i c -> Bytes.set res i c) s;
    res

  let write_file_ (name,by) =
    let oc = open_out ("../"^name) in
    Out_channel.output_bytes oc by;
    close_out oc
end

module Char = Eclat_char
module Bytes = Eclat_bytes
module IOFile = Eclat_IOFile