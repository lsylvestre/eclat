
let nb_bits_limit = 32 ;;
let size_limit = 1 lsl nb_bits_limit ;;

let err_size_overflow () =
  Prelude.Errors.error (fun fmt ->
        Format.fprintf fmt "The program try to create a very huge value (more than 2^%d bits).\nThis is not representable in the generated code.\n" 
            nb_bits_limit) 

module Size_op = struct
  let binop f s1 s2 =
    if s1 > size_limit || s2 > size_limit then err_size_overflow () else
    let x = f s1 s2 in
    if x > size_limit then err_size_overflow () 
    else x
  
  let add = binop (+)
  
  let sub = binop ( - )
  
  let mul a b = 
    let x = binop ( * ) a b in
    if x < a || x < b then err_size_overflow () 
    else x

  let div = ( / )
  
  let pow = binop (fun n k -> 
              if k > 32 then err_size_overflow () else
              n lsl (k - 1))
end