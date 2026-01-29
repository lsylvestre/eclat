
let main () =
  let i = (42 : int<'z + 1+2>) in
  let j = (42 : int <'z + 2>) in
  (i + j) ;;
