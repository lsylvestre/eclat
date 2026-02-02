let apply2 ((f,z) : ((bool vect<'s> => bool vect<'s+1>) * bool vect<'s>))
    : bool vect<'s+1+1> =
  f(f(z)) ;;

let main () =
  () ;;
