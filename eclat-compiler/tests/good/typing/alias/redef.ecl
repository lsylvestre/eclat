type ~a t<?n> = int<?n> * ~a;;
type t = bool t<5>;;

let f (x:t) = x;;

f (0,true);;

type t = (int * int);;

let main (y:t) = y ;;