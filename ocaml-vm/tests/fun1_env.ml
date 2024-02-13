external ( + ) : int -> int -> int = "%addint"
external ( < ) : 'a -> 'a -> bool = "%lessthan"

external print : int -> unit = "1";;

let f = let a = 1 in fun x -> x + a;;


print (f 42);;
