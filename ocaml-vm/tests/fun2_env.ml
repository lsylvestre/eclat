open CustomStdlib


let f = let a = 1000 in fun x y -> x + y + a;;


print_int (f 42 200);;
