open CustomStdlib

let f = let a = 1000 in fun x y z u -> x + y + z + u + a;;


print_int (f 42 200 100 300);;
