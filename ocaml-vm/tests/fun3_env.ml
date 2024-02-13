open CustomStdlib

let f = let a = 1000 in fun x y z -> x + y + z + a;;


print_int (f 42 200 100);;
