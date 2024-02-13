open CustomStdlib

let a = (42,10) in
print_int (match a with (x,y) -> x + y);;
