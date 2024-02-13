open CustomStdlib

let f (x,y) = (y,x);;

let a = f (30,100) in

print_int (match a with (i,j) -> i+j);;
