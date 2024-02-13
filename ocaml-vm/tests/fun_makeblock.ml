open CustomStdlib

let f x = (x,x,x);;

let a = f 100 in
print_int (match a with (i,j,k) -> i+j+k);;
