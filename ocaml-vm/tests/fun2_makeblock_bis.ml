open CustomStdlib

let f (x,y) = (y,x);;

let a = f ((100,42),30) in
print_int (match a with (i,(j,k)) -> i+j+k);;
