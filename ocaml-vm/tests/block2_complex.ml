open CustomStdlib

let a = (42,(10,20,17)) in
print_int (match a with (x,(y,z,u)) -> x + y + z + u);;
