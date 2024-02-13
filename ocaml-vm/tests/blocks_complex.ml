open CustomStdlib

let a = (42,(10,20,17)) in
let b = (5,6) in
print_int (match a,b with 
	       | (x,(y,z,u)),(v1,v2) -> x + y + z + u + v1 + v2);;
