type `a vect<?s> ;;

operator M.f : unit => `a vect <?a> ;;
let g <<?s>> y : `a vect<?s> = M.f ();;


let main () =
   let a = g<<4>> () in
   let b = g<<5>> () in
   (a,b) ;;


operator Vect2.create : `a => `a vect<?sz> ;;
let vect2_make <<?s>> (x:`a) : `a vect<?s> = Vect2.create(x) ;;


let main () =
  let a = vect2_make<<64>>(false) in
  let w = vect2_make<<65>>(5) in
  (a,w);;