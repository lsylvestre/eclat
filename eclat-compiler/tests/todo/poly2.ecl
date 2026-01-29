type `a vect<?s> ;;

operator M.f : `a => `a vect <?a> ;;
let g <<?s>> y : `a vect<?s> = M.f y;;


let main () =
   let a = g<<4>> (0) in
   let b = g<<5>> (true) in
   (a,b) ;;
