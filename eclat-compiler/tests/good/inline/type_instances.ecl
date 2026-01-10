operator M.f : bool vect<'s> => bool vect<'s+1> ;;

let m_f x = M.f x ;;

  (* let m_f = M.f ;; *)

let x = m_f (vect_create<5>(true)) ;;
let y = m_f (vect_create<6>(true)) ;;

let main () = (x,y) ;;

