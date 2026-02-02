operator M.f : `a => unit ;; 
       (* ===========================
          this `a should be different 
          to any other occurrence of `a 
          in other declarations:
          type variables in the type signature 
          of each operator must be renamed. 
          =========================== *)

let g (y:`b) : `a = M.f y;; 
       (* ===========================
          `b should not be unified with `a !!!
        * =========================== *)

let main () =
   let a = g (true,true) in
   let b = g (true) in
         (* =========================
         g must be called with argument 
         of different types (by 
         instantiating its type scheme)
         ==================== *)
   (a,b) ;;

