let f (x,k) = [k(x)||k(create<5>())] ;;

f (create<5>(),(fun a -> set(a,0,42)));;

