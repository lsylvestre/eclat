(* ./eclat -relax *)

type ~a matrix<?n,?size> = ~a array<?size> ;;


let reduce<<?n>>(f,(tab:~a array<?n>)) =
  let s = ref 0 in
  for i = 1 to int<<?n>> by 4 do
    s := f(!s, get(tab,i-1))
  done;
  !s