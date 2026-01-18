
let rec interval min max =
  if min > max then [] else min :: interval (min + 1) max

let rec filter p l = match l with
    []   -> []
  | (a::r) -> if p a then a :: filter p r else filter p r

let remove_multiples_of n =
  filter (fun m -> if (m mod n) = 0 then false else true)

let sieve max =
  let rec filter_again = function
     [] -> []
  | n::r as l ->
      if n*n > max then l else n :: filter_again (remove_multiples_of n r)
  in
    filter_again (interval 2 max)

let rec do_list f l = match l with
       []   -> ()
  |  (a::q) -> f a; do_list f q


let () =
    do_list (fun n -> print_int n) (sieve 50);;