let static t = 10^100 ;;
let static depth = 10 ;;

let reduce (f,init,src,depth) =
  let rec loop(acc,i) =
    if i <= src.length - depth
    then let g (ofs,acc) = f (acc, src[i+ofs]) in
         let acc = generate g acc ~depth:depth in
         loop(acc,i+depth)
    else acc
  in loop(init,0) ;;

let counter () =
  reg (fun c -> c + 1) last 0 ;;


let main () =
  let c = counter () in
  let ((),rdy) = 
    exec 
      print_int (reduce ((+),0,t,depth));
      print_newline ()
    default ()
  in if rdy then (print_string "cy:"; print_int c; print_newline ()) ;;


(*
/\ ~x:t . e



let rec h x =
  x + 1;;

let foo () ~f =
  f 42
;;

let main () =
  exec
  print_int (foo () ~f:h) 
default ();;


<size n>

(* val f : unit -> forall size<n> 
                -> forall x:array<t,s * n> 
                -> unit *)
let f = fun () -> /\ (size<n>) . /\ x:array<t,s * n> .
  let rec loop i =
    if i < x.length then
      print_int x[i]
    else loop(i+n)
  in
  

  generate (f,e) (size n)


let f = fun e -> /\ (size n) ->
  generate (f,e) (size n)

*)

(* 


let h (x,y) =
  x + 2 * y;;


let main () =
  print_int (generate (h,0) ~n:10) ;;


*)
(* 

          G[x:t]|- e:t'
--------------------------------------
G |- (/\ x:t . e) : forrall x : t . t'



  *)