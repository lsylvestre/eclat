(* $ ./eclat tests/pipe.ecl -relax -arg "1;2;3;4;5;6;7;8;9;10;11;12;13;14"
   $ make simul NS=400 *)

let pipe (f1,f2,x0) =
  let ((_,x2,_),rdy) = 
    reg (fun ((x1,x2,init),rdy) ->
          exec (let x1 = f1(x0) 
                and x2 = if init then x2 else f2(x1) in
                (x1,x2,false))
          default (x1,x2,init))
    last ((0,0,true),false)
  in
  (x2,rdy)
;;

let wait n =
  let rec loop (m) = 
    if m < 1 then n else loop(m - 1) 
  in loop(n) ;;

let sum n = 
  let rec loop (acc,n) = 
    if n < 1 then acc else loop(acc+n,n-1) 
  in loop(0,n) ;;

let rec f1 x = x * 2 ;;
let rec f2 x = x + 1 ;;
let rec f3 x = x * 100 ;;

let main x0 =
  print_string "|x0:"; print_int x0;
  let (o,rdy) = pipe(f1,f2,x0) in
  print_string "|rdy:"; print_int (if rdy then 1 else 0);
  print_newline ();
  if rdy then (
    print_string "===> x2: "; print_int o; print_newline ()
  ) else ()
  ;;
