external queens : int -> int array list = "caml_queens" ;;

let lst = queens(10) ;;
print_int 42;;
(* print_int (List.length lst) ;; *)
(*
List.iter (fun a ->
  print_int 12345;
  Array.iter print_int a) lst;;
*)
(*
print_int (List.length lst) ;;

List.iter (fun a ->
  print_int 1234567;
  Array.iter print_int a) lst;;*)

(*
match lst with 
| [|a;b;c;d;e;f;g;h|]::v ->
   print_int a; print_newline ();
   print_int b;print_newline ();
   print_int c;print_newline ();
   print_int d;print_newline ();
   print_int d;print_newline ();
   print_int e;print_newline ();
   print_int f;print_newline ();
   print_int g;print_newline ();
   print_int h

*)

