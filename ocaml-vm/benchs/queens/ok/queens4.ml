


external foo : int -> int array list = "foo" ;;

let lst = foo(9) ;;
print_int (List.length lst) ;;

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
List.iter (fun a ->
  print_int 1234567;
  Array.iter print_int a) lst;;