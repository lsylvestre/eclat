let main x =
  assert (x > 10);
  print_string "foo";
  print_newline () ;;

(* to run: $ ./eclat tests/assertion.ecl \
                  -arg "11;3;100;2;1000" \
              ; make simul NS=200

~>

foo
main.vhdl:40:13:@20ns:(assertion error): assertion failed
foo
foo
main.vhdl:40:13:@40ns:(assertion error): assertion failed
foo
foo
foo
foo
foo

*)