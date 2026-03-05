let f <<?n>>(x,y) =
  for i = 1 to int<<?n>> do
    pause();pause();print_int i
  done
wcet (?n+?n+?n);;

(*  Error: Expect ?n1 * 3 + 1 <= ?n1 + ?n1 + ?n1.
    It is not always true.
    For example, 127 > 126. *)