(* ./eclat -relax tests/good/specialize/sp10.ecl  -arg="()" -nostdlib *)

let main () =
  let a = create<1> () in
  let rec h () =
    set(a,0,true); h() in
  let rec loop() =
    h ()
  in 
  loop();;
