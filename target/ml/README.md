# simulating the circuit generated by the Eclat compiler, by translating it to OCaml code.

```
$ cd eclat-compiler
$ make
$ ./eclat -ocaml -arg="1;2;3;4"
> let main i = 
     print_int (reg (fun s -> s + i) init 0);
     print_newline ();;
val main : forall '143  . (int<~z143> -{0}-> unit) | 0

> #q.
ocaml code generated in ../target/ml/main_step.ml
$
$ cd ../target/ml
$ ocamlc runtime.ml main_step.ml 
./a.out -run 10
1
3
6
10
14
18
22
26
30
34
$ cat main_step.ml
open Runtime

type state_t = IDLE23


let main_step =
  let state = ref IDLE23 in
  let _108 : Int.t ref = ref 0L in
  let _110 : Int.t ref = ref 0L in
  let _v20 : bool ref = ref false in
  let rdy22 : bool ref = ref false in
  let _109 : unit ref = ref () in
  let result21 : unit ref = ref () in
  fun arg ->
    let argument = ref arg in
    (match !state with
    | IDLE23 -> 
      rdy22 := false;
      (if !_v20 then
         (())
      else
        (_v20 := true;
         _110 := 0L)
        );
      _110 := Int.add_ ((!_110, !argument));
      _108 := !_110;
      _109 := Print.int_ (!_108);
      result21 := Print.newline_ (());
      rdy22 := true;
      state := IDLE23
        
    );
  !result21
  

let run n =
  let args_list = [|1L; 2L; 3L; 4L|] in
  let k = Array.length args_list - 1 in
  for i = 0 to n - 1 do
    ignore (main_step (args_list.(min i k)))
  done;;
  

let () =
  Arg.parse [
    ("-run", Arg.Int (fun n -> run n), "execute the given number of cycles")
  ]
  (fun _ -> ()) "Usage:
  ./eclat file"
;;
```