open Runtime

type state_t = IDLE106

type state_var112 = IDLE109  | Q_665_FACT86
let state = ref IDLE106 ;;
let state_var112 = ref IDLE109 ;;
let result104 : (unit*bool) ref = ref ((),false) ;;
let _665_fact86_id : Int.t ref = ref 0L ;;
let _665_fact86_result : Int.t ref = ref 0L ;;
let _666 : Int.t ref = ref 0L ;;let _670 : Int.t ref = ref 0L ;;
let _671 : Int.t ref = ref 0L ;;
let _665_fact86_arg : (Int.t*Int.t) ref = ref (0L,0L) ;;
let _669 : bool ref = ref false ;;let _v110 : bool ref = ref false ;;
let rdy105 : bool ref = ref false ;;let rdy108 : bool ref = ref false ;;
let result107 : unit ref = ref () ;;
let main_step =
  fun arg ->
    let argument = ref arg in
    (match !state with
    | IDLE106 -> 
      rdy105 := false;
      (match !state_var112 with
      | Q_665_FACT86 ->
         _669 := Int.lt_ (((tuple_get_ (0,!_665_fact86_arg) : Int.t), 2L));
         _v110 := !_669;
         (if !_v110 then
            (_665_fact86_result := (tuple_get_ (1,!_665_fact86_arg) : Int.t);
             _666 := !_665_fact86_result;
             result107 := Int.print_ (!_666);
             rdy108 := true;
             state_var112 := IDLE109)
         else
           (_670 := Int.sub_ (((tuple_get_ (0,!_665_fact86_arg) : Int.t), 1L));
            _671 := Int.mul_ (((tuple_get_ (1,!_665_fact86_arg) : Int.t), (tuple_get_ (0,!_665_fact86_arg) : Int.t)));
            _665_fact86_arg := (!_670, !_671);
            state_var112 := Q_665_FACT86)
           )
          
      | IDLE109 -> 
        rdy108 := false;
        _665_fact86_id := 1L;
        _665_fact86_arg := (6L, 1L);
        state_var112 := Q_665_FACT86
          
      );
      (if !rdy108 then
         (())
      else
        (result107 := ())
        );
      result104 := (!result107, !rdy108);
      rdy105 := true;
      state := IDLE106
      
  );!result104


let run n =
  let args_list = [|()|] in
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

