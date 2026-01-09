open Runtime

type state_t = IDLE154

type state_var185 = IDLE157  | Q_795_FINI86
type state_var184 = IDLE161  | Q_812 | Q_818 | PAUSE_GET162 | PAUSE_GET168 | PAUSE_GET171 | PAUSE_SET165 | PAUSE_SET175 | PAUSE_SET178 | Q_WAIT163 | Q_WAIT166 | Q_WAIT169 | Q_WAIT172 | Q_WAIT176 | Q_WAIT179
let _718_tab = Array.make 50 (default_char ()) ;;
let _718_tab_lock : Lock.t = (Lock.init ()) ;;
let _718_tab_value = ref (default_char ()) ;;
let _741_r = Array.make 1 0L ;;
let _741_r_lock : Lock.t = (Lock.init ()) ;;
let _741_r_value = ref 0L ;;
let state = ref IDLE154 ;;
let state_var185 = ref IDLE157 ;;let state_var184 = ref IDLE161 ;;
let _793 : (unit*bool) ref = ref ((),false) ;;
let _794_w : (unit*bool) ref = ref ((),false) ;;
let result152 : (unit*bool) ref = ref ((),false) ;;
let _803_by : 'vv13 ref = ref (default_bytes ()) ;;
let _792_cy : Int.t ref = ref 0L ;;let _822 : Int.t ref = ref 0L ;;
let _823_cy : Int.t ref = ref 0L ;;let _795_fini86_id : Int.t ref = ref 0L ;;
let _812_id : Int.t ref = ref 0L ;;let _818_id : Int.t ref = ref 0L ;;
let _815 : 'vv14 ref = ref (default_char ()) ;;
let _807_n : Int.t ref = ref 0L ;;let _816 : Int.t ref = ref 0L ;;
let __718_tab_lock : bool ref = ref false ;;
let __741_r_lock : bool ref = ref false ;;let _v164 : bool ref = ref false ;;
let _v167 : bool ref = ref false ;;let _v170 : bool ref = ref false ;;
let _v173 : bool ref = ref false ;;let _v174 : bool ref = ref false ;;
let _v177 : bool ref = ref false ;;let _v180 : bool ref = ref false ;;
let _v181 : bool ref = ref false ;;let _v183 : bool ref = ref false ;;
let label148 : bool ref = ref false ;;let rdy153 : bool ref = ref false ;;
let rdy156 : bool ref = ref false ;;let rdy160 : bool ref = ref false ;;
let _818_arg : (Int.t*(unit*'vv15)) ref = ref (0L,((),(default_bytes ()))) ;;
let _795_fini86_arg : unit ref = ref () ;;let _796 : unit ref = ref () ;;
let _797 : unit ref = ref () ;;let _798 : unit ref = ref () ;;
let _801 : unit ref = ref () ;;let _802 : unit ref = ref () ;;
let _804 : unit ref = ref () ;;let _805 : unit ref = ref () ;;
let _806 : unit ref = ref () ;;let _808 : unit ref = ref () ;;
let _809 : unit ref = ref () ;;let _810 : unit ref = ref () ;;
let _811 : unit ref = ref () ;;let _812_result : unit ref = ref () ;;
let _817 : unit ref = ref () ;;let _818_result : unit ref = ref () ;;
let _821 : unit ref = ref () ;;let result155 : unit ref = ref () ;;
let result159 : unit ref = ref () ;;
let _812_arg : (Int.t*(unit*unit)) ref = ref (0L,((),())) ;;
let main_step =
  fun arg ->
    let argument = ref arg in
    (match !state with
    | IDLE154 -> 
      rdy153 := false;
      (if !label148 then
         (())
      else
        (label148 := true;
         _822 := Int.neg_ (1L))
        );
      _822 := Int.add_ ((!_822, 1L));
      _792_cy := !_822;
      _v183 := (tuple_get_ (1,!_793) : bool);
      (if !_v183 then
         ((match !state_var185 with
          | Q_795_FINI86 ->
             _795_fini86_arg := ();
             state_var185 := Q_795_FINI86
              
          | IDLE157 -> 
            rdy156 := false;
            _823_cy := !_792_cy;
            _796 := Print.string_ ("temps d'exécution: ");
            _797 := Print.int_ (!_823_cy);
            _798 := Print.newline_ (());
            _795_fini86_id := 1L;
            _795_fini86_arg := ();
            state_var185 := Q_795_FINI86
              
          );
         (if !rdy156 then
            (())
         else
           (result155 := ())
           );
         _794_w := (!result155, !rdy156);
         _793 := ((), true))
      else
        ((match !state_var184 with
         | Q_812 ->
            _v174 := Int.gt_ (((tuple_get_ (0,!_812_arg) : Int.t), Int.sub_ ((Int64.of_int (Array.length _718_tab), 1L))));
            (if !_v174 then
               (_812_result := ();
                _811 := !_812_result;
                _v164 := Lock.is_taken(_741_r_lock);
                (if !_v164 then
                   (state_var184 := Q_WAIT163)
                else
                  (Lock.acquire(_741_r_lock);
                   _741_r_value := _741_r.(Int64.to_int (0L));
                   state_var184 := PAUSE_GET162)
                  ))
            else
              (_v173 := Lock.is_taken(_718_tab_lock);
               (if !_v173 then
                  (state_var184 := Q_WAIT172)
               else
                 (Lock.acquire(_718_tab_lock);
                  _718_tab_value := _718_tab.(Int64.to_int ((tuple_get_ (0,!_812_arg) : Int.t)));
                  state_var184 := PAUSE_GET171)
                 ))
              )
             
         | Q_818 ->
            _v181 := Int.gt_ (((tuple_get_ (0,!_818_arg) : Int.t), Int.sub_ ((Bytes.len_ ((tuple_get_ (1,(tuple_get_ (1,!_818_arg) : (unit*'vv17))) : 'vv16)), 1L))));
            (if !_v181 then
               (_818_result := ();
                _805 := !_818_result;
                _v177 := Lock.is_taken(_741_r_lock);
                (if !_v177 then
                   (state_var184 := Q_WAIT176)
                else
                  (Lock.acquire(_741_r_lock);
                   (_741_r.(Int64.to_int (0L)) <- 0L);
                   state_var184 := PAUSE_SET175)
                  ))
            else
              (_v180 := Lock.is_taken(_718_tab_lock);
               (if !_v180 then
                  (state_var184 := Q_WAIT179)
               else
                 (Lock.acquire(_718_tab_lock);
                  (_718_tab.(Int64.to_int ((tuple_get_ (0,!_818_arg) : Int.t))) <- Bytes.get_ (432, 8)(((tuple_get_ (1,(tuple_get_ (1,!_818_arg) : (unit*'vv19))) : 'vv18), (tuple_get_ (0,!_818_arg) : Int.t))));
                  state_var184 := PAUSE_SET178)
                 ))
              )
             
         | PAUSE_GET162 ->
            _807_n := !_741_r_value;
            Lock.release(_741_r_lock);
            _808 := Print.newline_ (());
            _809 := Print.string_ ("sommes des caractères: ");
            _810 := Print.int_ (!_807_n);
            result159 := Print.newline_ (());
            rdy160 := true;
            state_var184 := IDLE161
             
         | PAUSE_GET168 ->
            _816 := !_741_r_value;
            Lock.release(_741_r_lock);
            _v167 := Lock.is_taken(_741_r_lock);
            (if !_v167 then
               (state_var184 := Q_WAIT166)
            else
              (Lock.acquire(_741_r_lock);
               (_741_r.(Int64.to_int (0L)) <- Int.add_ ((!_816, Int.resize_(Char.code_ (!_815),32))));
               state_var184 := PAUSE_SET165)
              )
             
         | PAUSE_GET171 ->
            _815 := !_718_tab_value;
            Lock.release(_718_tab_lock);
            _v170 := Lock.is_taken(_741_r_lock);
            (if !_v170 then
               (state_var184 := Q_WAIT169)
            else
              (Lock.acquire(_741_r_lock);
               _741_r_value := _741_r.(Int64.to_int (0L));
               state_var184 := PAUSE_GET168)
              )
             
         | PAUSE_SET165 ->
            ();
            Lock.release(_741_r_lock);
            _817 := ();
            _812_arg := (Int.add_ (((tuple_get_ (0,!_812_arg) : Int.t), 1L)), ((), ()));
            state_var184 := Q_812
             
         | PAUSE_SET175 ->
            ();
            Lock.release(_741_r_lock);
            _806 := ();
            _812_id := 2L;
            _812_arg := (0L, ((), ()));
            state_var184 := Q_812
             
         | PAUSE_SET178 ->
            ();
            Lock.release(_718_tab_lock);
            _821 := ();
            _818_arg := (Int.add_ (((tuple_get_ (0,!_818_arg) : Int.t), 1L)), ((), (tuple_get_ (1,(tuple_get_ (1,!_818_arg) : (unit*'vv21))) : 'vv20)));
            state_var184 := Q_818
             
         | Q_WAIT163 ->
            _v164 := Lock.is_taken(_741_r_lock);
            (if !_v164 then
               (state_var184 := Q_WAIT163)
            else
              (Lock.acquire(_741_r_lock);
               _741_r_value := _741_r.(Int64.to_int (0L));
               state_var184 := PAUSE_GET162)
              )
             
         | Q_WAIT166 ->
            _v167 := Lock.is_taken(_741_r_lock);
            (if !_v167 then
               (state_var184 := Q_WAIT166)
            else
              (Lock.acquire(_741_r_lock);
               (_741_r.(Int64.to_int (0L)) <- Int.add_ ((!_816, Int.resize_(Char.code_ (!_815),32))));
               state_var184 := PAUSE_SET165)
              )
             
         | Q_WAIT169 ->
            _v170 := Lock.is_taken(_741_r_lock);
            (if !_v170 then
               (state_var184 := Q_WAIT169)
            else
              (Lock.acquire(_741_r_lock);
               _741_r_value := _741_r.(Int64.to_int (0L));
               state_var184 := PAUSE_GET168)
              )
             
         | Q_WAIT172 ->
            _v173 := Lock.is_taken(_718_tab_lock);
            (if !_v173 then
               (state_var184 := Q_WAIT172)
            else
              (Lock.acquire(_718_tab_lock);
               _718_tab_value := _718_tab.(Int64.to_int ((tuple_get_ (0,!_812_arg) : Int.t)));
               state_var184 := PAUSE_GET171)
              )
             
         | Q_WAIT176 ->
            _v177 := Lock.is_taken(_741_r_lock);
            (if !_v177 then
               (state_var184 := Q_WAIT176)
            else
              (Lock.acquire(_741_r_lock);
               (_741_r.(Int64.to_int (0L)) <- 0L);
               state_var184 := PAUSE_SET175)
              )
             
         | Q_WAIT179 ->
            _v180 := Lock.is_taken(_718_tab_lock);
            (if !_v180 then
               (state_var184 := Q_WAIT179)
            else
              (Lock.acquire(_718_tab_lock);
               (_718_tab.(Int64.to_int ((tuple_get_ (0,!_818_arg) : Int.t))) <- Bytes.get_ (432, 8)(((tuple_get_ (1,(tuple_get_ (1,!_818_arg) : (unit*'vv23))) : 'vv22), (tuple_get_ (0,!_818_arg) : Int.t))));
               state_var184 := PAUSE_SET178)
              )
             
         | IDLE161 -> 
           rdy160 := false;
           _801 := Print.string_ ("lecture du fichier `input.txt`");
           _802 := Print.newline_ (());
           _803_by := InputFile.read_file_ (256, 400)("../advent-of-code/input.txt");
           _804 := Bytes.print_ (!_803_by);
           _818_id := 3L;
           _818_arg := (0L, ((), !_803_by));
           state_var184 := Q_818
             
         );
        (if !rdy160 then
           (())
        else
          (result159 := ())
          );
        _793 := (!result159, !rdy160))
      );
      result152 := !_793;
      rdy153 := true;
      state := IDLE154
    );!result152


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

