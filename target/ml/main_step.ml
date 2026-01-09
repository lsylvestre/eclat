open Runtime

open Extra

type state_t = IDLE153

type state_var184 = IDLE156  | Q_790_FINI86
type state_var183 = IDLE160  | Q_806 | Q_812 | PAUSE_GET161 | PAUSE_GET167 | PAUSE_GET170 | PAUSE_SET164 | PAUSE_SET174 | PAUSE_SET177 | Q_WAIT162 | Q_WAIT165 | Q_WAIT168 | Q_WAIT171 | Q_WAIT175 | Q_WAIT178
let _714_tab = Array.make 50 (default_char ()) ;;
let _714_tab_lock : Lock.t = (Lock.init ()) ;;
let _714_tab_value = ref (default_char ()) ;;
let _736_r = Array.make 1 0L ;;
let _736_r_lock : Lock.t = (Lock.init ()) ;;
let _736_r_value = ref 0L ;;
let state = ref IDLE153 ;;
let state_var184 = ref IDLE156 ;;let state_var183 = ref IDLE160 ;;
let _788 : (unit*bool) ref = ref ((),false) ;;
let _789_w : (unit*bool) ref = ref ((),false) ;;
let result151 : (unit*bool) ref = ref ((),false) ;;
let _797_by : 'vv13 ref = ref (default_bytes ()) ;;
let _790_fini86_id : Int.t ref = ref 0L ;;let _806_id : Int.t ref = ref 0L ;;
let _812_id : Int.t ref = ref 0L ;;
let _809 : 'vv14 ref = ref (default_char ()) ;;
let _801_n : Int.t ref = ref 0L ;;let _810 : Int.t ref = ref 0L ;;
let _787_cy : Int.t ref = ref 0L ;;let _816 : Int.t ref = ref 0L ;;
let _817_cy : Int.t ref = ref 0L ;;
let __714_tab_lock : bool ref = ref false ;;
let __736_r_lock : bool ref = ref false ;;let _v163 : bool ref = ref false ;;
let _v166 : bool ref = ref false ;;let _v169 : bool ref = ref false ;;
let _v172 : bool ref = ref false ;;let _v173 : bool ref = ref false ;;
let _v176 : bool ref = ref false ;;let _v179 : bool ref = ref false ;;
let _v180 : bool ref = ref false ;;let _v182 : bool ref = ref false ;;
let label147 : bool ref = ref false ;;let rdy152 : bool ref = ref false ;;
let rdy155 : bool ref = ref false ;;let rdy159 : bool ref = ref false ;;
let _812_arg : (Int.t*(unit*'vv15)) ref = ref (0L,((),(default_bytes ()))) ;;
let _790_fini86_arg : unit ref = ref () ;;let _791 : unit ref = ref () ;;
let _792 : unit ref = ref () ;;let _793 : unit ref = ref () ;;
let _796 : unit ref = ref () ;;let _798 : unit ref = ref () ;;
let _799 : unit ref = ref () ;;let _800 : unit ref = ref () ;;
let _802 : unit ref = ref () ;;let _803 : unit ref = ref () ;;
let _804 : unit ref = ref () ;;let _805 : unit ref = ref () ;;
let _806_result : unit ref = ref () ;;let _811 : unit ref = ref () ;;
let _812_result : unit ref = ref () ;;let _815 : unit ref = ref () ;;
let result154 : unit ref = ref () ;;let result158 : unit ref = ref () ;;
let _806_arg : (Int.t*(unit*unit)) ref = ref (0L,((),())) ;;
let main_step =
  fun arg ->
    let argument = ref arg in
    (match !state with
    | IDLE153 -> 
      rdy152 := false;
      (if !label147 then
         (())
      else
        (label147 := true;
         _816 := Int.neg_ (1L))
        );
      _816 := Int.add_ ((!_816, 1L));
      _787_cy := !_816;
      _v182 := (tuple_get_ (1,!_788) : bool);
      (if !_v182 then
         ((match !state_var184 with
          | Q_790_FINI86 ->
             _790_fini86_arg := ();
             state_var184 := Q_790_FINI86
              
          | IDLE156 -> 
            rdy155 := false;
            _817_cy := !_787_cy;
            _791 := Print.string_ ("temps d'exécution: ");
            _792 := Print.int_ (!_817_cy);
            _793 := Print.newline_ (());
            _790_fini86_id := 1L;
            _790_fini86_arg := ();
            state_var184 := Q_790_FINI86
              
          );
         (if !rdy155 then
            (())
         else
           (result154 := ())
           );
         _789_w := (!result154, !rdy155);
         _788 := ((), true))
      else
        ((match !state_var183 with
         | Q_806 ->
            _v173 := Int.gt_ (((tuple_get_ (0,!_806_arg) : Int.t), Int.sub_ ((Int64.of_int (Array.length _714_tab), 1L))));
            (if !_v173 then
               (_806_result := ();
                _805 := !_806_result;
                _v163 := Lock.is_taken(_736_r_lock);
                (if !_v163 then
                   (state_var183 := Q_WAIT162)
                else
                  (Lock.acquire(_736_r_lock);
                   _736_r_value := _736_r.(Int64.to_int (0L));
                   state_var183 := PAUSE_GET161)
                  ))
            else
              (_v172 := Lock.is_taken(_714_tab_lock);
               (if !_v172 then
                  (state_var183 := Q_WAIT171)
               else
                 (Lock.acquire(_714_tab_lock);
                  _714_tab_value := _714_tab.(Int64.to_int ((tuple_get_ (0,!_806_arg) : Int.t)));
                  state_var183 := PAUSE_GET170)
                 ))
              )
             
         | Q_812 ->
            _v180 := Int.gt_ (((tuple_get_ (0,!_812_arg) : Int.t), Int.sub_ ((Bytes.len_ ((tuple_get_ (1,(tuple_get_ (1,!_812_arg) : (unit*'vv17))) : 'vv16)), 1L))));
            (if !_v180 then
               (_812_result := ();
                _799 := !_812_result;
                _v176 := Lock.is_taken(_736_r_lock);
                (if !_v176 then
                   (state_var183 := Q_WAIT175)
                else
                  (Lock.acquire(_736_r_lock);
                   (_736_r.(Int64.to_int (0L)) <- 0L);
                   state_var183 := PAUSE_SET174)
                  ))
            else
              (_v179 := Lock.is_taken(_714_tab_lock);
               (if !_v179 then
                  (state_var183 := Q_WAIT178)
               else
                 (Lock.acquire(_714_tab_lock);
                  (_714_tab.(Int64.to_int ((tuple_get_ (0,!_812_arg) : Int.t))) <- Bytes.get_ (432, 8)(((tuple_get_ (1,(tuple_get_ (1,!_812_arg) : (unit*'vv19))) : 'vv18), (tuple_get_ (0,!_812_arg) : Int.t))));
                  state_var183 := PAUSE_SET177)
                 ))
              )
             
         | PAUSE_GET161 ->
            _801_n := !_736_r_value;
            Lock.release(_736_r_lock);
            _802 := Print.newline_ (());
            _803 := Print.string_ ("sommes des caractères: ");
            _804 := Print.int_ (!_801_n);
            result158 := Print.newline_ (());
            rdy159 := true;
            state_var183 := IDLE160
             
         | PAUSE_GET167 ->
            _810 := !_736_r_value;
            Lock.release(_736_r_lock);
            _v166 := Lock.is_taken(_736_r_lock);
            (if !_v166 then
               (state_var183 := Q_WAIT165)
            else
              (Lock.acquire(_736_r_lock);
               (_736_r.(Int64.to_int (0L)) <- Int.add_ ((!_810, Int.resize_(Char.code_ (!_809),32))));
               state_var183 := PAUSE_SET164)
              )
             
         | PAUSE_GET170 ->
            _809 := !_714_tab_value;
            Lock.release(_714_tab_lock);
            _v169 := Lock.is_taken(_736_r_lock);
            (if !_v169 then
               (state_var183 := Q_WAIT168)
            else
              (Lock.acquire(_736_r_lock);
               _736_r_value := _736_r.(Int64.to_int (0L));
               state_var183 := PAUSE_GET167)
              )
             
         | PAUSE_SET164 ->
            ();
            Lock.release(_736_r_lock);
            _811 := ();
            _806_arg := (Int.add_ (((tuple_get_ (0,!_806_arg) : Int.t), 1L)), ((), ()));
            state_var183 := Q_806
             
         | PAUSE_SET174 ->
            ();
            Lock.release(_736_r_lock);
            _800 := ();
            _806_id := 2L;
            _806_arg := (0L, ((), ()));
            state_var183 := Q_806
             
         | PAUSE_SET177 ->
            ();
            Lock.release(_714_tab_lock);
            _815 := ();
            _812_arg := (Int.add_ (((tuple_get_ (0,!_812_arg) : Int.t), 1L)), ((), (tuple_get_ (1,(tuple_get_ (1,!_812_arg) : (unit*'vv21))) : 'vv20)));
            state_var183 := Q_812
             
         | Q_WAIT162 ->
            _v163 := Lock.is_taken(_736_r_lock);
            (if !_v163 then
               (state_var183 := Q_WAIT162)
            else
              (Lock.acquire(_736_r_lock);
               _736_r_value := _736_r.(Int64.to_int (0L));
               state_var183 := PAUSE_GET161)
              )
             
         | Q_WAIT165 ->
            _v166 := Lock.is_taken(_736_r_lock);
            (if !_v166 then
               (state_var183 := Q_WAIT165)
            else
              (Lock.acquire(_736_r_lock);
               (_736_r.(Int64.to_int (0L)) <- Int.add_ ((!_810, Int.resize_(Char.code_ (!_809),32))));
               state_var183 := PAUSE_SET164)
              )
             
         | Q_WAIT168 ->
            _v169 := Lock.is_taken(_736_r_lock);
            (if !_v169 then
               (state_var183 := Q_WAIT168)
            else
              (Lock.acquire(_736_r_lock);
               _736_r_value := _736_r.(Int64.to_int (0L));
               state_var183 := PAUSE_GET167)
              )
             
         | Q_WAIT171 ->
            _v172 := Lock.is_taken(_714_tab_lock);
            (if !_v172 then
               (state_var183 := Q_WAIT171)
            else
              (Lock.acquire(_714_tab_lock);
               _714_tab_value := _714_tab.(Int64.to_int ((tuple_get_ (0,!_806_arg) : Int.t)));
               state_var183 := PAUSE_GET170)
              )
             
         | Q_WAIT175 ->
            _v176 := Lock.is_taken(_736_r_lock);
            (if !_v176 then
               (state_var183 := Q_WAIT175)
            else
              (Lock.acquire(_736_r_lock);
               (_736_r.(Int64.to_int (0L)) <- 0L);
               state_var183 := PAUSE_SET174)
              )
             
         | Q_WAIT178 ->
            _v179 := Lock.is_taken(_714_tab_lock);
            (if !_v179 then
               (state_var183 := Q_WAIT178)
            else
              (Lock.acquire(_714_tab_lock);
               (_714_tab.(Int64.to_int ((tuple_get_ (0,!_812_arg) : Int.t))) <- Bytes.get_ (432, 8)(((tuple_get_ (1,(tuple_get_ (1,!_812_arg) : (unit*'vv23))) : 'vv22), (tuple_get_ (0,!_812_arg) : Int.t))));
               state_var183 := PAUSE_SET177)
              )
             
         | IDLE160 -> 
           rdy159 := false;
           _796 := Print.string_ ("lecture du fichier `input.txt`");
           _797_by := InputFile.read_file_ (1, 400)(());
           _798 := Bytes.print_ (!_797_by);
           _812_id := 3L;
           _812_arg := (0L, ((), !_797_by));
           state_var183 := Q_812
             
         );
        (if !rdy159 then
           (())
        else
          (result158 := ())
          );
        _788 := (!result158, !rdy159))
      );
      result151 := !_788;
      rdy152 := true;
      state := IDLE153
    );!result151


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

