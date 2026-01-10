open Extra;;
open Runtime

type state_t = IDLE163

type state_var194 = IDLE166  | Q_1047_FINI101
type state_var193 = IDLE170  | Q_1072 | Q_1083 | PAUSE_GET171 | PAUSE_GET177 | PAUSE_GET180 | PAUSE_SET174 | PAUSE_SET184 | PAUSE_SET187 | Q_WAIT172 | Q_WAIT175 | Q_WAIT178 | Q_WAIT181 | Q_WAIT185 | Q_WAIT188
let _968_tab = Array.make 50 (default_char ()) ;;
let _968_tab_lock : Lock.t = (Lock.init ()) ;;
let _968_tab_value = ref (default_char ()) ;;
let _994_r = Array.make 1 0L ;;
let _994_r_lock : Lock.t = (Lock.init ()) ;;
let _994_r_value = ref 0L ;;
let state = ref IDLE163 ;;
let state_var194 = ref IDLE166 ;;let state_var193 = ref IDLE170 ;;
let _1043 : Int.t ref = ref 0L ;;let _1044_cy : Int.t ref = ref 0L ;;
let _1090 : Int.t ref = ref 0L ;;let _1091_cy : Int.t ref = ref 0L ;;
let _1045 : (unit*bool) ref = ref ((),false) ;;
let _1046_w : (unit*bool) ref = ref ((),false) ;;
let result161 : (unit*bool) ref = ref ((),false) ;;
let _1055_by : 'vv16 ref = ref (default_bytes ()) ;;
let _1077 : Int.t ref = ref 0L ;;
let _1068_x : 'vv17 ref = ref (default_bytes ()) ;;
let _1070_n20 : Int.t ref = ref 0L ;;let _1082 : Int.t ref = ref 0L ;;
let _1047_fini101_id : Int.t ref = ref 0L ;;
let _1072_id : Int.t ref = ref 0L ;;let _1083_id : Int.t ref = ref 0L ;;
let _1067 : 'vv18 ref = ref (default_char ()) ;;
let _1076 : 'vv19 ref = ref (default_char ()) ;;
let _1087 : 'vv20 ref = ref (default_char ()) ;;
let _1057 : Int.t ref = ref 0L ;;let _1058_n13 : Int.t ref = ref 0L ;;
let _1061_n : Int.t ref = ref 0L ;;let _1078_n : Int.t ref = ref 0L ;;
let _1079 : Int.t ref = ref 0L ;;let _1080 : Int.t ref = ref 0L ;;
let _1089 : Int.t ref = ref 0L ;;
let _1072_arg : (Int.t*(unit*unit*Int.t)) ref = ref (0L,((),(),0L)) ;;
let __968_tab_lock : bool ref = ref false ;;
let __994_r_lock : bool ref = ref false ;;let _1075 : bool ref = ref false ;;
let _1086 : bool ref = ref false ;;let _v173 : bool ref = ref false ;;
let _v176 : bool ref = ref false ;;let _v179 : bool ref = ref false ;;
let _v182 : bool ref = ref false ;;let _v183 : bool ref = ref false ;;
let _v186 : bool ref = ref false ;;let _v189 : bool ref = ref false ;;
let _v190 : bool ref = ref false ;;let _v192 : bool ref = ref false ;;
let label157 : bool ref = ref false ;;let rdy162 : bool ref = ref false ;;
let rdy165 : bool ref = ref false ;;let rdy169 : bool ref = ref false ;;
let _1047_fini101_arg : unit ref = ref () ;;let _1048 : unit ref = ref () ;;
let _1049 : unit ref = ref () ;;let _1050 : unit ref = ref () ;;
let _1053 : unit ref = ref () ;;let _1054 : unit ref = ref () ;;
let _1056 : unit ref = ref () ;;let _1059 : unit ref = ref () ;;
let _1060 : unit ref = ref () ;;let _1062 : unit ref = ref () ;;
let _1063 : unit ref = ref () ;;let _1064 : unit ref = ref () ;;
let _1065 : unit ref = ref () ;;let _1066 : unit ref = ref () ;;
let _1069 : unit ref = ref () ;;let _1071 : unit ref = ref () ;;
let _1072_result : unit ref = ref () ;;let _1081 : unit ref = ref () ;;
let _1083_result : unit ref = ref () ;;let _1088 : unit ref = ref () ;;
let result164 : unit ref = ref () ;;let result168 : unit ref = ref () ;;
let _1083_arg : (Int.t*(unit*'vv21*Int.t)) ref = ref (0L,((),(default_bytes ()),0L)) ;;

let main_step =
  fun arg ->
    let argument = ref arg in
    (match !state with
    | IDLE163 -> 
      rdy162 := false;
      _1043 := Int.neg_ (1L);
      (if !label157 then
         (())
      else
        (label157 := true;
         _1090 := !_1043)
        );
      _1090 := Int.add_ ((!_1090, 1L));
      _1044_cy := !_1090;
      _v192 := (tuple_get_ (1,!_1045) : bool);
      (if !_v192 then
         ((match !state_var194 with
          | Q_1047_FINI101 ->
             _1047_fini101_arg := ();
             state_var194 := Q_1047_FINI101
              
          | IDLE166 -> 
            rdy165 := false;
            _1091_cy := !_1044_cy;
            _1048 := Print.string_ ("temps d'exécution: ");
            _1049 := Print.int_ (!_1091_cy);
            _1050 := Print.newline_ (());
            _1047_fini101_id := 1L;
            _1047_fini101_arg := ();
            state_var194 := Q_1047_FINI101
              
          );
         (if !rdy165 then
            (())
         else
           (result164 := ())
           );
         _1046_w := (!result164, !rdy165);
         _1045 := ((), true))
      else
        ((match !state_var193 with
         | Q_1072 ->
            _1075 := Int.gt_ (((tuple_get_ (0,!_1072_arg) : Int.t), (tuple_get_ (2,(tuple_get_ (1,!_1072_arg) : (unit*unit*Int.t))) : Int.t)));
            _v183 := !_1075;
            (if !_v183 then
               (_1072_result := ();
                _1071 := !_1072_result;
                _v173 := Lock.is_taken(_994_r_lock);
                (if !_v173 then
                   (state_var193 := Q_WAIT172)
                else
                  (Lock.acquire(_994_r_lock);
                   _994_r_value := _994_r.(Int64.to_int (0L));
                   state_var193 := PAUSE_GET171)
                  ))
            else
              (_v182 := Lock.is_taken(_968_tab_lock);
               (if !_v182 then
                  (state_var193 := Q_WAIT181)
               else
                 (Lock.acquire(_968_tab_lock);
                  _968_tab_value := _968_tab.(Int64.to_int ((tuple_get_ (0,!_1072_arg) : Int.t)));
                  state_var193 := PAUSE_GET180)
                 ))
              )
             
         | Q_1083 ->
            _1086 := Int.gt_ (((tuple_get_ (0,!_1083_arg) : Int.t), (tuple_get_ (2,(tuple_get_ (1,!_1083_arg) : (unit*'vv22*Int.t))) : Int.t)));
            _v190 := !_1086;
            (if !_v190 then
               (_1083_result := ();
                _1059 := !_1083_result;
                _v186 := Lock.is_taken(_994_r_lock);
                (if !_v186 then
                   (state_var193 := Q_WAIT185)
                else
                  (Lock.acquire(_994_r_lock);
                   (_994_r.(Int64.to_int (0L)) <- 0L);
                   state_var193 := PAUSE_SET184)
                  ))
            else
              (_1087 := Bytes.get_ (432, 8)(((tuple_get_ (1,(tuple_get_ (1,!_1083_arg) : (unit*'vv24*Int.t))) : 'vv23), (tuple_get_ (0,!_1083_arg) : Int.t)));
               _v189 := Lock.is_taken(_968_tab_lock);
               (if !_v189 then
                  (state_var193 := Q_WAIT188)
               else
                 (Lock.acquire(_968_tab_lock);
                  (_968_tab.(Int64.to_int ((tuple_get_ (0,!_1083_arg) : Int.t))) <- !_1087);
                  state_var193 := PAUSE_SET187)
                 ))
              )
             
         | PAUSE_GET171 ->
            _1061_n := !_994_r_value;
            Lock.release(_994_r_lock);
            _1062 := Print.newline_ (());
            _1063 := Print.string_ ("somme des caractères: ");
            _1064 := Print.int_ (!_1061_n);
            _1065 := Print.newline_ (());
            _1066 := Print.string_ ("écrit le caractère numéro 42 dans `output.txt`");
            _1067 := Char.chr_ (42L);
            _1068_x := Bytes.make_ (8, 8)(!_1067);
            _1069 := IOFile.write_file_ (("../advent-of-code/output.txt", !_1068_x));
            result168 := Print.newline_ (());
            rdy169 := true;
            state_var193 := IDLE170
             
         | PAUSE_GET177 ->
            _1079 := !_994_r_value;
            Lock.release(_994_r_lock);
            _1080 := Int.add_ ((!_1079, !_1078_n));
            _v176 := Lock.is_taken(_994_r_lock);
            (if !_v176 then
               (state_var193 := Q_WAIT175)
            else
              (Lock.acquire(_994_r_lock);
               (_994_r.(Int64.to_int (0L)) <- !_1080);
               state_var193 := PAUSE_SET174)
              )
             
         | PAUSE_GET180 ->
            _1076 := !_968_tab_value;
            Lock.release(_968_tab_lock);
            _1077 := Char.code_ (!_1076);
            _1078_n := Int.resize_(!_1077,32);
            _v179 := Lock.is_taken(_994_r_lock);
            (if !_v179 then
               (state_var193 := Q_WAIT178)
            else
              (Lock.acquire(_994_r_lock);
               _994_r_value := _994_r.(Int64.to_int (0L));
               state_var193 := PAUSE_GET177)
              )
             
         | PAUSE_SET174 ->
            ();
            Lock.release(_994_r_lock);
            _1081 := ();
            _1082 := Int.add_ (((tuple_get_ (0,!_1072_arg) : Int.t), 1L));
            _1072_arg := (!_1082, ((), (), (tuple_get_ (2,(tuple_get_ (1,!_1072_arg) : (unit*unit*Int.t))) : Int.t)));
            state_var193 := Q_1072
             
         | PAUSE_SET184 ->
            ();
            Lock.release(_994_r_lock);
            _1060 := ();
            _1070_n20 := Int.sub_ ((Int64.of_int (Array.length _968_tab), 1L));
            _1072_id := 2L;
            _1072_arg := (0L, ((), (), !_1070_n20));
            state_var193 := Q_1072
             
         | PAUSE_SET187 ->
            ();
            Lock.release(_968_tab_lock);
            _1088 := ();
            _1089 := Int.add_ (((tuple_get_ (0,!_1083_arg) : Int.t), 1L));
            _1083_arg := (!_1089, ((), (tuple_get_ (1,(tuple_get_ (1,!_1083_arg) : (unit*'vv26*Int.t))) : 'vv25), (tuple_get_ (2,(tuple_get_ (1,!_1083_arg) : (unit*'vv27*Int.t))) : Int.t)));
            state_var193 := Q_1083
             
         | Q_WAIT172 ->
            _v173 := Lock.is_taken(_994_r_lock);
            (if !_v173 then
               (state_var193 := Q_WAIT172)
            else
              (Lock.acquire(_994_r_lock);
               _994_r_value := _994_r.(Int64.to_int (0L));
               state_var193 := PAUSE_GET171)
              )
             
         | Q_WAIT175 ->
            _v176 := Lock.is_taken(_994_r_lock);
            (if !_v176 then
               (state_var193 := Q_WAIT175)
            else
              (Lock.acquire(_994_r_lock);
               (_994_r.(Int64.to_int (0L)) <- !_1080);
               state_var193 := PAUSE_SET174)
              )
             
         | Q_WAIT178 ->
            _v179 := Lock.is_taken(_994_r_lock);
            (if !_v179 then
               (state_var193 := Q_WAIT178)
            else
              (Lock.acquire(_994_r_lock);
               _994_r_value := _994_r.(Int64.to_int (0L));
               state_var193 := PAUSE_GET177)
              )
             
         | Q_WAIT181 ->
            _v182 := Lock.is_taken(_968_tab_lock);
            (if !_v182 then
               (state_var193 := Q_WAIT181)
            else
              (Lock.acquire(_968_tab_lock);
               _968_tab_value := _968_tab.(Int64.to_int ((tuple_get_ (0,!_1072_arg) : Int.t)));
               state_var193 := PAUSE_GET180)
              )
             
         | Q_WAIT185 ->
            _v186 := Lock.is_taken(_994_r_lock);
            (if !_v186 then
               (state_var193 := Q_WAIT185)
            else
              (Lock.acquire(_994_r_lock);
               (_994_r.(Int64.to_int (0L)) <- 0L);
               state_var193 := PAUSE_SET184)
              )
             
         | Q_WAIT188 ->
            _v189 := Lock.is_taken(_968_tab_lock);
            (if !_v189 then
               (state_var193 := Q_WAIT188)
            else
              (Lock.acquire(_968_tab_lock);
               (_968_tab.(Int64.to_int ((tuple_get_ (0,!_1083_arg) : Int.t))) <- !_1087);
               state_var193 := PAUSE_SET187)
              )
             
         | IDLE170 -> 
           rdy169 := false;
           _1053 := Print.string_ ("lecture du fichier `input.txt`");
           _1054 := Print.newline_ (());
           _1055_by := IOFile.read_file_ (256, 400)("../advent-of-code/input.txt");
           _1056 := Bytes.print_ (!_1055_by);
           _1057 := Bytes.len_ (!_1055_by);
           _1058_n13 := Int.sub_ ((!_1057, 1L));
           _1083_id := 3L;
           _1083_arg := (0L, ((), !_1055_by, !_1058_n13));
           state_var193 := Q_1083
             
         );
        (if !rdy169 then
           (())
        else
          (result168 := ())
          );
        _1045 := (!result168, !rdy169))
      );
      result161 := !_1045;
      rdy162 := true;
      state := IDLE163
    );!result161


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

