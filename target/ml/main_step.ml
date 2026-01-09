open Runtime
open Extra
type state_t = IDLE163

type state_var194 = IDLE166  | Q_1016_FINI101
type state_var193 = IDLE170  | Q_1041 | Q_1052 | PAUSE_GET171 | PAUSE_GET177 | PAUSE_GET180 | PAUSE_SET174 | PAUSE_SET184 | PAUSE_SET187 | Q_WAIT172 | Q_WAIT175 | Q_WAIT178 | Q_WAIT181 | Q_WAIT185 | Q_WAIT188
let _937_tab = Array.make 50 (default_char ()) ;;
let _937_tab_lock : Lock.t = (Lock.init ()) ;;
let _937_tab_value = ref (default_char ()) ;;
let _963_r = Array.make 1 0L ;;
let _963_r_lock : Lock.t = (Lock.init ()) ;;
let _963_r_value = ref 0L ;;
let state = ref IDLE163 ;;
let state_var194 = ref IDLE166 ;;let state_var193 = ref IDLE170 ;;
let _1014 : (unit*bool) ref = ref ((),false) ;;
let _1015_w : (unit*bool) ref = ref ((),false) ;;
let result161 : (unit*bool) ref = ref ((),false) ;;
let _1024_by : 'vv16 ref = ref (default_bytes ()) ;;
let _1046 : Int.t ref = ref 0L ;;
let _1037_x : 'vv17 ref = ref (default_bytes ()) ;;
let _1016_fini101_id : Int.t ref = ref 0L ;;
let _1041_id : Int.t ref = ref 0L ;;let _1052_id : Int.t ref = ref 0L ;;
let _1039_n20 : Int.t ref = ref 0L ;;let _1051 : Int.t ref = ref 0L ;;
let _1036 : 'vv18 ref = ref (default_char ()) ;;
let _1045 : 'vv19 ref = ref (default_char ()) ;;
let _1056 : 'vv20 ref = ref (default_char ()) ;;
let _1026 : Int.t ref = ref 0L ;;let _1027_n13 : Int.t ref = ref 0L ;;
let _1030_n : Int.t ref = ref 0L ;;let _1047_n : Int.t ref = ref 0L ;;
let _1048 : Int.t ref = ref 0L ;;let _1049 : Int.t ref = ref 0L ;;
let _1058 : Int.t ref = ref 0L ;;
let _1041_arg : (Int.t*(unit*unit*Int.t)) ref = ref (0L,((),(),0L)) ;;
let __937_tab_lock : bool ref = ref false ;;
let __963_r_lock : bool ref = ref false ;;let _1044 : bool ref = ref false ;;
let _1055 : bool ref = ref false ;;let _v173 : bool ref = ref false ;;
let _v176 : bool ref = ref false ;;let _v179 : bool ref = ref false ;;
let _v182 : bool ref = ref false ;;let _v183 : bool ref = ref false ;;
let _v186 : bool ref = ref false ;;let _v189 : bool ref = ref false ;;
let _v190 : bool ref = ref false ;;let _v192 : bool ref = ref false ;;
let label157 : bool ref = ref false ;;let rdy162 : bool ref = ref false ;;
let rdy165 : bool ref = ref false ;;let rdy169 : bool ref = ref false ;;
let _1016_fini101_arg : unit ref = ref () ;;let _1017 : unit ref = ref () ;;
let _1018 : unit ref = ref () ;;let _1019 : unit ref = ref () ;;
let _1022 : unit ref = ref () ;;let _1023 : unit ref = ref () ;;
let _1025 : unit ref = ref () ;;let _1028 : unit ref = ref () ;;
let _1029 : unit ref = ref () ;;let _1031 : unit ref = ref () ;;
let _1032 : unit ref = ref () ;;let _1033 : unit ref = ref () ;;
let _1034 : unit ref = ref () ;;let _1035 : unit ref = ref () ;;
let _1038 : unit ref = ref () ;;let _1040 : unit ref = ref () ;;
let _1041_result : unit ref = ref () ;;let _1050 : unit ref = ref () ;;
let _1052_result : unit ref = ref () ;;let _1057 : unit ref = ref () ;;
let result164 : unit ref = ref () ;;let result168 : unit ref = ref () ;;
let _1012 : Int.t ref = ref 0L ;;let _1013_cy : Int.t ref = ref 0L ;;
let _1059 : Int.t ref = ref 0L ;;let _1060_cy : Int.t ref = ref 0L ;;
let _1052_arg : (Int.t*(unit*'vv21*Int.t)) ref = ref (0L,((),(default_bytes ()),0L)) ;;

let main_step =
  fun arg ->
    let argument = ref arg in
    (match !state with
    | IDLE163 -> 
      rdy162 := false;
      _1012 := Int.neg_ (1L);
      (if !label157 then
         (())
      else
        (label157 := true;
         _1059 := !_1012)
        );
      _1059 := Int.add_ ((!_1059, 1L));
      _1013_cy := !_1059;
      _v192 := (tuple_get_ (1,!_1014) : bool);
      (if !_v192 then
         ((match !state_var194 with
          | Q_1016_FINI101 ->
             _1016_fini101_arg := ();
             state_var194 := Q_1016_FINI101
              
          | IDLE166 -> 
            rdy165 := false;
            _1060_cy := !_1013_cy;
            _1017 := Print.string_ ("temps d'exécution: ");
            _1018 := Print.int_ (!_1060_cy);
            _1019 := Print.newline_ (());
            _1016_fini101_id := 1L;
            _1016_fini101_arg := ();
            state_var194 := Q_1016_FINI101
              
          );
         (if !rdy165 then
            (())
         else
           (result164 := ())
           );
         _1015_w := (!result164, !rdy165);
         _1014 := ((), true))
      else
        ((match !state_var193 with
         | Q_1041 ->
            _1044 := Int.gt_ (((tuple_get_ (0,!_1041_arg) : Int.t), (tuple_get_ (2,(tuple_get_ (1,!_1041_arg) : (unit*unit*Int.t))) : Int.t)));
            _v183 := !_1044;
            (if !_v183 then
               (_1041_result := ();
                _1040 := !_1041_result;
                _v173 := Lock.is_taken(_963_r_lock);
                (if !_v173 then
                   (state_var193 := Q_WAIT172)
                else
                  (Lock.acquire(_963_r_lock);
                   _963_r_value := _963_r.(Int64.to_int (0L));
                   state_var193 := PAUSE_GET171)
                  ))
            else
              (_v182 := Lock.is_taken(_937_tab_lock);
               (if !_v182 then
                  (state_var193 := Q_WAIT181)
               else
                 (Lock.acquire(_937_tab_lock);
                  _937_tab_value := _937_tab.(Int64.to_int ((tuple_get_ (0,!_1041_arg) : Int.t)));
                  state_var193 := PAUSE_GET180)
                 ))
              )
             
         | Q_1052 ->
            _1055 := Int.gt_ (((tuple_get_ (0,!_1052_arg) : Int.t), (tuple_get_ (2,(tuple_get_ (1,!_1052_arg) : (unit*'vv22*Int.t))) : Int.t)));
            _v190 := !_1055;
            (if !_v190 then
               (_1052_result := ();
                _1028 := !_1052_result;
                _v186 := Lock.is_taken(_963_r_lock);
                (if !_v186 then
                   (state_var193 := Q_WAIT185)
                else
                  (Lock.acquire(_963_r_lock);
                   (_963_r.(Int64.to_int (0L)) <- 0L);
                   state_var193 := PAUSE_SET184)
                  ))
            else
              (_1056 := Bytes.get_ (432, 8)(((tuple_get_ (1,(tuple_get_ (1,!_1052_arg) : (unit*'vv24*Int.t))) : 'vv23), (tuple_get_ (0,!_1052_arg) : Int.t)));
               _v189 := Lock.is_taken(_937_tab_lock);
               (if !_v189 then
                  (state_var193 := Q_WAIT188)
               else
                 (Lock.acquire(_937_tab_lock);
                  (_937_tab.(Int64.to_int ((tuple_get_ (0,!_1052_arg) : Int.t))) <- !_1056);
                  state_var193 := PAUSE_SET187)
                 ))
              )
             
         | PAUSE_GET171 ->
            _1030_n := !_963_r_value;
            Lock.release(_963_r_lock);
            _1031 := Print.newline_ (());
            _1032 := Print.string_ ("somme des caractères: ");
            _1033 := Print.int_ (!_1030_n);
            _1034 := Print.newline_ (());
            _1035 := Print.string_ ("écrit le caractère numéro 42 dans `output.txt`");
            _1036 := Char.chr_ (42L);
            _1037_x := Bytes.make_ (8, 8)(!_1036);
            _1038 := IOFile.write_file_ (("../advent-of-code/output.txt", !_1037_x));
            result168 := Print.newline_ (());
            rdy169 := true;
            state_var193 := IDLE170
             
         | PAUSE_GET177 ->
            _1048 := !_963_r_value;
            Lock.release(_963_r_lock);
            _1049 := Int.add_ ((!_1048, !_1047_n));
            _v176 := Lock.is_taken(_963_r_lock);
            (if !_v176 then
               (state_var193 := Q_WAIT175)
            else
              (Lock.acquire(_963_r_lock);
               (_963_r.(Int64.to_int (0L)) <- !_1049);
               state_var193 := PAUSE_SET174)
              )
             
         | PAUSE_GET180 ->
            _1045 := !_937_tab_value;
            Lock.release(_937_tab_lock);
            _1046 := Char.code_ (!_1045);
            _1047_n := Int.resize_(!_1046,32);
            _v179 := Lock.is_taken(_963_r_lock);
            (if !_v179 then
               (state_var193 := Q_WAIT178)
            else
              (Lock.acquire(_963_r_lock);
               _963_r_value := _963_r.(Int64.to_int (0L));
               state_var193 := PAUSE_GET177)
              )
             
         | PAUSE_SET174 ->
            ();
            Lock.release(_963_r_lock);
            _1050 := ();
            _1051 := Int.add_ (((tuple_get_ (0,!_1041_arg) : Int.t), 1L));
            _1041_arg := (!_1051, ((), (), (tuple_get_ (2,(tuple_get_ (1,!_1041_arg) : (unit*unit*Int.t))) : Int.t)));
            state_var193 := Q_1041
             
         | PAUSE_SET184 ->
            ();
            Lock.release(_963_r_lock);
            _1029 := ();
            _1039_n20 := Int.sub_ ((Int64.of_int (Array.length _937_tab), 1L));
            _1041_id := 2L;
            _1041_arg := (0L, ((), (), !_1039_n20));
            state_var193 := Q_1041
             
         | PAUSE_SET187 ->
            ();
            Lock.release(_937_tab_lock);
            _1057 := ();
            _1058 := Int.add_ (((tuple_get_ (0,!_1052_arg) : Int.t), 1L));
            _1052_arg := (!_1058, ((), (tuple_get_ (1,(tuple_get_ (1,!_1052_arg) : (unit*'vv26*Int.t))) : 'vv25), (tuple_get_ (2,(tuple_get_ (1,!_1052_arg) : (unit*'vv27*Int.t))) : Int.t)));
            state_var193 := Q_1052
             
         | Q_WAIT172 ->
            _v173 := Lock.is_taken(_963_r_lock);
            (if !_v173 then
               (state_var193 := Q_WAIT172)
            else
              (Lock.acquire(_963_r_lock);
               _963_r_value := _963_r.(Int64.to_int (0L));
               state_var193 := PAUSE_GET171)
              )
             
         | Q_WAIT175 ->
            _v176 := Lock.is_taken(_963_r_lock);
            (if !_v176 then
               (state_var193 := Q_WAIT175)
            else
              (Lock.acquire(_963_r_lock);
               (_963_r.(Int64.to_int (0L)) <- !_1049);
               state_var193 := PAUSE_SET174)
              )
             
         | Q_WAIT178 ->
            _v179 := Lock.is_taken(_963_r_lock);
            (if !_v179 then
               (state_var193 := Q_WAIT178)
            else
              (Lock.acquire(_963_r_lock);
               _963_r_value := _963_r.(Int64.to_int (0L));
               state_var193 := PAUSE_GET177)
              )
             
         | Q_WAIT181 ->
            _v182 := Lock.is_taken(_937_tab_lock);
            (if !_v182 then
               (state_var193 := Q_WAIT181)
            else
              (Lock.acquire(_937_tab_lock);
               _937_tab_value := _937_tab.(Int64.to_int ((tuple_get_ (0,!_1041_arg) : Int.t)));
               state_var193 := PAUSE_GET180)
              )
             
         | Q_WAIT185 ->
            _v186 := Lock.is_taken(_963_r_lock);
            (if !_v186 then
               (state_var193 := Q_WAIT185)
            else
              (Lock.acquire(_963_r_lock);
               (_963_r.(Int64.to_int (0L)) <- 0L);
               state_var193 := PAUSE_SET184)
              )
             
         | Q_WAIT188 ->
            _v189 := Lock.is_taken(_937_tab_lock);
            (if !_v189 then
               (state_var193 := Q_WAIT188)
            else
              (Lock.acquire(_937_tab_lock);
               (_937_tab.(Int64.to_int ((tuple_get_ (0,!_1052_arg) : Int.t))) <- !_1056);
               state_var193 := PAUSE_SET187)
              )
             
         | IDLE170 -> 
           rdy169 := false;
           _1022 := Print.string_ ("lecture du fichier `input.txt`");
           _1023 := Print.newline_ (());
           _1024_by := IOFile.read_file_ (256, 400)("../advent-of-code/input.txt");
           _1025 := Bytes.print_ (!_1024_by);
           _1026 := Bytes.len_ (!_1024_by);
           _1027_n13 := Int.sub_ ((!_1026, 1L));
           _1052_id := 3L;
           _1052_arg := (0L, ((), !_1024_by, !_1027_n13));
           state_var193 := Q_1052
             
         );
        (if !rdy169 then
           (())
        else
          (result168 := ())
          );
        _1014 := (!result168, !rdy169))
      );
      result161 := !_1014;
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

