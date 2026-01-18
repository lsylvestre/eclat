open OByteLib

let target_code_size = 4096 ;;
let target_globals_rom_size = 2048 ;;

let primitives_list = [ (*"string_length";*)
       "caml_identity";
       
       "caml_sub";
       "caml_mult";
       "caml_div";
       "caml_mod";
       "caml_and";
       "caml_or";
       "caml_xor";
       "caml_lsl";
       "caml_lsr";
       "caml_asr";

       "caml_not";
       "caml_neg";
       "caml_is_int";
       "caml_vectlength";
       "caml_fresh_oo_id";
       "caml_print_int";
       "caml_print_string";
       "caml_print_int";
       "caml_led_off";
       "caml_led_on";
       "caml_equal";
       "caml_lessthan";
       "caml_greaterthan";
       "caml_greaterequal";
       "caml_lessequal";
       "caml_fresh_oo_id";
       "caml_make_vect";
       "caml_obj_dup";
        "bytes_create";
        "caml_array_get";
        "caml_array_set";
        "caml_array_unsafe_set";
        "caml_array_unsafe_get";
       "caml_array_get_addr";
       "caml_array_set_addr";
       "caml_array_unsafe_get_addr";
       "caml_array_unsafe_set_addr";
       "unsafe_chr";
       "bytes_unsafe_set";
       "unsafe_to_string";
       "caml_ml_string_length";
       "caml_create_bytes";
       "caml_blit_string";
       "caml_string_of_bytes";
       "caml_compare";
       "caml_array_sub";
       "caml_string_get";
       "caml_string_equal";
       "caml_print_newline";
 ] ;;


(* let mode_simul = Array.length Sys.argv > 2 && Sys.argv.(2) = "ml" ;;
*)




let load_code_flag = ref false
let load_data_flag = ref false
let ml_syntax_flag = ref false


let main_argument = ref "" ;;

let custom_list = ref primitives_list ;;

(* main configuration *)
let () =
  let others (s:string) : unit =
    main_argument := s
  in
  Arg.parse [
    ("-load-code", Arg.Set load_code_flag,
                 "load bytecode at VM starting");
    ("-load-data", Arg.Set load_data_flag,
                 "load globals at VM starting");
    ("-custom", Arg.String (fun s -> custom_list := !custom_list @ [s]),
                 "add a custom external primitive");
    ("-ml-syntax", Arg.Unit (fun () -> ml_syntax_flag := true;
                                       load_code_flag := true;
                                       load_data_flag := true),
                 "compatbility of the output with OCaml")
    ]
      others "Usage:\n  ./eclat file"
;;


let primitives = Hashtbl.create 100 ;;

let () = 
  List.iteri (fun i x -> Hashtbl.add primitives x i) @@
   !custom_list ;;


let gensym =
  let c = ref 0 in
  (fun () -> incr c; !c)
 
let charlist_of_string s =
  let r = ref [] in
  String.iter (fun c -> r := c::!r) s;
  List.rev !r

let rec add_val oc i v =
  (let open Value in
        match v with
        | Int n -> "val_long("^string_of_int n^")";
        | Block (tag,vs) ->
           let len = Array.length vs+1 in
           let x_num = gensym () in
           Printf.fprintf oc "(* ========= *)\n";
           Printf.fprintf oc "data_rom.(x%d) <- val_long(make_header(%d,%d));\n" x_num tag (len-1);
           Printf.fprintf oc "let x%d = x%d+%d in\n" (x_num+1) x_num len;
           Array.iteri (fun j v ->
              let res = add_val oc i v in
              Printf.fprintf oc "data_rom.(x%d+%d) <- %s;\n" x_num (j+1) res) vs;
         
           "val_ptr(x" ^ string_of_int(x_num) ^ ")"
        | String s -> 
            let vs = List.map (fun c -> Int (Char.code c)) (charlist_of_string s) in
            add_val oc i (Block(252,Array.of_list vs))
        | _ -> failwith "unsupported value type"
        );;

let write_data oc data =
  Printf.fprintf oc "\n\nlet init_data () = (\n";
  if !load_data_flag then (
  let i = ref 0 in
  Printf.fprintf oc "let x1 = global_start + %d in\n" (Array.length data);
  Array.iter (fun v ->
      if !i >= 12 then
        (let n = !i in
        Printf.fprintf oc "(* ADD GLOBAL %d *)\n" n;
        Printf.fprintf oc "data_rom.(global_start + %d) <- %s;\n" n (add_val oc n v));
        incr i;
    ) data;
  ); 

    Printf.fprintf oc "  for i = 0 to %d do\n" (target_globals_rom_size - 1);
    Printf.fprintf oc "    ram.(i) <- data_rom.(i)\n";
    Printf.fprintf oc "  done ) ;;\n"
  


(* reset to 0 ! *)
let gensym =
  let c = ref 0 in
  (fun () -> incr c; !c)

let rec _add_val oc i v =
  (let open Value in
        match v with
        | Int n -> "val_long("^string_of_int n^")";
        | Block (tag,vs) ->
           let len = Array.length vs+1 in
           let x_num = gensym () in
           Printf.fprintf oc "(* ========= *)\n";
           Printf.fprintf oc "print_int x%d; print_string \":\"; print (val_long(make_header(%d,%d))); print_string \";\"; print_newline ();\n" x_num tag (len-1);
           Printf.fprintf oc "let x%d = x%d+%d in\n" (x_num+1) x_num len;
           Array.iteri (fun j v ->
              let res = _add_val oc i v in
              Printf.fprintf oc "print_int (x%d+%d); print_string \":\"; print (%s); print_string \";\"; print_newline ();\n" x_num (j+1) res) vs;
         
           "val_ptr(x" ^ string_of_int(x_num) ^ ")"
        | String s -> 
            let vs = List.map (fun c -> Int (Char.code c)) (charlist_of_string s) in
            _add_val oc i (Block(252,Array.of_list vs))
        | _ -> failwith "unsupported value type"
        );;

let _write_data oc data =
    Printf.fprintf oc  "(* CODE GENERATED FOR CREATING data_rom.mif INITIALIZATION FILE *)\n\n";
 
   Printf.fprintf oc "let init_data () =\nlet global_start = 0 in\n"; 
   Printf.fprintf oc  "  exec\n";

  Printf.fprintf oc  "  print_string \"WIDTH=\"; print_int (size_of_val (val_unit)); print_string \";\"; print_newline ();\n";
  Printf.fprintf oc  "  print_string \"DEPTH=%d;\"; print_newline ();\n" target_globals_rom_size (*!closures_pos+n*);

  Printf.fprintf oc  "print_string \"ADDRESS_RADIX=DEC;\"; print_newline ();";
  Printf.fprintf oc  "print_string \"DATA_RADIX=BIN;\"; print_newline ();";
  Printf.fprintf oc  "print_string \"CONTENT BEGIN\"; print_newline ();";

  Printf.fprintf oc "print_string \"[0..11] : \"; print val_unit; print_string \";\"; print_newline ();\n";

  let i = ref 0 in
  Printf.fprintf oc "let x1 = global_start + %d in\n" (Array.length data);
  Array.iter (fun v ->
      if !i >= 12 then
        (let n = !i in
        Printf.fprintf oc "(* ADD GLOBAL %d *)\n" n;
        Printf.fprintf oc "print_int (global_start + %d); print_string \":\"; print (%s); print_string \";\"; print_newline ();\n" n (_add_val oc n v));
        incr i;
    ) data;

  (* Printf.fprintf oc "print_string \"[%d..%d] : \"; print val_unit; print_string \";\"; print_newline ();\n" (Array.length data+1) (target_globals_rom_size-1);

                NO! *)


  Printf.fprintf oc "print_string \"END;\"; print_newline ();";
  Printf.fprintf oc "(let rec f () = f () in f ()) default ()\n ;;"  
  (* Printf.fprintf oc "  global_end.(0) <- global_start + %d\n ;;\n\n" (Array.length data)*)






(* ********************************** *)

let write_prim oc =
  
  Printf.fprintf oc "let external_call (n,args,env) =\n" ;
  Printf.fprintf oc "  match n with\n";
  List.iteri (fun i s -> Printf.fprintf oc "  | %d -> %s(args,env)\n" i s) !custom_list;
  Printf.fprintf oc "  | _ -> print_string \"unknown primitive\"; (val_unit,env)\n  ;;\n\n\n"
(* ********************************** *)

let process ~src oc ?(pre=(fun _ _ -> ())) f ?(post=(fun _ -> ())) () =
  let ic = open_in src in
  let b = Bytes.create 4 in
  let i = ref 0 in
  pre oc (in_channel_length ic);
  try while true do
    really_input ic b 0 4;
    (* let n = Bytes.get_int16_le b 0 in*)
    (* let n = Int32.(to_int (if compare n zero >= 0 then n else neg n)) in*)
    f oc !i b;
    incr i
  done with _ -> ();
  post oc;
  close_in ic


let _write_code ~version oc code =
  let tmp_code_path = "tmp/code.txt" in
  let oc_tmp = open_out tmp_code_path in
  Code.write version oc_tmp code;
  close_out oc_tmp;
  if !load_code_flag then (
  process ~src:tmp_code_path oc
    ~pre:(fun oc len -> Printf.fprintf oc "let static code = (0:long)^%d ;;\n\n(* let load_code () =\n *)" (len/4))
    (fun oc i b ->
        let n = Int32.to_int (Bytes.get_int32_le b 0) in
        Printf.fprintf oc "  code.(%d) <- %d;\n" i n)
    ~post:(fun oc -> Printf.fprintf oc "  ()\n ;;\n\n")
    ())



let write_code2 ~version:_ ~prim oc oc2 code =
  let l = Normalised_code.of_code code in
  if Array.length l >= target_code_size then (
    Printf.printf "code to large (max %d instructions)" target_code_size;
    exit 0
  );


  let closures_pos = ref (Array.length l) in (* closure_rec_pos *)

  let to_string i =
    let open Printf in
    let open Normalised_instr in
    match i with
    | ACC(n) -> sprintf "ACC(%d)" (n+1)
    | PUSH -> "PUSH()"
    | POP(n) -> sprintf "POP(%d)" n
    | ASSIGN(n) -> sprintf "ASSIGN(%d)" (n+1)
    | ENVACC(n) -> sprintf "GET(ENVACC(),%d)" n
    | PUSH_RETADDR(n) -> sprintf "GROUP4(PUSHRETADDR(%d))" n
    | APPLY(((1|2|3) as s)) -> sprintf "GROUP4(APPLY(true,%d,0))" s
    | APPLY(n) -> sprintf "GROUP4(APPLY(false,%d,0))" n
    | APPTERM(n,s) -> sprintf "GROUP4(APPTERM(%d,%d))" n s
    | RETURN(n) -> sprintf "GROUP4(RETURN(%d))" n
    | RESTART -> "GROUP4(RESTART())"
    | GRAB(n) -> sprintf "GROUP4(GRAB(%d))" n
    | CLOSURE(n,l) -> sprintf "GROUP4(MAKEBLOCK(false,true,%d,closure_tag,%d))" n l
    | CLOSUREREC(v,t) -> 
        let f = Array.length t in
        let o = !closures_pos in
        closures_pos := o + (* 2 **) Array.length t (* + 1*) ;
        (if Array.length t < 256 then () 
         else (Printf.printf "recursive closure starting at position %d \
                              should have at less than 256 mutual recursive values" o; exit 0));
        sprintf "GROUP4(CLOSUREREC(%d,%d,%d,%d))" f v o t.(0)
    | OFFSETCLOSURE(n) -> sprintf "OFFSETCLOSURE(%d)" n
    | GETGLOBAL(n) -> sprintf "GET(GETGLOBAL(),%d)" (n-1)
    | SETGLOBAL(n) -> sprintf "SET(SETGLOBAL(),%d)" (n-1)
    | ATOM(n) -> sprintf "GROUP4(MAKEBLOCK(true,false,1,%d,0))" n
    | MAKEBLOCK(tag,sz) -> sprintf "GROUP4(MAKEBLOCK(false,false,%d,%d,0))" sz tag
    (*| MAKEFLOATBLOCK of int *)
    | GETFIELD(n) -> sprintf "GET(GETFIELD(),%d)" n
    (*| GETFLOATFIELD  of int*)
    | SETFIELD(n) -> sprintf "SET(SETFIELD(),%d)" n
    (*| SETFLOATFIELD  of int *)
    | GETVECTITEM -> "GETVECTITEM()"
    | SETVECTITEM -> "SETVECTITEM()"
    | GETBYTESCHAR -> "GROUP1(GETBYTESCHAR())"
    | SETBYTESCHAR -> "GROUP1(GETSTRINGCHAR())"
    | GETSTRINGCHAR -> "GROUP1(GETSTRINGCHAR())"
    | BRANCH(n) -> sprintf "GROUP3(BRANCH(%d))" n(* 3 *)
    | BRANCHIF(n) -> sprintf "GROUP3(BRANCHIF(true,%d))" n(* 3 *)
    | BRANCHIFNOT(n) -> sprintf "GROUP3(BRANCHIF(false,%d))" n (* 3 *)
    | SWITCH (t1,t2) ->
        let l1 = !closures_pos in
        closures_pos := l1 + Array.length t1 ;
        let l2 = !closures_pos in
        closures_pos := l2 + Array.length t2 ;
        sprintf "GROUP3(SWITCH(%d,%d))" l1 l2
    | PUSHTRAP(n) -> sprintf "GROUP4(PUSHTRAP(%d))" n
    | POPTRAP -> "GROUP4(POPTRAP())"
    | RAISE
    | RERAISE
    | RAISE_NOTRACE -> "GROUP4(RAISE())"
    | CHECK_SIGNALS -> "GROUP3(CHECK_SIGNALS())"
    | C_CALL(narg, idx) ->
        let num = match Hashtbl.find_opt primitives prim.(idx) with
                  | Some n -> n
                  | None -> (Printf.printf "Unknown primitive %s\n" prim.(idx); exit 1) in
        sprintf "GROUP4(CALL(%d,%s))" num (match narg-1 with
                                           | 0 -> "false,false,false,false"
                                           | 1 -> "true,false,false,false"
                                           | 2 -> "true,true,false,false"
                                           | 3 -> "true,true,true,false"
                                           | _ -> "true,true,true,true")
    | CONSTINT(n) -> sprintf "CONST(%d)" n
    | UNAPP(unop) -> 
          (match unop with
           | NOT -> "GROUP4(CALL(11,false,false,false,false))"
           | NEG -> "GROUP4(CALL(12,false,false,false,false))"
           | OFFSET ofs -> sprintf "ADD(true,%d)" ofs
           | ISINT -> "GROUP4(CALL(13,false,false,false,false))"
           | VECTLENGTH -> "GROUP4(CALL(14,false,false,false,false))")
    (* | BINAPP(MUL) -> "MULT()"*)
    | BINAPP(binop) ->
        (match binop with 
        | ADD -> "ADD(false,0)"
        | SUB -> "GROUP4(CALL(1,true,false,false,false))"
        | MUL -> "GROUP4(CALL(2,true,false,false,false))"
        | DIV -> "GROUP4(CALL(3,true,false,false,false))"
        | MOD -> "GROUP4(CALL(4,true,false,false,false))"
        | AND -> "GROUP4(CALL(5,true,false,false,false))"
        | OR ->  "GROUP4(CALL(6,true,false,false,false))"
        | XOR -> "GROUP4(CALL(7,true,false,false,false))"
        | LSL -> "GROUP4(CALL(8,true,false,false,false))"
        | LSR -> "GROUP4(CALL(9,true,false,false,false))"
        | ASR -> "GROUP4(CALL(10,true,false,false,false))")
    | COMPBRANCH(op,n,l) -> 
       let s = match op with
               | ULT | LT -> "LT()" (* U ok ? *)
               | UGE | LE -> "LE()" (* U ok ? *)
               | EQ -> "EQ()"
               | GE -> "GE()"
               | GT -> "GT()"
               | NEQ -> "NEQ()"  in
       sprintf "GROUP_COMPARE(%s,BCOMPARE(%d,%d))" 
          s n l
   
    (*| COMPARE        of compop
    | COMPBRANCH     of compop * int * int*)
    | OFFSETREF(n) -> sprintf "GROUP1(OFFSETREF(%d),0)" n
    | GETMETHOD -> "GETMETHOD()"
    | GETPUBMET(n) -> sprintf "GETPUBMET(%d)" n
    | GETDYNMET -> "GETDYNMET()"
    | STOP -> "GROUP3(STOP())"
  | COMPARE(LT) -> "GROUP_COMPARE(LT(),COMPARE())" 
    | COMPARE(GT) -> "GROUP_COMPARE(GT(),COMPARE())" 
    | COMPARE(NEQ) -> "GROUP_COMPARE(NEQ(),COMPARE())" 
    | COMPARE(EQ) -> "GROUP_COMPARE(EQ(),COMPARE())" 
    | COMPARE(GE) -> "GROUP_COMPARE(GE(),COMPARE())" 
    | COMPARE(LE) -> "GROUP_COMPARE(LE(),COMPARE())" 
    | COMPARE _ -> assert false
    | _ -> Printf.printf "%s" (Normalised_instr.to_string i);  assert false
  in

   (*let n = 
     let r = ref 0 in 
     Array.iter (function Normalised_instr.CLOSUREREC(_,t) -> r := !r + Array.length t * 2 - 1 
                 | Normalised_instr.SWITCH(t1,t2) -> r := !r + Array.length t1 +Array.length t2
                | _ -> ()) l; 
    !r
  in*)
  if !ml_syntax_flag then (
    Printf.fprintf oc "let code = Array.make %d (GROUP3(STOP()):opcode) ;;\n" target_code_size; (*!closures_pos+n*)
    Printf.fprintf oc "let data_rom = Array.make %d (0,false) ;;\n" target_globals_rom_size
  ) else ( 
    Printf.fprintf oc "let static code = (GROUP3(STOP()):opcode)^%d ;;\n" target_code_size (*!closures_pos+n*); 
    Printf.fprintf oc "let static data_rom = ((0,false))^%d ;;\n" target_globals_rom_size 
  );

  Printf.fprintf oc2  "(* CODE GENERATED FOR CREATING code.mif ROM INITIALIZATION FILE *)\n\n";
  Printf.fprintf oc2  "let get_code () =\n";
  Printf.fprintf oc2  "  exec\n";

  Printf.fprintf oc2  "  print_string \"WIDTH=\"; print_int (size_of_val (GROUP3(STOP()))); print_string \";\"; print_newline ();";
  Printf.fprintf oc2  "  print_string \"DEPTH=%d;\"; print_newline ();" target_code_size (*!closures_pos+n*);

  Printf.fprintf oc2  "print_string \"ADDRESS_RADIX=DEC;\"; print_newline ();\n";
  Printf.fprintf oc2  "print_string \"DATA_RADIX=BIN;\"; print_newline ();\n";


  Printf.fprintf oc2  "print_string \"CONTENT BEGIN\"; print_newline ();";
  Array.iteri (fun i c ->  Printf.fprintf oc2  "print_int %d; print_string \": \"; print (%s); print_string \";\"; print_newline ();\n" i (to_string c)) l ;


  let r = ref (Array.length l) in
  Array.iter (fun instr -> 
                 match instr with
                 | Normalised_instr.CLOSUREREC(_,t) ->
                       (* Printf.fprintf oc "  pause(closures.(%d) <- %d);\n" !r pc; incr r;*)
                     (* Printf.fprintf oc "  pause(code.(%d) <- LABEL(%d));\n" !r t.(0); incr r;*)
                     Array.iter (fun n ->
                       Printf.fprintf oc2 "print_int %d; print_string \": \"; print (LABEL(%d)); print_string \";\"; print_newline ();\n" !r n; incr r) t
                 | Normalised_instr.SWITCH(t1,t2) ->
                    Array.iter (fun n ->
                       Printf.fprintf oc2 "print_int %d; print_string \": \"; print (LABEL(%d)); print_string \";\"; print_newline ();\n" !r n; incr r) t1;
                    Array.iter (fun n ->
                       Printf.fprintf oc2  "print_int %d; print_string \": \"; print (LABEL(%d)); print_string \";\"; print_newline ();\n" !r n; incr r) t2
                 | _ -> ()) l;
   Printf.fprintf oc2 "print_string \"END;\"; print_newline ();";
  Printf.fprintf oc2 "(let rec f () = f () in f ()) default ()\n ;;"  ;
 

 (* ************************************************ *)

  if !load_code_flag then (
    Printf.fprintf oc "\n\n let load_code () =\n" ;

  closures_pos := (Array.length l) ; (* reset *)


  Array.iteri (fun i c ->  Printf.fprintf oc "  (code.(%d) <- %s);\n" i (to_string c)) l ;


  let r = ref (Array.length l) in
  Array.iter (fun instr -> 
                 match instr with
                 | Normalised_instr.CLOSUREREC(_,t) ->
                       (* Printf.fprintf oc "  pause(closures.(%d) <- %d);\n" !r pc; incr r;*)
                     (* Printf.fprintf oc "  pause(code.(%d) <- LABEL(%d));\n" !r t.(0); incr r;*)
                     Array.iter (fun n ->
                       Printf.fprintf oc "  (code.(%d) <- LABEL(%d));\n" !r n; incr r) t
                 | Normalised_instr.SWITCH(t1,t2) ->
                    Array.iter (fun n ->
                       Printf.fprintf oc "  (code.(%d) <- LABEL(%d));\n" !r n; incr r) t1;
                    Array.iter (fun n ->
                       Printf.fprintf oc "  (code.(%d) <- LABEL(%d));\n" !r n; incr r) t2
                 | _ -> ()) l;
  Printf.fprintf oc "  ()\n ;;"
 );;

let main () =
  let inpath = !main_argument in
  
  let bytefile = Bytefile.read inpath in

  Bytefile.print stdout bytefile;

  let Bytefile.{version;code;data;prim;_} = bytefile in

  let oc = open_out "../bytecode.ecl" in
  Printf.fprintf oc "(* THIS FILE HAS BEEN GENERATED *)\n\n";
  write_prim oc;

  let oc2 = open_out "../get_code.ecl" in
 
  write_code2 ~version ~prim oc oc2 code;

  let oc3 = open_out "../init_data.ecl" in
  _write_data oc3 data;

  write_data oc data;

  begin 
    Printf.fprintf oc "let main_load () =\n";
    if !load_code_flag then (Printf.fprintf oc "  load_code();\n");

    (* calling init_data is mendatory *)
    Printf.fprintf oc "  init_data();\n";
    
    Printf.fprintf oc "  global_end.(0) <- global_start + %d ;;\n\n" (Array.length data)
  end;


  close_out oc;
  close_out oc2;
  close_out oc3;;


let () = main ()