open Format
open Ast

let has_init_file_ram : string list ref = ref []

let pp_var =
  let seen = Hashtbl.create 100 in
  List.iter (fun x -> Hashtbl.add seen x "")
     [(* Eclat reserved name *)
      "argument";"result"; 
      (* vhdl keyword *)
      "next"; "entity"; "architecture"; "for"; "loop"; "to"; "begin"; "end"; 
      "if"; "then"; "else"; "elsif"; "case"; "when"; "others"; "port"; "map"; 
      "library"; "use"; "is"; "in"; "out"; "signal"; "variable"; "process" 
      ];
  fun fmt x ->
    let x =
      match String.index_from_opt x 0 '_'  with
      | Some j -> 
              let i = j + 1 in 
              if x.[0] = '$' then
              let x' = String.sub x i (String.length x - i) in
              match Hashtbl.find_opt seen x' with
              | Some y -> if y = x then x' else x
              | None -> 
                (Hashtbl.add seen x' x; x')
              else x
      | None -> x
    in
    let x = String.map (function '$' -> 'v' | c -> c) x in 
    if String.for_all (function '1'..'9' | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true | _ -> false) x then
     fprintf fmt "%s" x
    else 
     fprintf fmt "\\%s\\" x

let pp_sig fmt x = pp_var fmt ("sig_"^x)
let pp_next fmt x = pp_var fmt ("next_"^x)

let pp_init_var fmt x = pp_var fmt ("status_" ^ x)
let pp_next_init_var fmt x = pp_var fmt ("next_status_" ^ x)


(* used in comments *)
let pp_inj fmt x = fprintf fmt "%s" @@ String.uppercase_ascii x

let v_result () =
  "v_result"

let const_zero nbits =
  let bits_in_hexa = nbits / 4 in
    let bits_in_binary = nbits mod 4 in
    let make ?(hexa=false) n =
      "\""^(String.make n '0')^"\""
    in
    match bits_in_binary,bits_in_hexa  with
    | 0,n -> "X"^make n
    | n,0 -> make n
    | n,m -> make n^"& X"^make m


let int2bin ~int_size =
  (* from https://discuss.ocaml.org/t/pretty-printing-binary-ints/9062 *)
  let buf = Bytes.create int_size in
  fun n ->
    for i = 0 to int_size - 1 do
      let pos = int_size - 1 - i in
      Bytes.set buf pos (if i <= 63 && (n land (1 lsl i) != 0) then '1' else '0') (* I add i <= 63 to avoid incorrect val
      ue due to overflow *)
    done;
    Bytes.to_string buf


let default_tyB_unknow = Types.TyB_int (Sz_lit 32)

let size_tyB = Types.size_tyB
(* let rec size_const c =
  match c with
  | Int (_,sz) -> size_sz
  | Bool _ | Unit -> 1
  | TyB_tuple cs -> List.fold_left (+) 0 (List.map size_const cs)
  | _ -> 32 (* todo *)
*)

let pp_tyB fmt t =
  fprintf fmt "std_logic_vector(0 to %d)" (size_tyB t - 1)

let rec vhdl_expression = function
| E_var _ | E_const _ | E_app _ -> true
| E_tuple(es) -> List.for_all vhdl_expression es
| E_if(e1,e2,e3) -> vhdl_expression e1 && vhdl_expression e2 && vhdl_expression e3
| _ -> false

let contains_print e = 
  let exception Found in
  let rec loop e =
    match e with
    | E_const(Op(Runtime(Print|Print_string|Print_int|Print_newline))) -> raise Found
    | e -> Ast_mapper.iter loop e
  in
  try (loop e; false) 
  with Found -> true

let gen_get_tuple fmt field tyBs x =
  let pp i sz = fprintf fmt "%a(%d to %d)" pp_var x i (i+sz-1) in
  let rec loop i j = function
  | [] -> ()
  | tyB::tyBs -> if j = field then pp i (size_tyB tyB) else
                     loop (i+size_tyB tyB) (j+1) tyBs
   in loop 0 0 tyBs

let pp_int fmt = function 
   Int (n,sz) ->
      let sz = match Types.canon_size sz with Types.Sz_lit sz -> sz | _ -> 32 in 
      fprintf fmt "\"%s\"" (int2bin ~int_size:sz n)
| _ -> assert false

(** emit a VHDL expression implementing an Eclat constant *)
let rec pp_const fmt c =
  match c with
  | Unit -> fprintf fmt "values.val_unit"
  | Bool b -> fprintf fmt "%s" (if b then "values.val_true" else "values.val_false")
  | Int(n,sz) ->
        let sz = match Types.canon_size sz with Types.Sz_lit sz -> sz | _ -> 32 in 
        let is_neg = n < 0 in
        let n = abs n in
        if is_neg then fprintf fmt "int.neg(";
        if sz < 16 then
          fprintf fmt "values.val_int(\"%s\")" (int2bin ~int_size:sz n)
        else
          (let v = Printf.sprintf "%x" n in (* dislay in hexa directly *)
           let l_pad = sz - String.length v * 4 in
           if l_pad = 0 then fprintf fmt "X\"%s\"" v else
           fprintf fmt "values.val_int(%s & X\"%s\")" (const_zero l_pad) v);
        if is_neg then fprintf fmt ")"
  | C_tuple cs ->
     (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " & ") pp_const) fmt cs
  (* | C_appInj(x,c, tyB) -> *) (*todo*)
  | String s -> fprintf fmt "work.Print.of_string(\"%s\")" s


(** emit a VHDL std_logic_vector of size ~sz
    implementing an Eclat constructor (sum type) of code [n]
*)
let pp_ctor ~sz fmt n =
  fprintf fmt "\"%s\"" @@ int2bin ~int_size:sz n

(** naming convention for accessing ram block *)
let pp_ram_lock fmt x =          pp_var fmt ("$"^x^"_lock")
let pp_ram_write_request fmt x = pp_var fmt ("$"^x^"_write_request")
let pp_ram_ptr fmt x =           pp_var fmt ("$"^x^"_ptr")
let pp_ram_write fmt x =         pp_var fmt ("$"^x^"_write")
let pp_ram_value fmt x =         pp_var fmt ("$"^x^"_value")
let pp_block_ram fmt x =         pp_var fmt (x^"_ram")

(** naming convention for accessing external circuits *)
let pp_external_argument ~l fmt f = pp_var fmt ("argument_" ^ l ^ f)
let external_result ~l f = "result_" ^ l ^ f
let pp_external_result ~l fmt f = pp_var fmt (external_result ~l f)

let externals_instances = Hashtbl.create 10 

let collect_externals_instances e =
  let rec loop e =
    match e with
    | E_run(f,e1,l) ->
       Hashtbl.add externals_instances f l;
       loop e1
    | e -> Ast_mapper.iter loop e
  in 
  loop e


(** given an Eclat operator [op] and a combinational Eclat expression [a],
    emit the corresponding VHDL function or procedure call *)
module Gen_operator : sig
  val gen_op : Operators.op ->
           externals:'a * (string * ('b * (bool * int * 'c))) list ->
           (externals:'a * (string * ('b * (bool * int * 'c))) list ->
            Format.formatter -> Ast.e -> unit) ->
           Format.formatter -> Ast.e -> unit
end = struct
  let operators_gen_op_aux ~externals fmt op pp a =
    let open Operators in
    match op with
    | Resize_int(sz) ->
        let n = match Types.canon_size sz with Sz_lit n -> n | _ -> assert false in
        fprintf fmt "int.resize(%a,%d)" pp a n 
    | Default(tyB) ->
        fprintf fmt "\"%s\"" (String.make (size_tyB tyB) '0')
    | External_fun (x,ty) ->
       (let annot_with_sizes,arity = match List.assoc_opt x (snd externals) with
                                     | Some (_,(b,n,_)) -> (b,n)
                                     | None -> false,1 in
        let extra = match Types.canon_ty ty with
                    | Ty_fun(Ty_base tyB1,_,tyB2) -> 
                         [size_tyB tyB1
                         ;size_tyB tyB2  ]
                    | _ -> assert false
        in
        fprintf fmt "@[work.%s(" x;
        if annot_with_sizes then List.iter (fun n -> fprintf fmt "%d, " n) extra;
        (match a with
        | E_tuple aa when arity > 1 -> 
           fprintf fmt "@[%a)@]"
              (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp) aa
        | _ -> fprintf fmt "@[%a)@]" pp a);
        fprintf fmt "@]"
      )
     | Get_tuple{field;tyBs} ->
        let pp_slice i sz = fprintf fmt "%a(%d to %d)" pp a i (i+sz-1) in
        let rec loop i j a = function
        | [] -> assert false
        | tyB::tyBs -> let sz = Types.size_tyB tyB in
                       if j = field then (match a with
                                          | E_var y -> (i,sz)
                                          | E_app(E_const(Op(Runtime(Operators.Get_tuple{field;tyBs}))),a2) ->
                                              loop 0 j a2 tyBs
                                          | _ -> assert false) else
                       loop (i+sz) (j+1) a tyBs
        in 
        let (i,sz) = loop 0 0 a tyBs in
        pp_slice i sz
     | _ -> Operators.gen_op fmt op pp a 
   
  let gen_op op ~externals pp_a fmt a =
    match op with
    | Operators.Get_tuple{field;tyBs} ->
        (match a with
        | E_var x -> gen_get_tuple fmt field tyBs x
        | _ -> assert false)
   | _ ->
    let pp = match op with    
             | (Operators.Start_read sz |Start_write sz) ->
               (fun ~externals fmt a ->
                 (match a with
                 | Ast.E_tuple (((E_var x)::_) as aa) ->
                    fprintf fmt "%a, %a'length"
                      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",") (pp_a ~externals)) aa
                      pp_ram_value x
                 | _ -> Ast_pprint.pp_exp Format.std_formatter a; flush stdout;  assert false))
              | _ -> pp_a in
    operators_gen_op_aux ~externals fmt op (pp ~externals) a
end


let rec pp_a ~externals fmt a =
  match a with
  | E_var x -> pp_var fmt x 
  | E_const c -> pp_const fmt c
  | E_tuple aa -> (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " & ") (pp_a ~externals)) 
                     fmt aa
  (* | E_app(E_const(Op(Runtime(Get_tuple{field;tyBs}))),E_var x) ->
      gen_get_tuple fmt field tyBs x *)
  | E_app(E_const(Op(Runtime(op))),a) ->
      Gen_operator.gen_op op ~externals pp_a fmt a
  | E_array_length(y) ->
      fprintf fmt "@,std_logic_vector(to_unsigned(%a'length,%a'length))" pp_block_ram y pp_ram_value y
  | e -> Ast_pprint.pp_exp Format.std_formatter e; flush stdout; assert false (* todo *)



let find_ctor x sums =
  let (n,sum,t) = Types.find_ctor x sums in
  let arg_size = List.fold_left (max) 0 @@ List.map (fun (_,t) -> size_tyB t) sum in
  let sz : int = Types.compute_tag_size sum in
  let n = if n = 0 then int_of_float (2. ** float sz) - 1 else n - 1 in
  (n,sz, arg_size,t)


let log2 sz = int_of_float @@ Float.(ceil @@ log2 (float sz))

let array_info ty =
  let sz,tyB = match Types.canon_ty ty with Types.Ty_array (Types.Sz_lit n,tyB) -> n,tyB | _ -> assert false  in
    let n = size_tyB tyB in 
    let log2_sz = log2 sz in
    (sz,n,tyB,log2_sz)



let  pp_static_decl x fmt st = 
    match st with
    (*
    | Static_array(c, n) -> pp_static_decl x fmt @@ Static_array_of ((Types.Ty_array(Types.Sz_lit n,Typing.typ_const ~loc:Prelude.dloc SMap.empty c)),Prelude.dloc)
    *)
    | (Static_array _ | Static_array_of _), ty ->
        let (sz,n,tyB,log2_sz) = array_info ty in
        let id = gensym () in
        fprintf fmt "@,-- block ram %a ***************@," pp_var x;
        fprintf fmt "type \\array_%s\\ is array (0 to %d) of %a;@," id (sz-1) pp_tyB tyB;
        fprintf fmt "signal %a : \\array_%s\\;@," pp_block_ram x id;
        (if List.mem x !has_init_file_ram then 
          fprintf fmt "attribute ram_init_file of %a : signal is \"%s.mif\";" pp_block_ram x x
        );
        fprintf fmt "signal %a, %a : std_logic_vector(0 to %d) := (others => '0');@," pp_sig x pp_next x (2+log2_sz+n+n-1);
        fprintf fmt "signal %a : std_logic := '0';@,"  pp_ram_lock x;
        fprintf fmt "signal %a : std_logic := '0';@,"  pp_ram_write_request x;
        fprintf fmt "signal %a : integer range 0 to %d := 0;@," pp_ram_ptr x (sz-1) ;
        fprintf fmt "signal %a : std_logic_vector(0 to %d) := (others => '0');@," pp_ram_write x (n-1);
        fprintf fmt "signal %a : std_logic_vector(0 to %d) := (others => '0');@," pp_ram_value x (n-1)
    | _ -> assert false


let  pp_static_var_in_process_decl x fmt st = 
    match st with
    | (Static_array _| Static_array_of _), ty ->
        let (sz,n,tyB,log2_sz) = array_info ty in
        fprintf fmt "@,variable %a : std_logic_vector(0 to %d);" pp_var x (2+log2_sz+n+n-1)
    | _ -> assert false

let pp_static_var_in_process_assign fmt x = 
  fprintf fmt "@,%a := %a;" pp_var x pp_sig x

let pp_static_var_in_process_next fmt x = 
  fprintf fmt "@,%a <= %a;" pp_next x pp_var x

let pp_static_update fmt x = 
  fprintf fmt "@,%a <= %a;" pp_sig x pp_next x

let pp_static_config0 fmt x = 
  fprintf fmt "@,%a <= (others => '0');" pp_sig x (* NB: set first bit (lock) to 0 *)



let pp_static x fmt st = 
    match st with
    | (Static_array _| Static_array_of _), ty ->
      let (sz,n,_,log2_sz) = array_info ty in
      fprintf fmt "%a <= %a(0);@," pp_ram_lock x pp_next x ;
      fprintf fmt "%a <= %a(1);@," pp_ram_write_request x pp_next x ;
      fprintf fmt "%a <= to_integer(unsigned(%a(2 to %d)));@," pp_ram_ptr x  pp_next x  (2+log2_sz-1) ; (* resize(%a,%d) log2_sz;*)
      fprintf fmt "%a <= %a(%d to %d);@,"  pp_ram_write x pp_next x (2+log2_sz) (2+log2_sz+n-1);
      fprintf fmt "@[<v 0>process (clk)\
            \ @,begin\
            \ @,  if rising_edge(clk) then\
            \ @,    if %a = '1' then\
            \ @,       %a(%a) <= %a;\
            \ @,    end if;\
            \ @,    %a <= %a(%a);\
            \ @,  end if;\
            \ @,end process;@,@,@]"
          pp_ram_write_request x
          pp_block_ram x
          pp_ram_ptr x
          pp_ram_write x
          pp_ram_value x
          pp_block_ram x
          pp_ram_ptr x
    | _ -> assert false

let rec pp_s ~statics ~externals ~sums p tyB fmt e =
 
 let rec set_ps_ ps ts i fmt ex =
  match ps, ts with
  | [],[] -> ()
  | P_unit::ps, _::ts ->
      set_ps_ ps ts (i+1) fmt ex
  | P_var y::ps, t::ts ->
      let j = (i + size_tyB t) in
      fprintf fmt "@,%a := %a(%d to %d);" pp_var y  (pp_a ~externals) ex  i (j-1); 
      set_ps_ ps ts j fmt ex
  | P_tuple ps1::ps, Types.TyB_tuple tys::ts ->
      set_ps_ (ps1@ps) (tys@ts) i fmt ex

(* assign the value of atomic expression [e] of type [tyB]
   to pattern [p] *)
and set_ ?(assign_symbol=":=") p tyB fmt e =
  if (not @@ vhdl_expression e) then
     (fprintf std_formatter "[%a / %a]\n" Ast_pprint.pp_exp (Pattern.pat2exp p) Ast_pprint.pp_exp e; assert false);
  if (Pattern.pat2exp) p = e then () else (
  match p,Types.canon_tyB tyB,e with
  | P_unit,_, (E_const Unit | E_var _) -> ()
  | _, TyB_unit, E_app(E_const(Op(Runtime(Default _))),_) -> ()
  | _, TyB_unit, E_app(E_const(Op(Runtime(Get_tuple _))),_) -> ()
  | _, TyB_unit, E_app(E_const(Op(Runtime(op))),a) ->
      fprintf fmt "@,%a;" (Gen_operator.gen_op op ~externals pp_a) a
  | _, _, E_app(E_const(Op((Runtime End_read))),a) ->
      (match p,a with
      | P_var x,E_var l ->
         fprintf fmt "@,%a := %a;" pp_var x pp_ram_value l
      | _ -> assert false)  

  | P_var x,TyB_tuple ts, E_tuple es ->
      let rec loop i ts es = match ts,es with
      | [],[] -> ()
      | tyB::ts,e::es -> 
           let j = i + size_tyB tyB in
           fprintf std_formatter "==========>%s(%d to %d) : %a := %a\n" x i j Types.pp_tyB tyB (pp_a ~externals) e;
           fprintf fmt "@[<v 2>%a(%d to %d) := %a@];" pp_var x i (j-1) (pp_a ~externals) e; 
           (if ts <> [] then fprintf fmt "@,");
           loop j ts es in loop 0 ts es
  | P_var x,_, e -> 
      (match e with 
         E_const _ | E_var _ | E_app(E_const(Op _),_) | E_tuple _ -> fprintf fmt "%a := %a;" pp_var x (pp_a ~externals) e
       | E_letIn(p1,ty, e1,e2) ->
           let tyB1 = match Types.canon_ty ty with Types.Ty_base tyB1 -> tyB1 | _ -> default_tyB_unknow  in
           pp_s ~statics ~externals ~sums p1 tyB1 fmt e1; fprintf fmt "@,";
           pp_s ~statics ~externals ~sums p tyB fmt e2
      | _ -> pp_s ~statics ~externals ~sums p tyB fmt e) (* can loop ?*)
      (* | e -> Ast_pprint.pp_exp Format.std_formatter e; assert false) *)
  | P_tuple ps, TyB_tuple ts, E_tuple es -> 
        List.iter2 (fun (p,t) e -> fprintf fmt "@,"; set_ ~assign_symbol p t fmt e) 
            (List.combine ps ts) es
  | P_tuple ps,TyB_tuple ts, (E_var _ as ex) ->
      set_ps_ ps ts 0 fmt ex
  | _ -> fprintf std_formatter "[%a / %a]\n" Ast_pprint.pp_exp (Pattern.pat2exp p) Ast_pprint.pp_exp e; assert false
)

and set_arg ~start p tyB fmt ex =
  match p,Types.canon_tyB tyB with
  | P_unit,_ | _, TyB_unit -> fprintf fmt "@,%a(0 to 0) := values.val_unit;" (pp_a ~externals) ex
  | P_var y,_ -> fprintf fmt "@,@[<v 2>%a := %a(%d to %d)@];"
                       pp_var y (pp_a ~externals) ex start (start+size_tyB tyB-1) 
  | P_tuple ps,TyB_tuple ts ->
      set_ps_ ps ts start fmt ex

in
  match e with
  | E_var _ | E_const _
  | E_tuple _ -> set_ p tyB fmt e
  | Ast.E_if(E_var x,e1,e2) ->
      fprintf fmt "@[<v 2>if bool.is_true(%a) then@,%a@]@,@[<v 2>else@,%a@]@,end if;"
        pp_var x
        (pp_s ~statics ~externals ~sums p tyB) e1
        (pp_s ~statics ~externals ~sums p tyB) e2
  | E_case(E_var x,hs,e_els) ->  
      fprintf fmt "@[<v 0>@[<v 2>case %a is%a@,when others => %a@]@,end case;@]"
        pp_var x
        (fun fmt hs ->
          List.iter (fun (cs,ei) ->
          fprintf fmt "@,when %a => %a" 
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "|") pp_int) cs
            (pp_s ~statics ~externals ~sums p tyB) ei) hs
        ) hs
        (pp_s ~statics ~externals ~sums p tyB) e_els
  | E_match((E_var x as ex),[],Some e') ->
      pp_s ~statics ~externals ~sums p tyB fmt e'
  | E_match((E_var x as ex),hs,eo) ->
      let size_ctor = 
        let witness_inj = match hs with 
                          | (inj,_)::_ -> inj
                          | _ -> assert false in
        let (_,sz,_,_) = find_ctor witness_inj sums in
        sz
      in
      fprintf fmt "@[<v 0>-- begin match ********************@,@[<v 2>case %a(0 to %d) is%a"
        pp_var x
        (size_ctor - 1)
        (fun fmt hs ->
          List.iter (fun (inj,(p',e')) ->
             let n,sz,_,tyB' = find_ctor inj sums in
             fprintf fmt "@,@[<v 2>when %a => @[<hov>-- where %a is %a@]%a@]"
               (pp_ctor ~sz) n
               (pp_ctor ~sz) n
                pp_inj inj
               (fun fmt () ->
                  set_arg ~start:size_ctor p' tyB' fmt ex;
                  fprintf fmt "@,";
                  pp_s ~statics ~externals ~sums p tyB fmt e') ()) hs) hs;
      fprintf fmt "@,@[<v 2>when others =>";
      Option.iter (fun e_else ->
          fprintf fmt "@,";
          pp_s ~statics ~externals ~sums p tyB fmt e_else) eo;
      fprintf fmt "@]@]@,end case;@,-- end match ********************@]"
  | E_letIn(p',ty,e1,e2) ->
      let tyB' = match Types.canon_ty ty with Types.Ty_base tyB' -> tyB' | _ -> default_tyB_unknow in
      pp_s ~statics ~externals ~sums p' tyB' fmt e1;
      (* set_ p' tyB' fmt e1; *) fprintf fmt "@,";
      pp_s ~statics ~externals ~sums p tyB fmt e2
  | E_reg((p',_,e1),e0,l) ->
      let ep' = (Pattern.pat2exp p') in
      fprintf fmt "-- begin reg ********************@,";
      fprintf fmt "@[<v 2>if %a = '0' then@," pp_init_var l;
      pp_s ~statics ~externals ~sums p' tyB fmt e0;
      fprintf fmt "@]@,@[<v 2>else@,";
      (set_ p' tyB) fmt (E_var l);
      fprintf fmt "@]@,end if;@,";
      pp_s ~statics ~externals ~sums p' tyB fmt e1;
      fprintf fmt "@,";
      pp_s ~statics ~externals ~sums p tyB fmt ep';
      fprintf fmt "@,";
      fprintf fmt "%a <= %a;@,%a <= '1';@,-- end reg **********************"  
          pp_next l (pp_a ~externals) ep'
          pp_next_init_var l
  | E_app(E_const (Inj x), a) ->
      let n,sz,_,tyB' = find_ctor x sums in
      (let sz_tyB' = size_tyB tyB' in
        match p with
        | P_var y -> fprintf fmt "@[<v 2>%a(0 to %d) := %a@]; -- %a@," pp_var y (sz-1) (pp_ctor ~sz) n pp_inj x;
                     fprintf fmt "@[<v 2>%a(%d to %d) := %a@];" pp_var y sz (sz + sz_tyB' - 1) (pp_a ~externals) a
        | _ -> assert false)
  | E_app(E_const(Op((Runtime End_read))),a) ->
      (match p,a with
      | P_var x,E_var l ->
         fprintf fmt "@,%a := %a;" pp_var x pp_ram_value l
      | _ -> assert false)  
  | E_app(E_const(Op(Runtime(op))),a) -> (* when combinational *)
     set_ p tyB fmt e

  | E_array_length(y) ->
      (match p with
      | P_var z -> fprintf fmt "@,%a := std_logic_vector(to_unsigned(%a'length,%d));" pp_var z pp_block_ram y (size_tyB tyB)
      | _ -> assert false)
  (*| E_app(E_const(Op(Runtime(op))),a) -> Operators.gen_op fmt op (pp_a ~externals) a (* set_ p tyB fmt e*)
*)
  | E_run(f,a,l) ->
      fprintf fmt "@,%a(0) <= '1';" (pp_external_argument ~l) f;
      fprintf fmt "@,%a(1 to %a'length-1) <= %a;@," (pp_external_argument ~l) f (pp_external_argument ~l) f (pp_a ~externals) a;
      set_ p tyB fmt (E_var (external_result ~l f));
  | e -> Ast_pprint.pp_exp Format.std_formatter e; assert false (* todo *)



let collect_ll e =
  let ll = ref [] in
  let rec loop e =
    match e with
    | E_reg((_,ty,e1),e0,l) ->
       ll := (l,ty)::!ll;
       loop e1; loop e0
    | e -> Ast_mapper.iter loop e
  in 
  loop e; !ll


let collect_local_pat ~sums e =
  let ps = ref [] in
  let rec loop e =
    match e with
    | E_letIn(p,ty,e1,e2) ->
       ps := (p,ty)::!ps;
       loop e1; loop e2
    | E_match(_,hs,oe) ->
       List.iter (fun (inj,(p',e')) -> 
         let n,_,_,tyB = find_ctor inj sums in
         ps := (p',Types.Ty_base tyB)::!ps;
         loop e') hs;
       Option.iter loop oe
    | E_reg((p,tyB,e1),e2,_) ->
        ps := (p,Types.Ty_base tyB)::!ps;
        loop e1; loop e2
    | e -> Ast_mapper.iter loop e
  in 
  loop e; !ps



let def_pat ?(kw="variable") fmt p_list tys = 
  
  let module IMap = Map.Make(Int) in
  let add_among x tyB m =
    let n = size_tyB tyB in
    match IMap.find_opt n m with
    | None -> IMap.add n (tyB,[x]) m
    | Some (ty,l) ->  IMap.add n (tyB, (x::l)) m 
  in
  let imap = ref IMap.empty in
  let rec loop p tyB = match p,tyB with
  | P_unit,_ -> ()
  | P_var x, t -> imap := add_among x t !imap
  | P_tuple ps, Types.TyB_tuple ts -> List.iter2 loop ps ts
  in 
  List.iter2 (fun p ty ->
    let tyB = match Types.canon_ty ty with
            | Ty_base tyB -> tyB
            | _ -> default_tyB_unknow in
    loop p tyB) p_list tys
  ;
  IMap.iter (fun n (tyB, xs) ->
    fprintf fmt "@,%s " kw;
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_var) fmt xs;
    fprintf fmt " : %a;" pp_tyB tyB) !imap 





let pp_pi ~name tyB_argument tyB_result fmt pi =
     (* let fmt = Format.std_formatter in *)
  Hashtbl.clear externals_instances;
  collect_externals_instances pi.main;

  let ll = collect_ll pi.main in

  fprintf fmt "@.@[<v 0>library IEEE;@,";
  fprintf fmt "use IEEE.std_logic_1164.all;@,";
  fprintf fmt "use IEEE.numeric_std.all;@,";
  fprintf fmt "use work.all;@,@,";
  fprintf fmt "use runtime.all;@,@,"; (* todo: remove this depency *)
  fprintf fmt "entity %s is@," name; 
  fprintf fmt "port @[<v 0>(clk : in std_logic;@,\
                         \ reset : in std_logic;@,\
                         \ argument : in %a;@,\
                         \ result : out %a);@]@,\
                          end entity;@," pp_tyB tyB_argument pp_tyB tyB_result; 

  fprintf fmt "@[<v 2>architecture rtl of %s is" name;

    List.iter (fun (l,tyB) -> fprintf fmt "@,signal %a, %a : %a := (others => '0');" pp_var l pp_next l pp_tyB tyB;
                              fprintf fmt "@,signal %a, %a : std_logic := '0';" pp_init_var l pp_next_init_var l) ll;
  
  if !has_init_file_ram <> [] then
    fprintf fmt "@,attribute ram_init_file : string;@,";

  List.iter (fun (x,st) -> pp_static_decl x fmt st) pi.statics;


  let cis_externals,_ = pi.externals in
  List.iter (fun (f,_) ->
    let ty,_ = List.assoc f cis_externals in
    let tyB_arg, tyB_res = match Types.canon_ty ty with
                           | Ty_fun(Ty_base tyB1,_,tyB2) -> tyB1,tyB2
                           | _ -> assert false in
    let insts = Hashtbl.find_all externals_instances f in
    let sz_arg = size_tyB tyB_arg in
    let sz_res = size_tyB tyB_res in
    fprintf fmt "@,component %a is@,\
                 \   port (clk,reset : in std_logic;@,\
                 \         argument  : in std_logic_vector(0 to %d);@,\
                 \         result    : out std_logic_vector(0 to %d));@,\
                 \ end component;" pp_var f (sz_arg-1+1) (sz_res-1);
  
    List.iter (fun l ->
      fprintf fmt "@,-- instance %s of component %s ------------------" l f;
      fprintf fmt "@,signal %a : std_logic_vector(0 to %d) := (others => '0');" (pp_external_argument ~l) f (sz_arg-1+1);
      fprintf fmt "@,signal %a : std_logic_vector(0 to %d) := (others => '0');" (pp_external_result ~l) f (sz_res-1);
      ) insts) cis_externals;




  fprintf fmt "@]@,@[<v 2>begin@,";


  List.iter (fun (x,st) -> pp_static x fmt st) pi.statics;

  Hashtbl.iter (fun f l ->
     fprintf fmt "@,%a: %a port map(clk, reset, %a, %a);@," pp_var l pp_var f 
        (pp_external_argument ~l) f (pp_external_result ~l) f
   ) externals_instances;


  if ll <> [] then begin
   fprintf fmt "@[<v>SEQUENTIAL: process (clk, reset)@,@[<v 2>begin@,\
   \ @[<v 2>if reset = '1' then@,";
   (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,") (fun fmt (l,_) ->
      fprintf fmt "%a <= (others => '0');@,%a <= '0';" pp_var l pp_init_var l)) fmt ll;
  
       List.iter (fun (x,st) -> pp_static_config0 fmt x) pi.statics;
      (if pi.statics <> [] then fprintf fmt "@,");


   fprintf fmt "@]@,@[<v 2>elsif rising_edge(clk) then@,";
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,") (fun fmt (l,_) ->
      fprintf fmt "%a <= %a;@,%a <= %a;" pp_var l pp_next l pp_init_var l pp_next_init_var l)) fmt ll;

      List.iter (fun (x,_) -> pp_static_update fmt x) pi.statics;
      (if pi.statics <> [] then fprintf fmt "@,");

   fprintf fmt "@]@,end if;";
   fprintf fmt "@]@,end process;@]@,@,";
 end;

 fprintf fmt "@[<v 2>COMBINATIONAL: process (argument";
 List.iter (fun (l,_) -> fprintf fmt ", %a, %a" pp_var l pp_init_var l) ll;
 (if pi.statics <> [] then List.iter (fun (x,_) -> fprintf fmt ", %a, %a" pp_sig x pp_ram_value x) pi.statics); 
 (if !Operators.flag_no_print || not(contains_print pi.main) then () else fprintf fmt ", clk");
 
 Hashtbl.iter (fun f l -> 
    fprintf fmt ", %a, %a" (pp_external_argument ~l) f (pp_external_result ~l) f
 ) externals_instances;

 fprintf fmt ")";
 let ps = collect_local_pat ~sums:pi.sums pi.main in
 let ps, tys = List.split ps in
 def_pat ~kw:"variable" fmt ps tys;
 def_pat ~kw:"variable" fmt [(P_var (v_result ()))] [(Types.Ty_base tyB_result)];
 (if pi.statics <> [] then fprintf fmt "@,");
 List.iter (fun (x,st) -> pp_static_var_in_process_decl  x fmt st) pi.statics;
 fprintf fmt "@]@,@[<v 2>begin";
  
  Hashtbl.iter (fun f l -> 
    fprintf fmt "@,%a(0) <= '0';" (pp_external_argument ~l) f
  ) externals_instances;


 List.iter (fun (l,_) ->
    fprintf fmt "@,%a <= %a;" pp_next_init_var l pp_init_var l;
    fprintf fmt "@,%a <= %a;" pp_next l pp_var l) ll;
 if ll <> [] then fprintf fmt "@,------------------" else ();
 fprintf fmt " @,";

 List.iter (fun (x,st) -> pp_static_var_in_process_assign fmt x) pi.statics;
 (if pi.statics <> [] then fprintf fmt "@,");

  pp_s ~statics:pi.statics 
           ~externals:pi.externals ~sums:pi.sums (P_var (v_result ())) 
               tyB_result fmt pi.main;

 List.iter (fun (x,_) -> pp_static_var_in_process_next fmt x) pi.statics;
 (if pi.statics <> [] then fprintf fmt "@,");

  fprintf fmt "@,result <= %a;" pp_var (v_result ());
  fprintf fmt "@]@,end process;@]@,end architecture;@]@,@."; flush stdout;
