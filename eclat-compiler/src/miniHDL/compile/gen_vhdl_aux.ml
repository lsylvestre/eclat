open MiniHDL_syntax
open Format

let ram_inference = ref true
let memory_initialization = ref false
let intel_max10_target = ref false
let intel_xilinx_target = ref false
let single_read_write_lock_flag = ref true

let has_init_file_ram = ref [] ;;

let size_ty t =
  (* we must canonize [t] to prevent it from being considered as a type variable *)
  MiniHDL_typing.(size_ty (canon t))

(** [size_const c] returns the number of bits of constant [c] *)
let rec size_const c =
  match c with
  | Unit | Bool _ ->
      1
  | Int {value=_;tsize} ->
      size_ty tsize
  | Enum _ ->
      assert false (* cannot infer enum size *)
  | CTuple cs | CVector cs ->
      List.fold_left (fun s c -> s + size_const c) 0 cs
  | CSize _ -> 
      Prelude.Errors.warning (fun fmt ->
        Format.fprintf fmt
          "Size literal detected in the generated code: this is not an immediate value");
      1 (* or 0 ? *)
  | String s -> String.length s * 8
  | C_encode(_,n) ->
      n

(* [reserved x] returns [true] iff [x] is a VHDL keyword
   or a reserved identifier (e.g., reset) *)
let reserved : string -> bool =
  let tbl = Hashtbl.create 20 in
  let () =
    List.iter (fun x -> Hashtbl.add tbl x ()) @@
      [ (*"result"; "argument"; *)"_"; "reset"; "others" ; "value"; "clk"; "loop"; "exit";"next";"rdy";"wait"]
      (* todo: complete with other VHDL keywords *)
  in
  (fun x -> Hashtbl.mem tbl x)

(** [norm_ident x] convert [x] to a valid VHDL identifier *)
let norm_ident x =
  let is_azAZ_19 c =
    (c >= '0' && c <= '9') ||
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') || c = '_'
  in
  let exception E in
  try
    if reserved x || (String.length x > 0 && x.[0] = '_') then raise E;
    String.iter (fun c -> if not (is_azAZ_19 c) then raise E) x;
    x
  with E -> "\\" ^ x ^"\\" (* extended VHDL identifier: anything between backslash *)

(** code generator for identifiers *)
let pp_ident fmt (x:x) : unit =
    fprintf fmt "%s" (norm_ident x)

let pp_state fmt (x:x) : unit =
    pp_ident fmt (String.uppercase_ascii x)

let qualify prefix y =
  prefix^"_"^y

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
      Bytes.set buf pos (if i <= 63 && (n land (1 lsl i) != 0) then '1' else '0') (* I add i <= 63 to avoid incorrect value due to overflow *)
    done;
    Bytes.to_string buf

(* lock-based support for concurrent memory accesses *)
let ptr_taken x = "$"^x^"_lock" 

let decl_locks ?(init=false) fmt x =
  (* if !single_read_write_lock_flag then *)
    fprintf fmt "variable %a : value(0 to 0)%s;@," pp_ident (ptr_taken x)
      (if init then " := (others => '0')" else "")
(* ******************************** *)

let pp_tuple fmt pp vs =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " & ") pp fmt vs

let pp_vector fmt pp vs =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " & ") pp fmt vs


(** code generator for constants *)
let rec pp_c fmt c =
  match c with
  | Unit -> fprintf fmt "eclat_unit"
  | Int {value=n;tsize} ->
      let is_neg = n < 0 in
      let n = abs n in
      let sz = size_ty tsize in
      if is_neg then fprintf fmt "eclat_neg(";
      if sz < 16 then
        fprintf fmt "\"%s\"" (int2bin ~int_size:sz n)
      else
        (let v = Printf.sprintf "%x" n in (* dislay in hexa directly *)
         let l_pad = sz - String.length v * 4 in
         if l_pad = 0 then fprintf fmt "X\"%s\"" v else
         fprintf fmt "%s & X\"%s\"" (const_zero l_pad) v);
      if is_neg then fprintf fmt ")";
  | Bool b ->
      (* notice: in VHDL, eclat_true(0) is valid, but "1"(0) is invalid. *)
      fprintf fmt "%s" (if b then "eclat_true" else "eclat_false")
  | Enum x -> pp_ident fmt x
  | CTuple(cs) ->
       pp_tuple fmt pp_c cs
  | CVector(cs) ->
      pp_vector fmt pp_c cs
  | String s -> fprintf fmt "of_string(\"%s\")" s
  | CSize n -> fprintf fmt "%d" n
  | C_encode(c,n) ->
      fprintf fmt "%a & %s" pp_c c (const_zero (n - size_const c))

(** code generator for tuples deconstruction *)
let rec pp_tuple_access externals fmt (i:int) ty (a:a) : unit =

  let rec tuple_access i ty_a a =
      (* compute bounds of the value to be accessed at index [i_to_find]
         among projections of types ts *)
    let slice_bounds i_to_find ts =
      let rec aux j acc = function
      | [] -> assert false
      | t::ts' -> if i_to_find = j then acc,t,ts' else aux (j+1) (t::acc) ts' in
      aux 0 [] ts
      in
      let open MiniHDL_typing in
      match a,canon ty_a with
      | A_tuple aas,TTuple ts ->
          `Atom(List.nth aas i)
      | A_call(GetTuple(j,_,ty2),a2),TTuple ts ->
          let ts_before,t,_ = slice_bounds i ts in
          (match tuple_access j ty2 a2 with
          | `Slice(a,i1,i2) ->
                let z = size_ty (TTuple ts_before) in
                let tz = size_ty t in
                `Slice(a,z+i1,z+i1+tz-1)
          | `Atom _ as a -> a)
      | _,TTuple ts ->
            let ts_before,t,ts_after = slice_bounds i ts in
            let z' = size_ty (TTuple ts_after) in
            let tot = size_ty (TTuple ts) in
            let j = tot-z'-size_ty t in
            let k = tot - z'-1 in
            `Slice(a,j,k)
      | _,ty2 -> Printf.printf "---> %s\n" (string_of_ty ty_a) ;
                assert false
  in

  match tuple_access i ty a with
  | `Slice(a,j,k) ->
      if j = k then
        (* this case is use to avoid a strange failure (a GHDL bug ?)
           during simulation (overflow detected)
           when using slice x(j to k) of size 1 (i.e., j = k) *)
        fprintf fmt "\"\"&%a(%d)" (pp_a externals) a j
      else
      let pp_slice fmt (j,k) =
        fprintf fmt "%d to %d" j k
      in
      fprintf fmt "%a(%a)" (pp_a externals) a pp_slice (j,k)
  | `Atom(a) -> pp_a externals fmt a


(** code generator for call of operator *)
and pp_call externals fmt (op,a) =
  match op with
  | GetTuple(i,_,ty) -> pp_tuple_access externals fmt i ty a
  | Runtime(Size_of_val(ty,size_int)) -> 
     let n = size_ty (MiniHDL_typing.translate_ty ty) in
     pp_c fmt (Int{value=n;tsize=MiniHDL_typing.translate_size size_int})
 | Runtime(Resize_int sz) ->
      let n = size_ty @@ MiniHDL_typing.translate_size sz in
      fprintf fmt "eclat_resize(%a,%d)" (pp_a externals) a n
 | Runtime(Vector_create sz) ->
      let n = size_ty @@ MiniHDL_typing.translate_size sz in
      fprintf fmt "eclat_vector_make(%d,%a)" n (pp_a externals) a
 (* | Runtime(Vector_length (sz,sz_res)) ->
      (match Types.canon_size sz with
      | Sz_lit n -> pp_c fmt (Int {value=n;tsize=(MiniHDL_typing.translate_size sz_res)})
      | _ -> Types.pp_size Format.std_formatter (Types.canon_size sz); assert false)
   | Runtime(Vector_get t) ->
      fprintf fmt "eclat_vector_get(%a,%d)" (pp_a externals) a (size_ty MiniHDL_typing.(translate_tyB t))
   | Runtime(Vector_update t) ->
      fprintf fmt "eclat_vector_update(%a,%d)" (pp_a externals) a (size_ty MiniHDL_typing.(translate_size t))
 *)
  | Runtime(External_fun (x,ty)) ->
      let annot_with_sizes,arity = match List.assoc_opt x (snd externals) with
                                   | Some (_,(b,n,_)) -> (b,n)
                                   | None -> false,1 in
      (* let rec extract_tyB tyB =
        match Types.canon_tyB tyB with
        | TyB_abstract(_,_sz,tyB_list) ->
            List.map (fun tyB -> MiniHDL_typing.(size_ty (translate_tyB tyB))) tyB_list
        | TyB_var{contents=Is v} -> extract_tyB v
        | _ -> []
      in*)
      let extra = match Types.canon_ty ty with
                  | Ty_fun(Ty_base tyB1,_,tyB2) -> 
                       [size_ty MiniHDL_typing.(translate_tyB tyB1)  
                       ;size_ty MiniHDL_typing.(translate_tyB tyB2)  ]
                  | _ -> assert false
      in
      fprintf fmt "@[work.%s(" x;
      if annot_with_sizes then List.iter (fun n -> fprintf fmt "%d, " n) extra;
      (match a with
      | A_tuple aa when arity > 1 -> 
         fprintf fmt "@[%a)@]"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") (pp_a externals)) aa
      | _ -> fprintf fmt "@[%a)@]" (pp_a externals) a);
      fprintf fmt "@]"
  | Runtime p -> Operators.gen_op fmt p (pp_a externals) a
  | _ -> fprintf fmt "@[%a(%a)@]" pp_op op (pp_a externals) a

(** code generator for operator *)
and pp_op fmt = function
| If -> fprintf fmt "eclat_if"
| Runtime p -> assert false (* deal with in pp_call*)
| TyConstr _ -> fprintf fmt "eclat_id"
| GetTuple (i,_,_) -> assert false (* special case, defined below (see tuple_access) *)

(** code generator for atoms (i.e. combinatorial expression) *)
(* assumes that the let-bindings of atoms are not nested *)
and pp_a externals fmt = function
| A_const c -> pp_c fmt c
| A_var x -> fprintf fmt "%a" pp_ident x
| A_call(op,a) ->
   pp_call externals fmt (op,a)
| A_letIn(x,a1,a2) ->
   assert false (* flattening needed before *) (* fprintf fmt "@[%a := %a;@,%a@]" pp_ident x pp_a externals a1 pp_a externals a2*)
| A_tuple aas -> pp_tuple fmt (pp_a externals) aas
| A_vector aas -> pp_vector fmt (pp_a externals) aas
| A_string_get(s,i) ->
    let i_norm = norm_ident i in
    fprintf fmt "@[%a(to_integer(unsigned(%s&\"000\")) to to_integer(unsigned(%s&\"000\"))+7)@]"
      pp_ident s
      i_norm i_norm
| A_array_get(x,y) ->
    fprintf fmt "@[%a(to_integer(unsigned(%a&\"000\")))@]"
      pp_ident x
      pp_ident y
| A_ptr_taken(x) ->
    pp_ident fmt (ptr_taken x)
| A_array_length(x,tz) ->
    fprintf fmt  "std_logic_vector(to_unsigned(%a'length,%d))" pp_ident x (size_ty tz)
| A_encode(y,ty,n) ->
   fprintf fmt "%a%s" pp_ident y (let m = size_ty ty in if n = m then "" else "&"^const_zero (n-m))
| A_decode(y,ty) ->
   fprintf fmt "%a(0 to %d)" pp_ident y (size_ty ty - 1)


let print_external fmt (n,(ty,shared)) =
  let arg,d,ret = match ty with
  | Types.Ty_fun(arg,d,ret) -> 
      size_ty MiniHDL_typing.(translate_ty arg),d, size_ty MiniHDL_typing.(translate_tyB ret)
  | _ -> assert false
  in
  let instances = match Hashtbl.find_opt Count_externals.external_count n with
                  | None -> Count_externals.IMap.empty | Some v -> v in
  Count_externals.IMap.iter (fun i () ->
      fprintf fmt "signal %s_argument_%d : std_logic_vector(0 to %d) := (others => '0');\n" n i arg;
      fprintf fmt "signal %s_result_%d : std_logic_vector(0 to %d) := (others => '0');\n" n i ret
    ) instances;

  fprintf fmt "@[<v 2>component %s is@, port(@[" n;
  fprintf fmt "signal clk : in std_logic;@,";
  fprintf fmt "signal reset : in std_logic;@,";
  fprintf fmt "signal argument : in std_logic_vector(0 to %d);@," arg;
  fprintf fmt "signal result : out std_logic_vector(0 to %d)@," ret;
  fprintf fmt "@]);@,end component;@,@,@]"


let instantiate_external fmt (n,(_,shared)) =    
  let instances = match Hashtbl.find_opt Count_externals.external_count n with
                  | None -> Count_externals.IMap.empty
                  | Some v -> v in
  Count_externals.IMap.iter (fun i () ->
    fprintf fmt "@[%s_cc_%d : component %s port map (@[" n i n;
    fprintf fmt "clk => clk,@,";
    fprintf fmt "reset => reset,@,";
    fprintf fmt "argument => %s_argument_%d,@," n i;
    fprintf fmt "result => %s_result_%d@," n i;
    fprintf fmt ");@]@,@]"
  ) instances



let variable_decl_go_external fmt (n,(ty,_)) =    
  let ty_arg = match ty with
  | Types.Ty_fun(arg,_,_) -> 
      MiniHDL_typing.(translate_ty arg)
  | _ -> assert false
  in  
  let instances = match Hashtbl.find_opt Count_externals.external_count n with
                  | None ->  Count_externals.IMap.empty
                  | Some v -> v in
  Count_externals.IMap.iter (fun i () ->
    fprintf fmt "variable %s_argument_%d_var : std_logic_vector(0 to %d);@," 
      n i (size_ty ty_arg);
  ) instances

let variable_init_go_external fmt (n,_) =    
  let instances = match Hashtbl.find_opt Count_externals.external_count n with
                  | None ->  Count_externals.IMap.empty
                  | Some v -> v in
  Count_externals.IMap.iter (fun i () ->
    fprintf fmt "%s_argument_%d_var := (others => '0');@," n i;
    (* fprintf fmt "restart_%s_%d := \"0\";@," n i; *)
  ) instances

(* | A_is_rdy(x,id) ->
    fprintf fmt "%s_result_%d(%s_result_%d'length - 1 to %s_result_%d'length - 1)" x id x id x id
| A_get_result(x,id) ->
    fprintf fmt "%s_result_%d(0 to %s_result_%d'length - 2)" x id x id
 *)

let variable_set_go_external fmt (n,_) =    
  let instances = match Hashtbl.find_opt Count_externals.external_count n with
                  | None ->  Count_externals.IMap.empty
                  | Some v -> v in
  Count_externals.IMap.iter (fun i () ->
    (* fprintf fmt "%s_argument_%d(0 to 0) <= restart_%s_%d;@," n i n i; *)
    fprintf fmt "%s_argument_%d <= %s_argument_%d_var;@," n i n i
  ) instances


module ArrayType = Map.Make(struct
    type t = int let compare = Stdlib.compare
  end)

let array_decl fmt x sz_elem n default_value_pp =

  fprintf fmt "signal %a : array_value_%d(0 to %d)" pp_ident x sz_elem (n-1);


  if not(!ram_inference) then (
   fprintf fmt " := (others => %a);@," default_value_pp ()
  ) else (fprintf fmt ";@,";
          if !memory_initialization then (
            if !intel_max10_target then ( 
              (** Intel MAX 10 FPGA device do not support memory initialization.
                (source: https://www.intel.com/content/www/us/en/support/programmable/articles/000074796.html
              *)
              Prelude.Errors.warning (fun fmt ->
                Format.fprintf fmt
                  "Static array %s%a%s (RAM block): Intel MAX 10 FPGA device do not support memory initialization.\n"
                  Prelude.Errors.bold
                  pp_ident x
                  Prelude.Errors.reset)) else (
            fprintf fmt "attribute %a_init_file : string;@," pp_ident x;
            fprintf fmt
               "attribute %a_init_file of %a : signal is \"init_file_%a.mif\";@,"
               pp_ident x
               pp_ident x
               pp_ident x)));

  if !intel_xilinx_target then ( (* attribute for enforcing RAM inference in Xilinx Vivado *)
    fprintf fmt "attribute ram_style of %a : signal is \"block\";@," pp_ident x;
  );

  if List.mem x !has_init_file_ram then fprintf fmt "attribute ram_init_file of %s : signal is \"%s.mif\";@," x x;

  fprintf fmt "signal %a : value(0 to %d) := (others => '0');@," pp_ident ("$"^x^"_value") (sz_elem - 1);
  fprintf fmt "signal %a : natural range 0 to %d := 0;@," pp_ident ("$"^x^"_ptr") (n - 1);
  fprintf fmt "signal %a : natural range 0 to %d := 0;@," pp_ident ("$"^x^"_ptr_write") (n - 1);
  fprintf fmt "signal %a : value(0 to %d) := (others => '0');@," pp_ident ("$"^x^"_write") (sz_elem - 1);
  fprintf fmt "signal %a : std_logic := '0';@," pp_ident ("$"^x^"_write_request")
