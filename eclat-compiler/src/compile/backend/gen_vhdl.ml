open Fsm_syntax
open Format

let ram_inference = ref false
let intel_max10_target = ref false

let size_ty t =
  (* we must canonising [t] to prevent it from being considered as a type variable *)
  Fsm_typing.(size_ty (canon t))

(** [size_const c] returns the number of bits of constant [c] *)
let rec size_const c =
  match c with
  | Unit | Bool _ ->
      1
  | Int {value=_;tsize} ->
      size_ty tsize
  | Enum _ ->
      assert false (* cannot infer enum size *)
  |CTuple cs ->
      List.fold_left (fun s c -> s + size_const c) 0 cs
  | String s -> String.length s * 8

(* [reserved x] returns [true] iff [x] is a VHDL keyword
   or a reserved identifier (e.g., reset) *)
let reserved : string -> bool =
  let tbl = Hashtbl.create 20 in
  let () =
    List.iter (fun x -> Hashtbl.add tbl x ()) @@
      [ "_"; "reset"; "others"; "run" ; "value"; "clk"; "loop"; "exit";"next";"rdy";"wait"]
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

let pp_tuple fmt pp vs =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " & ") pp fmt vs

(** code generator for constants *)
let rec pp_c fmt c =
  match c with
  | Unit -> fprintf fmt "eclat_unit"
  | Int {value=n;tsize} ->
      let is_neg = n < 0 in
      let n = abs n in
      let v = Printf.sprintf "%x" n in
      let l_pad = size_ty tsize - String.length v * 4 in
      if l_pad < 0 then
      begin
        assert false (** should not happen ! *)
      end;
      if is_neg then fprintf fmt "eclat_neg(";
      if l_pad = 0 then fprintf fmt "X\"%s\"" v else
      fprintf fmt "%s & X\"%s\"" (const_zero l_pad) v;
      if is_neg then fprintf fmt ")";
  | Bool b ->
      (* notice: in VHDL, eclat_true(0) is valid, but "1"(0) is invalid. *)
      fprintf fmt "%s" (if b then "eclat_true" else "eclat_false")
  | Enum x -> pp_ident fmt x
  | CTuple(cs) ->
      pp_tuple fmt pp_c cs
  | String s -> fprintf fmt "of_string(\"%s\")" s

(** code generator for tuples deconstruction *)
let rec pp_tuple_access fmt (i:int) ty (a:a) : unit =

  let rec tuple_access i ty_a a =
      (* compute bounds of the value to be accessed at index [i_to_find]
         among projections of types ts *)
    let slice_bounds i_to_find ts =
      let rec aux j acc = function
      | [] -> assert false
      | t::ts' -> if i_to_find = j then acc,t,ts' else aux (j+1) (t::acc) ts' in
      aux 0 [] ts
      in
      let open Fsm_typing in
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
        fprintf fmt "\"\"&%a(%d)" pp_a a j
      else
      let pp_slice fmt (j,k) =
        fprintf fmt "%d to %d" j k
      in
      fprintf fmt "%a(%a)" pp_a a pp_slice (j,k)
  | `Atom(a) -> pp_a fmt a


(** code generator for call of operator *)
and pp_call fmt (op,a) =
  match op with
  | GetTuple(i,_,ty) -> pp_tuple_access fmt i ty a
  | Compute_address -> fprintf fmt "eclat_compute_address(caml_heap_base,%a)" pp_a a
  | Runtime p -> Operators.gen_op fmt p pp_a a
  | _ -> fprintf fmt "@[%a(%a)@]" pp_op op pp_a a

(** code generator for operator *)
and pp_op fmt = function
| If -> fprintf fmt "eclat_if"
| Runtime p -> assert false (* deal with in pp_call*)
| TyConstr _ -> fprintf fmt "eclat_id"
| GetTuple (i,_,_) -> assert false (* special case, defined below (see tuple_access) *)
| Compute_address -> assert false (* deal with in pp_call*)


(** code generator for atoms (i.e. combinatorial expression) *)
(* assumes that the let-bindings of atoms are not nested *)
and pp_a fmt = function
| A_const c -> pp_c fmt c
| A_var x -> fprintf fmt "%a" pp_ident x
| A_call(op,a) ->
   pp_call fmt (op,a)
| A_letIn(x,a1,a2) ->
   assert false (* flattening needed before *) (* fprintf fmt "@[%a := %a;@,%a@]" pp_ident x pp_a a1 pp_a a2*)
| A_tuple aas -> pp_tuple fmt pp_a aas
| A_string_get(s,i) ->
    fprintf fmt "@[%a(to_integer(unsigned(%s&\"000\")) to to_integer(unsigned(%s&\"000\"))+7)@]" pp_ident s i i
| A_buffer_get(xb) ->
    pp_ident fmt ("$"^xb^"_value")
| A_ptr_taken(x) ->
    pp_ident fmt ("$"^x^"_ptr_take")
| A_ptr_write_taken(x) ->
    pp_ident fmt ("$"^x^"_ptr_write_take")
| A_buffer_length(x,tz) ->
    fprintf fmt  "std_logic_vector(to_unsigned(%a'length,%d))" pp_ident x (size_ty tz)
| A_encode(y,ty,n) ->
   fprintf fmt "%a%s" pp_ident y (let m = size_ty ty in if n = m then "" else "&"^const_zero (n-m))
| A_decode(y,ty) ->
   fprintf fmt "%a(0 to %d)" pp_ident y (size_ty ty - 1)


(** code generator for statements *)
let rec pp_s ~st fmt = function
| S_skip -> ()
| S_continue q -> fprintf fmt "%a <= %a;" pp_ident st pp_ident q
| S_if(z,s1,so) ->
    fprintf fmt "@[<v 2>if %a(0) = '1' then@,%a@]" pp_ident z (pp_s ~st) s1;
    Option.iter (fun s2 -> fprintf fmt "@,@[<v 2>else@,%a@]" (pp_s ~st) s2) so;
     fprintf fmt "@,end if;"
| S_case(y,hs,so) ->
    fprintf fmt "@[<v>case %a is@," pp_ident y;
    List.iter (fun (c,s) -> fprintf fmt "@[<v 2>when %a =>@,%a@]@," pp_c c (pp_s ~st) s) hs;
    Option.iter (fun s ->
      fprintf fmt "@[<v 2>when others =>@,%a@]@," (pp_s ~st) s) so;
    fprintf fmt "@]end case;";
| S_set(x,a) -> fprintf fmt "@[<v>%a := %a;@]" pp_ident x pp_a a
| S_setptr(x,idx) -> (* todo: avoid code duplication between S_setptr & S_setptr_write *)
    (match idx with
    | A_const(Int{value=n}) ->
       fprintf fmt
         "@[%a <= %d;@]" pp_ident ("$"^x^"_ptr") n
    | _ ->
       fprintf fmt
         "@[%a <= to_integer(unsigned(%a));@]" pp_ident ("$"^x^"_ptr") pp_a idx)
| S_ptr_take(x,b) ->
    fprintf fmt
      "@[%a(0) := '%d';@]" pp_ident ("$"^x^"_ptr_take") (if b then 1 else 0)
| S_ptr_write_take(x,b) ->
    fprintf fmt
      "@[%a(0) := '%d';@]" pp_ident ("$"^x^"_ptr_write_take") (if b then 1 else 0)
| S_setptr_write(x,idx,a) ->
    (match idx with
    | A_const(Int{value=n}) ->
       fprintf fmt
         "@[%a <= %d;@]@," pp_ident ("$"^x^"_ptr_write") n;
    | _ ->
       fprintf fmt
         "@[%a <= to_integer(unsigned(%a));@]@," pp_ident ("$"^x^"_ptr_write") pp_a idx);
    fprintf fmt
         "@[%a <= '1';@]@," pp_ident ("$"^x^"_write_request");
    fprintf fmt
      "@[%a <= %a;@]" pp_ident ("$"^x^"_write") pp_a a;
| S_buffer_set(x) ->
    fprintf fmt
      "@[%a <= '0';@]" pp_ident ("$"^x^"_write_request")
| S_seq(S_skip,s) | S_seq(s,S_skip) -> pp_s ~st fmt s
| S_seq(s1,s2) -> fprintf fmt "@[<v>%a@,%a@]" (pp_s ~st) s1 (pp_s ~st) s2
| S_letIn(x,a,s) -> fprintf fmt "@[<v>%a := %a;@,%a@]" pp_ident x pp_a a (pp_s ~st) s
| S_fsm(id,rdy,x,cp,ts,s,b) ->
     let (st2,_,_) = List.assoc id !List_machines.extra_machines in
     pp_fsm fmt ~restart:b ~state_var:st2 ~compute:cp ~rdy (id,ts,s)
| S_in_fsm(id,s) ->
     let (st2,_,_) = List.assoc id !List_machines.extra_machines in
     pp_s ~st:st2 fmt s
| S_call(op,a) ->
   fprintf fmt "%a;@," pp_call (Runtime(op),a)

(** code generator for FSMs *)
and pp_fsm fmt ~restart ~state_var:st ~compute ~rdy (id,ts,s) =
    if restart then (
      fprintf fmt "@[<v>case %a is@," pp_ident st;
      List.iter (fun (x,s) -> fprintf fmt "@[<v 2>when %a =>@,%a@]@," pp_ident x (pp_s ~st) s) ts;
      fprintf fmt "@[<v 2>when %a =>@,%a@]" pp_ident compute (pp_s ~st) s;
      fprintf fmt "@]@,end case;")
    else (
    fprintf fmt "@[<v>case %a is@," pp_ident st;
    List.iter (fun (x,s) -> fprintf fmt "@[<v 2>when %a =>@,%a@]@," pp_ident x (pp_s ~st) s) ts;
    fprintf fmt "@[<v 2>when %a =>@,%a@]@," pp_ident compute (pp_s ~st) s;
    fprintf fmt "@]end case;"
   )

(* default value as bitvector where each bit is at '0' *)
let default_zero_value nbits =
  "(others => '0')"

(* default value according to the given type. *)
let default_zero t =
  match Fsm_typing.canon t with
  | TStatic _ -> "(others => (others => '0'))"
  | _ -> "(others => '0')"

let qualify prefix y =
  prefix^"_"^y

let declare_state_var fmt state_var compute xs =
  let state_var_tname = Naming_convention.state_var_type state_var in
    fprintf fmt "type %a is (%a" pp_ident state_var_tname pp_ident compute;

    List.iter (fun x -> fprintf fmt ", %a" pp_ident x) xs;

    fprintf fmt ");@,signal %a: %a;@," pp_ident state_var pp_ident state_var_tname



let declare_machine fmt ~state_var ~compute ~infos (ts,s) =

  declare_state_var fmt state_var compute (List.map fst ts);

  List.iter (fun (_,(sv,cp,xs)) -> declare_state_var fmt sv cp xs) !List_machines.extra_machines;

  Fsm_comp.SMap.iter (fun x w ->
    let inst_tname = Naming_convention.instances_type x in
    let sq = Fsm_comp.IMap.to_seq w in
    let l = (List.of_seq sq) in
    begin
      fprintf fmt "type %a is (" pp_ident inst_tname ;
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
        (fun fmt (n,_) -> fprintf fmt "%a" pp_ident (Naming_convention.instance_enum_const n)) fmt l;
      fprintf fmt ");@,";
      fprintf fmt "signal %a : %a;@," pp_ident (Naming_convention.instance_id_of_fun x) pp_ident inst_tname
    end
  ) infos
(* type array_value is array (0 to 20) of value(0 to 31); *)
let pp_ty fmt t =
  match Fsm_typing.canon t with
  | TStatic{elem;size} -> fprintf fmt "array_value_%d(0 to %d)" (size_ty elem) (size_ty size - 1);
  | _ ->
      fprintf fmt "value(0 to %d)" (size_ty t-1)




module ArrayType = Map.Make(struct
    type t = int let compare = Stdlib.compare
  end)

let declare_variable ~argument ~statics typing_env fmt =
  let var_decls = Hashtbl.create 10 in
  let add_var x n =
    match Hashtbl.find_opt var_decls n with
    | None -> Hashtbl.add var_decls n [x]
    | Some s -> Hashtbl.replace var_decls n (x::s)
  in
  Hashtbl.iter (fun x t ->
      if x <> argument && not (List.mem_assoc x statics) then
          add_var x (size_ty t)
    ) typing_env;

  Hashtbl.iter (fun n xs ->
      (* Notice there is a default value ``0'' *)
      fprintf fmt "variable @[<v>@[<hov>%a@] : value(0 to %d) := (others => '0');@]@,"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ @,") pp_ident) xs (n-1)
    ) var_decls


(* code generator for the whole design *)
let pp_component fmt ~vhdl_comment ~name ~state_var ~argument ~result ~compute ~rdy ~statics typing_env infos (ts,s) =

  let arty = List.fold_left (fun arty (_,g) ->
      match g with
      | Static_array(c,_) -> ArrayType.add (size_const c) () arty) ArrayType.empty statics
  in

  Fsm_comp.SMap.iter (fun x _ -> Hashtbl.remove typing_env x;
                       Hashtbl.remove typing_env (Naming_convention.instance_id_of_fun x)) infos;

  fprintf fmt "@[<v>%s@]" vhdl_comment;
  fprintf fmt "@[<v>library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;

@[<v 2>entity %a is@," pp_ident name;

  let t_argument = Hashtbl.find_opt typing_env argument in
  let t_result = Hashtbl.find_opt typing_env result in
  if t_argument = None || t_result = None then
    (fprintf fmt "generic(@[<v>";
     if t_argument = None then fprintf fmt "argument_width : natural := 1";
     if t_argument = None && t_result = None then fprintf fmt ";@,";
     if t_result = None then fprintf fmt "  result_width : natural := 1@,";
     fprintf fmt ");@]");
  fprintf fmt "@,port(@[<v>signal clk    : in std_logic;@,";
  fprintf fmt "signal reset  : in std_logic;@,";
  fprintf fmt "signal run    : in std_logic;@,";
  fprintf fmt "signal rdy    : out value(0 to 0);@,";
  let st_argument = match t_argument with None -> "argument_width - 1" | Some t -> string_of_int (size_ty t - 1) in
  let st_result = match t_result with None -> "result_width - 1" | Some t -> string_of_int (size_ty t - 1) in
  fprintf fmt "signal %s : in value(0 to %s);@," argument st_argument;
  fprintf fmt "signal result : out value(0 to %s)" st_result;

  begin
    if !Fsm_comp.allow_heap_access || !Fsm_comp.allow_heap_assign then
      fprintf fmt ";@,signal caml_heap_base : in value(0 to 31)";
  end;
  fprintf fmt ");@,@]@]@,end entity;
architecture rtl of %a is@,@[<v 2>@," pp_ident name;


  declare_machine fmt ~state_var ~compute ~infos (ts,s);

  ArrayType.iter (fun n _ ->
      fprintf fmt "type array_value_%d is array (natural range <>) of value(0 to %d);@," n (n-1)) arty;

  List.iter (fun (x,Static_array(c,n)) ->
          fprintf fmt "signal %a : array_value_%d(0 to %d)" pp_ident x (size_const c) (n-1);

          if not(!ram_inference) then (
           fprintf fmt " := (others => %a); @," pp_c c
          ) else (fprintf fmt ";@,";
                  if !intel_max10_target then (
                    (** Intel MAX 10 FPGA device do not support memory initialization.
                        (source: https://www.intel.com/content/www/us/en/support/programmable/articles/000074796.html
                     *)
                    Prelude.Errors.warning (fun fmt ->
                        Format.fprintf fmt
                          "Static array %s%a%s (RAM block): Intel MAX 10 FPGA device do not support memory initialization.\n"
                          Prelude.Errors.bold
                          pp_ident x
                          Prelude.Errors.reset)
                  )
                  else (
                    fprintf fmt "attribute %a_init_file : string;@," pp_ident x;
                    fprintf fmt
                       "attribute %a_init_file of %a : signal is \"init_file_%a.mif\";@,"
                       pp_ident x
                       pp_ident x
                       pp_ident x));
          fprintf fmt "signal %a : value(0 to %d);@," pp_ident ("$"^x^"_value") (size_const c - 1);
          fprintf fmt "signal %a : natural range 0 to %d;@," pp_ident ("$"^x^"_ptr") (n - 1);
          fprintf fmt "signal %a : natural range 0 to %d;@," pp_ident ("$"^x^"_ptr_write") (n - 1);
          fprintf fmt "signal %a : value(0 to %d);@," pp_ident ("$"^x^"_write") (size_const c - 1);
          fprintf fmt "signal %a : std_logic := '0';@," pp_ident ("$"^x^"_write_request")
        ) statics;

  fprintf fmt "@,@[<v 2>begin@,";


  List.iter (fun (x,Static_array(c,n)) ->

    fprintf fmt "process (clk)
            begin
            if (rising_edge(clk)) then
                 %s if %a = '1' then
                    %a(%a) <= %a;
                 %s else
                   %a <= %a(%a);
                 %s end if;
            end if;
        end process;@,@,"
          (if !ram_inference then "--" else "")
          pp_ident ("$"^x^"_write_request")
          pp_ident x
          pp_ident ("$"^x^"_ptr_write")
          pp_ident ("$"^x^"_write")
          (if !ram_inference then "--" else "")
          pp_ident ("$"^x^"_value")
          pp_ident x
          pp_ident ("$"^x^"_ptr")
          (if !ram_inference then "--" else "");

    ) statics;


  fprintf fmt "@[<v 2>process(clk)@,";

  declare_variable ~argument ~statics typing_env fmt;


  List.iter (fun (x,Static_array(c,n)) ->
      fprintf fmt "variable %a : value(0 to 0) := \"0\";@," pp_ident ("$"^x^"_ptr_take");
      fprintf fmt "variable %a : value(0 to 0) := \"0\";@," pp_ident ("$"^x^"_ptr_write_take");
  ) statics;

  fprintf fmt "@]@,@[<v 2>begin@,";

  fprintf fmt "@,@[<v 2>if rising_edge(clk) then@,";

  fprintf fmt "@[<v 2>if (reset = '1') then@,";

  fprintf fmt "@[<hov>";
   Hashtbl.iter (fun x t ->
      match List.assoc_opt x statics with
      | Some (Static_array(c,n)) ->
          () (* fprintf fmt "@]@,%a <= (others => %a);@,@[<hov>" pp_ident x pp_c c *)
      | None ->
          if x <> argument then
            fprintf fmt "default_zero(%a);@ @," pp_ident x
    ) typing_env;
  fprintf fmt "@]";

  fprintf fmt "@,rdy <= \"1\";";
  fprintf fmt "@,%a := \"0\";@," pp_ident rdy;
  fprintf fmt "%a <= %a;@," pp_ident state_var pp_ident compute;

  List.iter (fun (_,(sv,cp,xs)) -> fprintf fmt "%a <= %a;@," pp_ident sv pp_ident cp) !List_machines.extra_machines;


  fprintf fmt "@]@,@[<v 2>else if run = '1' then@,";

  pp_fsm ~restart:false fmt ~state_var ~compute ~rdy ("main",ts,s);

  fprintf fmt "@,@,result <= %a;@," pp_ident result;
  fprintf fmt "rdy <= %a;@," pp_ident rdy;

  fprintf fmt "@]@,end if;
      end if;
    end if;
  end process;
end architecture;@]\n";

  ( (argument,t_argument), (result,t_result) )
