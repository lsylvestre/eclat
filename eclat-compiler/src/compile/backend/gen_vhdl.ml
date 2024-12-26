open Fsm_syntax
open Format

open Gen_vhdl_aux

let ram_inference = Gen_vhdl_aux.ram_inference
let memory_initialization = Gen_vhdl_aux.memory_initialization
let intel_max10_target = Gen_vhdl_aux.intel_max10_target
let intel_xilinx_target = Gen_vhdl_aux.intel_xilinx_target
let single_read_write_lock_flag = Gen_vhdl_aux.single_read_write_lock_flag

let has_init_file_ram = ref [] ;;


(** code generator for statements *)
let rec pp_s externals ~st fmt = function
| S_skip -> ()
| S_continue q -> fprintf fmt "%a := %a;" pp_ident st pp_state q
| S_if(z,s1,so) ->
    fprintf fmt "@[<v 2>if %a(0) = '1' then@,%a@]" pp_ident z (pp_s externals ~st) s1;
    Option.iter (fun s2 -> fprintf fmt "@,@[<v 2>else@,%a@]" (pp_s externals ~st) s2) so;
     fprintf fmt "@,end if;"
| S_case(y,hs,so) ->
    fprintf fmt "@[<v>case %a is@," pp_ident y;
    let pp_cs fmt cs = 
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " | ")
        pp_c fmt cs
    in
    List.iter (fun (cs,s) ->
      fprintf fmt "@[<v 2>when %a =>@,%a@]@," pp_cs cs (pp_s externals ~st) s) hs;
    Option.iter (fun s ->
      fprintf fmt "@[<v 2>when others =>@,%a@]@," (pp_s externals ~st) s) so;
    fprintf fmt "@]end case;";
| S_set(x,a) -> fprintf fmt "@[<v>%a := %a;@]" pp_ident x (pp_a externals) a
| S_acquire_lock(l) ->
      fprintf fmt
         "@[acquire(%a);@]" 
            pp_ident (ptr_taken l)
 | S_release_lock l ->
      fprintf fmt
         "@[release(%a);@]" 
            pp_ident (ptr_taken l)
| S_read_start(x,idx) -> (* todo: avoid code duplication between S_setptr & S_setptr_write *)
   fprintf fmt "@[%a <= " pp_ident ("$"^x^"_ptr");
   (match idx with
    | A_const(Int{value=n}) ->
       fprintf fmt
         "%d" n
    | _ ->
       fprintf fmt
         "to_integer(unsigned(%a))" (pp_a externals) idx);
   fprintf fmt ";@]"
| S_read_stop(x,l) ->
        fprintf fmt
         "@[%a := %a;@]" 
            pp_ident x
            pp_ident ("$"^l^"_value") 
  | S_write_start(x,idx,a) ->
     fprintf fmt "@[%a <= " pp_ident ("$"^x^"_ptr_write");
     (match idx with
      | A_const(Int{value=n}) ->
         fprintf fmt
           "%d" n
      | _ ->
         fprintf fmt
           "to_integer(unsigned(%a))" (pp_a externals) idx);
      fprintf fmt ";@]";
      fprintf fmt
        "@[%a <= %a; %a <= '1';@]" 
          pp_ident ("$"^x^"_write")
          (pp_a externals) a
          pp_ident ("$"^x^"_write_request")
    | S_write_stop(x) ->
        fprintf fmt
         "@[%a <= '0';@]" 
              pp_ident ("$"^x^"_write_request")
| S_seq(S_skip,s) | S_seq(s,S_skip) -> pp_s externals ~st fmt s
| S_seq(s1,s2) ->
    fprintf fmt "@[<v>%a@,%a@]" 
      (pp_s externals ~st) s1 
      (pp_s externals ~st) s2
| S_letIn(x,a,s) ->
    fprintf fmt "@[<v>%a := %a;@,%a@]"
      pp_ident x 
      (pp_a externals) a 
      (pp_s externals ~st) s
| S_fsm(id,rdy,x,cp,ts,s) ->
     let (st2,_,_) = List.assoc id !List_machines.extra_machines in
     pp_fsm externals fmt
        ~state_var:st2 ~idle:cp ~rdy (id,ts,s)
| S_in_fsm(id,s) ->
     let (st2,_,_) = List.assoc id !List_machines.extra_machines in
     pp_s externals ~st:st2 fmt s
| S_call(op,a) ->
   fprintf fmt "%a;@," (pp_call externals) (Runtime(op),a)
| S_external_run(f,id,res,rdy,a) ->
   fprintf fmt "%a := %s_result_%d(0 to %s_result_%d'length - 2);@,"
          pp_ident res
          f id 
          f id;
   fprintf fmt "%a := %s_result_%d(%s_result_%d'length - 1 to %s_result_%d'length - 1);@,"
          pp_ident rdy
          f id 
          f id 
          f id;
   fprintf fmt "%s_argument_%d_var := \"1\" & %a;@,"
          f id
          (pp_a externals) a



(** code generator for FSMs *)
and pp_fsm externals fmt ~state_var:st ~idle ~rdy (id,ts,s) =
  fprintf fmt "@[<v>case %a is@," pp_ident st;
  List.iter (fun (x,s) ->
      fprintf fmt "@[<v 2>when %a =>@,%a@]@," 
        pp_state x 
        (pp_s externals ~st) 
        s) ts;
  fprintf fmt "@[<v 2>when %a =>@,%a@]@," 
    pp_state idle 
    (pp_s externals ~st) s;
  fprintf fmt "@]end case;@,"

(* default value as bitvector where each bit is at '0' *)
let default_zero_value nbits =
  "(others => '0')"

(* default value according to the given type. *)
let default_zero t =
  match Fsm_typing.canon t with
  | TStatic{size=TTuple ts} ->
     let rec aux ts =
       match ts with
       | [] -> "'0'"
       | _::ts' -> "(others => "^aux ts'^")"
      in aux ts
  | TStatic{size=t} -> "(others => (others => '0'))"
  | _ -> "(others => '0')"

let qualify prefix y =
  prefix^"_"^y

let declare_state_var fmt state_var idle xs =
  let state_var_tname = Naming_convention.state_var_type state_var in
    fprintf fmt "type %a is (%a" pp_ident state_var_tname pp_state idle;

    List.iter (fun x -> fprintf fmt ", %a" pp_state x) xs;

    fprintf fmt ");@,signal %a, %a: %a;@," pp_ident (state_var^"%now") pp_ident (state_var^"%next") pp_ident state_var_tname



let declare_machine fmt ~state_var ~idle ~infos (ts,s) =

  declare_state_var fmt state_var idle (List.map fst ts);

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
  | TStatic{elem;size=TTuple ts} -> 
      fprintf fmt "array_value_%d" (size_ty elem);
      List.iter (fun tsize -> fprintf fmt "(0 to %d)" (size_ty tsize - 1)) ts;
  | TStatic{elem;size} -> fprintf fmt "array_value_%d(0 to %d)" (size_ty elem) (size_ty size - 1);
  | _ ->
      fprintf fmt "value(0 to %d)" (size_ty t-1)




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


let declare_signals variables fmt =
  let var_decls = Hashtbl.create 10 in
  let add_var x n =
    match Hashtbl.find_opt var_decls n with
    | None -> Hashtbl.add var_decls n [x]
    | Some s -> Hashtbl.replace var_decls n (x::s)
  in
  List.iter (fun (x,t) ->
          let n = (size_ty t) in
          add_var ((x^"%now")) n;
          add_var ((x^"%next")) n
    ) variables;

  Hashtbl.iter (fun n xs ->
      fprintf fmt "signal @[<v>@[<hov>%a@] : value(0 to %d) := (others => '0');@]@,"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ @,") pp_ident) xs (n-1)
    ) var_decls


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
let pp_component fmt ~vhdl_comment ~name ~externals ~state_var ~argument ~result ~idle ~rdy ~statics typing_env infos (ts,s) =

  let arty = List.fold_left (fun arty (_,g) ->
      match g with
      | Static_array_of ty -> 
          (match Fsm_typing.canon ty with
          | TStatic{elem} ->  ArrayType.add (size_ty elem) () arty
          | _ -> Debug.pp_ty Format.std_formatter ty;  assert false)
      | Static_array(c,_) -> ArrayType.add (size_const c) () arty
    ) ArrayType.empty statics
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
  (* fprintf fmt "signal rdy    : out value(0 to 0);@,"; *)
  let st_argument = match t_argument with None -> "argument_width - 1" | Some t -> string_of_int (size_ty t - 1) in
  let st_result = match t_result with None -> "result_width - 1" | Some t -> string_of_int (size_ty t - 1) in
  fprintf fmt "signal %s : in value(0 to %s);@," argument st_argument;
  fprintf fmt "signal result : out value(0 to %s)" st_result;
  fprintf fmt ");@,@]@]@,end entity;
architecture rtl of %a is@,@[<v 2>@," pp_ident name;

  if !intel_xilinx_target then ( (* attribute for enforcing RAM inference in Xilinx Vivado *)
    fprintf fmt "attribute ram_style : string;@,"
  );

  declare_machine fmt ~state_var ~idle ~infos (ts,s);

  ArrayType.iter (fun n _ ->
      fprintf fmt "type array_value_%d is array (natural range <>) of value(0 to %d);@," n (n-1)) arty;


  List.iter (fun (x,st) ->
    match st with
   
    | Static_array_of ty ->
          let ty_elem,sz = match Fsm_typing.canon ty with
                          | TStatic {elem;size=sz} -> elem,sz
                          | _ -> assert false (* error *) in
          let n = match Fsm_typing.canon sz with
                  | TSize n -> n
                  | _ ->  Prelude.Errors.error (fun fmt -> 
                            Format.fprintf fmt "unspecified size for array %s" x) in
          let sz_elem = size_ty ty_elem in 
          array_decl fmt x sz_elem n ((fun fmt () -> fprintf fmt "%s" (default_zero ty_elem)))
         
    | Static_array(c,n) ->
          array_decl fmt x (size_const c) n (fun fmt () -> pp_c fmt c)

    ) statics;


  List.iter (print_external fmt) (fst externals);




  let variables = List.filter (fun (x,t) ->
          x <> argument && not (List.mem_assoc x statics) 
          && not (List.mem_assoc x (fst externals))
          && not (List.mem_assoc x (snd externals))
  )
    @@ List.of_seq (Hashtbl.to_seq typing_env) in
  let variables = variables @ List.map (fun (x,_) -> ptr_taken x, TBool) statics in
  let variables = variables @ List.filter_map (fun (x,(_,shared)) -> 
                                 if shared then Some (ptr_taken x, TBool) else None) (fst externals) in

  declare_signals variables fmt;

  fprintf fmt "@,@[<v 2>begin@,";


  (* instantiate externals *)
  List.iter (instantiate_external fmt) (fst externals);



  List.iter (fun (x,st) ->
    match st with
    | Static_array_of _
    | Static_array _ ->
      fprintf fmt "process (clk)
            begin
            if rising_edge(clk) then
                 if %a = '1' then
                    %a(%a) <= %a;
                 end if;
                 %a <= %a(%a);
            end if;
        end process;@,@,"
          pp_ident ("$"^x^"_write_request")
          pp_ident x
          pp_ident ("$"^x^"_ptr_write")
          pp_ident ("$"^x^"_write")
          pp_ident ("$"^x^"_value")
          pp_ident x
          pp_ident ("$"^x^"_ptr");

    ) statics;

  fprintf fmt "@[<v 2>process (reset,clk)@,";
  fprintf fmt "begin@,";
  fprintf fmt "@[<v 2>if reset = '1' then@,";

  begin
    let pp fmt (x,t) =
      fprintf fmt "%a <= %s;" pp_ident (x^"%now") (default_zero t);
    in
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
       pp fmt variables
  end;
     
  fprintf fmt "@,%a <= %a;" 
    pp_ident (state_var^"%now") 
    pp_ident idle;
  begin
    let pp fmt (_,(sv,cp,xs)) =
      fprintf fmt "@,%a <= %a;" pp_ident (sv^"%now") pp_ident cp
    in 
    List.iter (pp fmt)  !List_machines.extra_machines
  end;

  fprintf fmt "@]@,@[<v 2>elsif (rising_edge(clk)) then@,";

  begin
    let update fmt (x,_) =
      fprintf fmt "%a <= %a;" 
         pp_ident (x^"%now") 
         pp_ident (x^"%next")
    in
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
       update fmt variables;
  end;

  begin
    let update fmt (_,(sv,cp,xs)) =
      fprintf fmt "@,%a <= %a;" 
        pp_ident (sv^"%now") 
        pp_ident (sv^"%next")

    in
    List.iter (update fmt) !List_machines.extra_machines
  end;
 
 fprintf fmt "@,%a <= %a;" 
       pp_ident (state_var^"%now")
       pp_ident (state_var^"%next");

  fprintf fmt "@]@,end if;
    end process;@,@,@]";


  fprintf fmt "@[<v 2>process(%a,%a" pp_ident argument pp_ident (state_var^"%now");
  if !Operators.flag_no_print then () else fprintf fmt ", clk";
  List.iter (fun (_,(sv,_,_)) -> fprintf fmt ",%a" pp_ident (sv^"%now")) !List_machines.extra_machines;

  List.iter (fun (x,(Static_array_of _ | Static_array _)) ->
      fprintf fmt ", %a"  pp_ident ("$"^x^"_value") 
  ) statics;

  List.iter (fun (x,_) -> fprintf fmt ", %a" pp_ident (x^"%now")) variables;


  (* *************** *)
  let sensibility_external fmt (n,(_,shared)) =    
    let instances = match Hashtbl.find_opt Count_externals.external_count n with
             | None ->  Count_externals.IMap.empty  | Some v -> v in
      Count_externals.IMap.iter (fun i () ->
        fprintf fmt ", %s_result_%d" n i
      ) instances
    in    
    List.iter (sensibility_external fmt) (fst externals);
    (* ***************** *)

  fprintf fmt ")@,";

  declare_variable ~argument ~statics typing_env fmt;

  fprintf fmt "variable %a : %a;@," pp_ident state_var pp_ident (Naming_convention.state_var_type state_var);

  List.iter (fun (_,(sv,_,_)) ->
     fprintf fmt "variable %a : %a;@," pp_ident sv pp_ident (Naming_convention.state_var_type sv);
  ) !List_machines.extra_machines;

  List.iter (fun (x,(Static_array_of _ | Static_array _)) ->
      decl_locks fmt x
  ) statics;
  List.iter (fun (x,(_,shared)) ->
      if shared then decl_locks fmt x
  ) (fst externals);

  List.iter (variable_decl_go_external fmt) (fst externals);

  

  fprintf fmt "@]@,@[<v 2>begin@,";

  List.iter (variable_init_go_external fmt) (fst externals);

  
  (* fprintf fmt "@,@[<v 2>if rising_edge(clk) then@,";

  fprintf fmt "@[<v 2>if (reset = '1') then@,";*)


  List.iter (fun (x,_) ->
     fprintf fmt "%a := %a;@," pp_ident x pp_ident (x^"%now");
  ) variables;
  
    fprintf fmt "%a := %a;@," pp_ident state_var pp_ident (state_var^"%now");

  List.iter (fun (_,(sv,_,_)) ->
     fprintf fmt "%a := %a;@," pp_ident sv pp_ident (sv^"%now");
  ) !List_machines.extra_machines;


  (* fprintf fmt "@,rdy <= \"1\";"; 
  fprintf fmt "@,%a := \"0\";@," pp_ident rdy;*)
  (* fprintf fmt "%a <= %a;@," pp_ident state_var pp_ident idle; *)

(*  List.iter (fun (_,(sv,cp,xs)) -> fprintf fmt "%a <= %a;@," pp_ident sv pp_ident cp) !List_machines.extra_machines;
*)

  pp_fsm externals fmt ~state_var ~idle ~rdy ("main",ts,s);

  fprintf fmt "%a <= %a;@," pp_ident (state_var^"%next") pp_ident state_var;
  List.iter (fun (_,(sv,_,_)) ->
     fprintf fmt "%a <= %a;@," pp_ident (sv^"%next") pp_ident sv;
  ) !List_machines.extra_machines;

  List.iter (fun (x,_) ->
     fprintf fmt "%a <= %a;@," pp_ident (x^"%next") pp_ident x;
  ) variables;

  fprintf fmt "@,@,result <= %a;@," pp_ident result;
  (* fprintf fmt "rdy <= %a;@," pp_ident rdy; *)


  List.iter (variable_set_go_external fmt) (fst externals);

  fprintf fmt
    "end process;
  end architecture;@]\n";

  ( (argument,t_argument), (result,t_result) )
