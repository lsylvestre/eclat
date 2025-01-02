open MiniHDL_syntax
open Format

open Gen_vhdl_aux

(** code generator for statements *)
let rec pp_s externals ~st fmt = function
| S_skip -> ()
| S_continue q -> fprintf fmt "%a <= %a;" pp_ident st pp_state q
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
| S_seq(s1,s2) -> fprintf fmt "@[<v>%a@,%a@]" (pp_s externals ~st) s1 (pp_s externals ~st) s2
| S_letIn(x,a,s) -> fprintf fmt "@[<v>%a := %a;@,%a@]" pp_ident x (pp_a externals) a (pp_s externals ~st) s
| S_fsm(id,rdy,x,cp,ts,s) ->
     let (st2,_,_) = List.assoc id !List_machines.extra_machines in
     pp_fsm externals fmt ~state_var:st2 ~idle:cp ~rdy (id,ts,s)
| S_in_fsm(id,s) ->
     let (st2,_,_) = List.assoc id !List_machines.extra_machines in
     pp_s externals ~st:st2 fmt s
| S_array_set(x,y,a) ->
    fprintf fmt "@[%a(to_integer(unsigned(%a&\"000\"))) := %a@]"
      pp_ident x
      pp_ident y
      (pp_a externals) a
| S_call(op,a) ->
   fprintf fmt "%a;@," (pp_call externals) (Runtime(op),a)
| S_external_run _ -> assert false (* todo *)

(** code generator for FSMs *)
and pp_fsm externals fmt ~state_var:st ~idle ~rdy (id,ts,s) =
  fprintf fmt "@[<v>case %a is@," 
    pp_ident st;
  List.iter (fun (x,s) -> fprintf fmt "@[<v 2>when %a =>@,%a@]@," 
                pp_state x 
                (pp_s externals ~st)
                s) ts;
  fprintf fmt "@[<v 2>when %a =>@,%a@]@," pp_state idle (pp_s externals ~st) s;
  fprintf fmt "@]end case;"

(* default value as bitvector where each bit is at '0' *)
let default_zero_value nbits =
  "(others => '0')"

(* default value according to the given type. *)
let default_zero t =
  match MiniHDL_typing.canon t with
  | TStatic{size=TTuple ts} ->
     let rec aux ts =
       match ts with
       | [] -> "'0'"
       | _::ts' -> "(others => "^aux ts'^")"
      in aux ts
  | TStatic{size=t} -> "(others => (others => '0'))"
  | _ -> "(others => '0')"

let declare_state_var fmt state_var idle xs =
  let state_var_tname = Naming_convention.state_var_type state_var in
    fprintf fmt "type %a is (%a" pp_ident state_var_tname pp_state idle;

    List.iter (fun x -> fprintf fmt ", %a" pp_state x) xs;

    fprintf fmt ");@,signal %a: %a;@," pp_ident state_var pp_ident state_var_tname



let declare_machine fmt ~state_var ~idle ~infos (ts,s) =

  declare_state_var fmt state_var idle (List.map fst ts);

  List.iter (fun (_,(sv,cp,xs)) -> declare_state_var fmt sv cp xs) !List_machines.extra_machines;

  Gen_miniHDL.SMap.iter (fun x w ->
    let inst_tname = Naming_convention.instances_type x in
    let sq = Gen_miniHDL.IMap.to_seq w in
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
  match MiniHDL_typing.canon t with
  | TStatic{elem;size=TTuple ts} -> 
      fprintf fmt "array_value_%d" (size_ty elem);
      List.iter (fun tsize -> fprintf fmt "(0 to %d)" (size_ty tsize - 1)) ts;
  | TStatic{elem;size} -> fprintf fmt "array_value_%d(0 to %d)" (size_ty elem) (size_ty size - 1);
  | _ ->
      fprintf fmt "value(0 to %d)" (size_ty t-1)

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
          (match ty with
          | TStatic{elem} ->  ArrayType.add (size_ty elem) () arty
          | _ -> assert false)
      | Static_array(c,_) -> ArrayType.add (size_const c) () arty
    ) ArrayType.empty statics
  in

  Gen_miniHDL.SMap.iter (fun x _ -> Hashtbl.remove typing_env x;
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
  fprintf fmt "signal rdy    : out value(0 to 0);@,";
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
          let ty_elem,n = match ty with TStatic {elem;size=TSize n} -> elem,n | _ -> assert false (* error *) in
          let sz_elem = size_ty ty_elem in 
          array_decl fmt x sz_elem n ((fun fmt () -> fprintf fmt "%s" (default_zero ty_elem)))
         
    | Static_array(c,n) ->
          array_decl fmt x (size_const c) n (fun fmt () -> pp_c fmt c)

    ) statics;



  fprintf fmt "@,@[<v 2>begin@,";


  List.iter (fun (x,st) ->
    match st with
    | Static_array_of _
    | Static_array _ ->
      fprintf fmt "process (clk)
            begin
            if (rising_edge(clk)) and %a = '1' then
              %a(%a) <= %a;
            end if;
            if %a = '0' then
              %a <= %a(%a);
            else
              %a <= (others => '0');
            end if;
        end process;@,@,"
          pp_ident ("$"^x^"_write_request")
          pp_ident x
          pp_ident ("$"^x^"_ptr_write")
          pp_ident ("$"^x^"_write")
          pp_ident ("$"^x^"_write_request")
          pp_ident ("$"^x^"_value")
          pp_ident x
          pp_ident ("$"^x^"_ptr")
          pp_ident ("$"^x^"_value")

   

    ) statics;


  fprintf fmt "@[<v 2>process(clk)@,";

  declare_variable ~argument ~statics typing_env fmt;


  List.iter (fun (x,(Static_array_of _ | Static_array _)) ->
      decl_locks ~init:true fmt x

  ) statics;

  fprintf fmt "@]@,@[<v 2>begin@,";

  fprintf fmt "@,@[<v 2>if rising_edge(clk) then@,";

  fprintf fmt "@[<v 2>if (reset = '1') then@,";

  fprintf fmt "@[<hov>";
   Hashtbl.iter (fun x t ->
      match List.assoc_opt x statics with
      | Some (Static_array_of _)  -> ()
      | Some (Static_array(c,n)) ->
          () (* fprintf fmt "@]@,%a <= (others => %a);@,@[<hov>" pp_ident x pp_c c *)
      | None ->
          if x <> argument then
            fprintf fmt "default_zero(%a);@ @," pp_ident x
    ) typing_env;
  fprintf fmt "@]";

  fprintf fmt "@,rdy <= \"1\";";
  fprintf fmt "@,%a := \"0\";@," pp_ident rdy;
  fprintf fmt "%a <= %a;@," pp_ident state_var pp_state idle;

  List.iter (fun (_,(sv,idle,xs)) -> 
    fprintf fmt "%a <= %a;@," 
      pp_ident sv 
      pp_state idle) 
      !List_machines.extra_machines;

  (* initializing all locks to 0 *)
  List.iter (fun (x,(Static_array_of _ | Static_array _)) ->
     fprintf fmt "@,%a := (others => '0');@," pp_ident (ptr_taken x)
  ) statics;

  fprintf fmt "@]@,else@,";

  pp_fsm externals fmt ~state_var ~idle ~rdy ("main",ts,s);

  fprintf fmt "@,@,result <= %a;@," pp_ident result;
  fprintf fmt "rdy <= %a;@," pp_ident rdy;

  fprintf fmt "@]@,end if;
    end if;
  end process;
end architecture;@]\n";

  ( (argument,t_argument), (result,t_result) )
