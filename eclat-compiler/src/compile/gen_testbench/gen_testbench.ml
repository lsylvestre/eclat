open Format

let size_ty_opt = function
| None -> 1
| Some t -> Gen_vhdl.size_ty t

let gen_testbench fmt ?(vhdl_comment="") typing_env name ty ((argument,ta),(result,tr)) (args_for_simul: _ list) =
  let argument_size = size_ty_opt ta in
  let result_size = size_ty_opt tr in

  let typing_env_tb = Hashtbl.create 10 in
  let tmp = Ast.gensym () in
  let ss_args = List.map (fun arg_for_simul ->
      let s_arg = Flat_let_atom.flat_s (Fsm_syntax.set_ tmp arg_for_simul) in
      Fsm_typing.typing_error_handler @@ (fun () ->
        Option.iter (fun t -> Fsm_typing.add_typing_env typing_env_tb tmp t) ta;
        Fsm_typing.typing_s ~result:tmp typing_env_tb s_arg;
        s_arg)) args_for_simul in

  fprintf fmt "@[<v>%s@]" vhdl_comment;
  fprintf fmt "@[<v>
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;
use work.all;

entity tb_%s is@," name;
  fprintf fmt "end entity;

architecture tb of tb_%s is
  " name;
  fprintf fmt "component main
    port(
      signal clk    : in std_logic;
      signal reset  : in std_logic;
      signal run    : in std_logic;
      signal rdy    : out value(0 to 0);
      signal %s : in value(0 to %d);
      signal result : out value(0 to %d)" argument (argument_size-1) (result_size-1);

  begin
    if !Fsm_comp.allow_heap_access || !Fsm_comp.allow_heap_assign then
      fprintf fmt ";@,signal caml_heap_base : in value(0 to 31)";
  end;

  fprintf fmt ");@,end component;";

  fprintf fmt "
  signal tb_run: std_logic;
  signal tb_argument: std_logic_vector(0 to %d);
  signal tb_result: std_logic_vector(0 to %d);
  signal tb_rdy: value(0 to 0);
  signal tb_clk: std_logic;
  signal rst: std_logic;" (argument_size-1) (result_size-1);

  begin
    if !Fsm_comp.allow_heap_access || !Fsm_comp.allow_heap_assign then
      fprintf fmt "@,signal tb_caml_heap_base : value(0 to 31);";
  end;

  fprintf fmt "
  begin

  RESET: process
  begin
    rst <= '1';
    wait for 2 ns;
    rst <= '0';
    wait;
  end process;


  CLOCK: process
  begin
    tb_clk <= '1';
    wait for 5 ns;
    tb_clk <= '0';
    wait for 5 ns;
  end process;

  U1: main port map(tb_clk,rst,tb_run,tb_rdy,tb_argument,tb_result";

  begin
    if !Fsm_comp.allow_heap_access || !Fsm_comp.allow_heap_assign then
      fprintf fmt ",tb_caml_heap_base";
  end;
  fprintf fmt ");";
  fprintf fmt "
  process
  ";


  Gen_vhdl.declare_variable ~argument:"tb_argument" ~statics:[] typing_env_tb fmt;

  fprintf fmt "begin
    tb_run <= '0';
    wait for 10 ns;
    tb_run <= '1';
      -- Start computation@,";

  List.iter (fun s_arg ->
    fprintf fmt "      @[<v>%a@]@," (Gen_vhdl.pp_s ~st:"fake") s_arg;
    fprintf fmt "      tb_argument <= %a;@," Gen_vhdl.pp_ident tmp; (* (Gen_vhdl.default_zero_value argument_size);*)
    fprintf fmt "
      wait for 10 ns;
      while tb_rdy = \"0\" loop
        wait for 10 ns;
      end loop;@,";
      (** -- not equivalent to wait until*)
  ) ss_args;

  fprintf fmt "
    wait;
  end process;

  end architecture;@,@]
  ";
  Format.pp_force_newline fmt ()
