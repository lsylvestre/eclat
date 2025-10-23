open Format

let size_ty_opt = function
| None -> 1
| Some t -> Gen_vhdl_aux.size_ty t

let gen_testbench fmt ?(vhdl_comment="") ~externals name ty result argument (tyB_arg,tyB_result) (es_args: _ list) =
  let argument_size = Gen_vhdl2.size_tyB tyB_arg in
  let result_size = Gen_vhdl2.size_tyB tyB_result in

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
  fprintf fmt "component %s
    port(
      signal clk    : in std_logic;
      signal reset  : in std_logic;
      signal %s : in value(0 to %d);
      signal result : out value(0 to %d)" name argument (argument_size-1) (result_size-1);

  fprintf fmt ");@,end component;";

  fprintf fmt "
  signal tb_argument: std_logic_vector(0 to %d) := (others => '0');
  signal tb_result: std_logic_vector(0 to %d);
  signal tb_next_result: std_logic_vector(0 to %d);
  signal tb_clk: std_logic;
  signal rst: std_logic;" (argument_size-1) (result_size-1) (result_size-1);

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

  U1: %s port map(tb_clk,rst,tb_argument,tb_result" name;

  fprintf fmt ");";

  fprintf fmt "
      process (RST,tb_clk) 
      begin
        if RST = '1' then
         -- tb_result <= (others => '0');
        elsif (rising_edge(tb_clk)) then
         -- tb_result <= tb_next_result;   -- resynchronize output
        end if;
      end process;@,";


  fprintf fmt "
  process
  ";

  (* Gen_vhdl.declare_variable ~argument:"tb_argument" ~statics:[] typing_env_tb fmt;
*)
  fprintf fmt "@,variable v_tb_argument : std_logic_vector(0 to %d);@," (argument_size-1);
  fprintf fmt "begin

      -- Start computation
    wait for 5 ns;@,";

  List.iter (fun e_arg ->
    fprintf fmt "      @[<v>";
    Gen_vhdl2.pp_s ~statics:[] ~externals:("",[]) ~sums:[] (Ast.P_var "v_tb_argument") tyB_arg fmt e_arg;
    fprintf fmt "tb_argument <= v_tb_argument; @]@,";  
    fprintf fmt "wait for 10 ns;@,"
  ) es_args;

  fprintf fmt "
    wait;
  end process;

  end architecture;@,@]
  ";
  Format.pp_force_newline fmt ()
