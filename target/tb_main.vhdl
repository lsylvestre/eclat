-- code generated from the following source code:
--   
--
-- with the following command:
--
--    ./eclat


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;
use work.all;

entity tb_main is
end entity;

architecture tb of tb_main is
  component main
    port(
      signal clk    : in std_logic;
      signal reset  : in std_logic;
      signal run    : in std_logic;
      signal rdy    : out value(0 to 0);
      signal argument : in value(0 to 0);
      signal result : out value(0 to 31));
end component;
  signal tb_run: std_logic;
  signal tb_argument: std_logic_vector(0 to 0);
  signal tb_result: std_logic_vector(0 to 31);
  signal tb_rdy: value(0 to 0);
  signal tb_clk: std_logic;
  signal rst: std_logic;
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

  U1: main port map(tb_clk,rst,tb_run,tb_rdy,tb_argument,tb_result);
  process
  begin
    tb_run <= '0';
    wait for 10 ns;
    tb_run <= '1';
      -- Start computation

    wait;
  end process;

  end architecture;

  
