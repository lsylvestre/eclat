-- code generated from the following source code:
--   ../examples/collatz.ecl
--
-- with the following command:
--
--    ./eclat -relax ../examples/collatz.ecl


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
      signal rdy    : out value(0 to 0);
      signal argument : in value(0 to 31);
      signal result : out value(0 to 31));
end component;
  signal tb_argument: std_logic_vector(0 to 31) := (others => '0');
  signal tb_result: std_logic_vector(0 to 31);
  signal tb_next_result: std_logic_vector(0 to 31);
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

  U1: main port map(tb_clk,rst,tb_rdy,tb_argument,tb_next_result);
      process (RST,tb_clk) 
      begin
        if RST = '1' then
          tb_result <= (others => '0');
        elsif (rising_edge(tb_clk)) then
          tb_result <= tb_next_result;   -- resynchronize output
        end if;
      end process;

  process
  begin

      -- Start computation
    wait for 5 ns;

    wait;
  end process;

  end architecture;

  
