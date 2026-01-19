-- code generated from the following source code:
--   stdlib.ecl
--
-- with the following command:
--
--    ./eclat

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.all;


entity main is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal argument : in Values.t(0 to 0);
       signal result : out Values.t(0 to 0));
       
end entity;
architecture rtl of main is

  type t_state is (IDLE98);
  signal \state%now\, \state%next\: t_state;
  signal \$601_x%next\, \$601_x%now\ : Values.t(0 to 319) := (others => '0');
  signal \rdy97%next\, \rdy97%now\, \result96%next\, \result96%now\ : Values.t(0 to 0) := (others => '0');
  signal \$603%next\, \$603%now\ : Values.t(0 to 31) := (others => '0');
  
  begin
    process (reset,clk)
      begin
      if reset = '1' then
        \$601_x%now\ <= (others => '0');
        \result96%now\ <= (others => '0');
        \rdy97%now\ <= (others => '0');
        \$603%now\ <= (others => '0');
        \state%now\ <= idle98;
      elsif (rising_edge(clk)) then
        \$601_x%now\ <= \$601_x%next\;
        \result96%now\ <= \result96%next\;
        \rdy97%now\ <= \rdy97%next\;
        \$603%now\ <= \$603%next\;
        \state%now\ <= \state%next\;
      end if;
    end process;
      
      process(argument,\state%now\, clk, \$601_x%now\, \result96%now\, \rdy97%now\, \$603%now\)
        variable \$601_x\ : Values.t(0 to 319) := (others => '0');
        variable rdy97, result96 : Values.t(0 to 0) := (others => '0');
        variable \$603\ : Values.t(0 to 31) := (others => '0');
        variable state : t_state;
        
    begin
      \$601_x\ := \$601_x%now\;
      result96 := \result96%now\;
      rdy97 := \rdy97%now\;
      \$603\ := \$603%now\;
      state := \state%now\;
       -- case state is when IDLE98 =>
      rdy97 := work.values.val_false;
      \$601_x\ := work.Vect.create(32, 320, "00000000000000000000000001100100");
      \$603\ := work.Vect.nth(352, 32, \$601_x\, "00000000000000000000000000001001");
      result96 := work.Int.print(clk,\$603\);
      rdy97 := work.Values.val_true;
      state := IDLE98;
      -- end case;
      \state%next\ <= state;
      \$601_x%next\ <= \$601_x\;
      \result96%next\ <= result96;
      \rdy97%next\ <= rdy97;
      \$603%next\ <= \$603\;
      
      
      result <= result96;
      end process;
  end architecture;
