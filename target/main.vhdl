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


entity main is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal run    : in std_logic;
       signal rdy    : out value(0 to 0);
       signal argument : in value(0 to 0);
       signal result : out value(0 to 31));
       
end entity;
architecture rtl of main is

  type t_state is (compute6);
  signal state: t_state;
  
  begin
    process(clk)
      variable rdy5 : value(0 to 0) := (others => '0');
      variable \$26_x\, result4, \$v7\ : value(0 to 31) := (others => '0');
      
    begin
      
      if rising_edge(clk) then
        if (reset = '1') then
          default_zero(rdy5); default_zero(\$v7\); default_zero(result4); 
          default_zero(\$26_x\); 
          rdy <= "1";
          rdy5 := "0";
          state <= compute6;
          
        else if run = '1' then
          case state is
          when compute6 =>
            rdy5 := eclat_false;
            \$v7\ := X"0000000" & X"5";
            case \$v7\ is
            when X"0000000" & X"1" =>
              \$26_x\ := X"0000000" & X"2";
            when X"0000000" & X"2" =>
              \$26_x\ := X"0000000" & X"3";
            when others =>
              \$26_x\ := X"0000000" & X"4";
            end case;
            result4 := eclat_add(\$26_x\ & X"0000000" & X"1");
            rdy5 := eclat_true;
            state <= compute6;
          end case;
          
          result <= result4;
          rdy <= rdy5;
          
        end if;
      end if;
    end if;
  end process;
end architecture;
