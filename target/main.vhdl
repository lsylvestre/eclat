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


entity main is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal rdy    : out value(0 to 0);
       signal argument : in value(0 to 31);
       signal result : out value(0 to 31));
       
end entity;
architecture rtl of main is

  type t_state is (IDLE59, \$104_LOOP1655\, \$122_LOOP1656\);
  signal \state%now\, \state%next\: t_state;
  signal \$104_loop1655_arg%next\, \$104_loop1655_arg%now\, 
         \$122_loop1656_arg%next\, \$122_loop1656_arg%now\ : value(0 to 63) := (others => '0');
  signal \$v62%next\, \$v62%now\, \$v61%next\, \$v61%now\, \rdy58%next\, 
         \rdy58%now\, \$v63%next\, \$v63%now\, \$v60%next\, \$v60%now\ : value(0 to 0) := (others => '0');
  signal \$104_loop1655_result%next\, \$104_loop1655_result%now\, 
         \result57%next\, \result57%now\, \$122_loop1656_result%next\, 
         \$122_loop1656_result%now\, \$103_x%next\, \$103_x%now\ : value(0 to 31) := (others => '0');
  
  begin
    process (reset,clk)
      begin
      if reset = '1' then
        \$122_loop1656_arg%now\ <= (others => '0');
        \$v60%now\ <= (others => '0');
        \$103_x%now\ <= (others => '0');
        \$v63%now\ <= (others => '0');
        \$104_loop1655_arg%now\ <= (others => '0');
        \rdy58%now\ <= (others => '0');
        \$122_loop1656_result%now\ <= (others => '0');
        \$v61%now\ <= (others => '0');
        \result57%now\ <= (others => '0');
        \$v62%now\ <= (others => '0');
        \$104_loop1655_result%now\ <= (others => '0');
        \state%now\ <= idle59;
      elsif (rising_edge(clk)) then
        \$122_loop1656_arg%now\ <= \$122_loop1656_arg%next\;
        \$v60%now\ <= \$v60%next\;
        \$103_x%now\ <= \$103_x%next\;
        \$v63%now\ <= \$v63%next\;
        \$104_loop1655_arg%now\ <= \$104_loop1655_arg%next\;
        \rdy58%now\ <= \rdy58%next\;
        \$122_loop1656_result%now\ <= \$122_loop1656_result%next\;
        \$v61%now\ <= \$v61%next\;
        \result57%now\ <= \result57%next\;
        \$v62%now\ <= \$v62%next\;
        \$104_loop1655_result%now\ <= \$104_loop1655_result%next\;
        \state%now\ <= \state%next\;
      end if;
    end process;
      
      process(argument,\state%now\, clk, \$122_loop1656_arg%now\, \$v60%now\, \$103_x%now\, \$v63%now\, \$104_loop1655_arg%now\, \rdy58%now\, \$122_loop1656_result%now\, \$v61%now\, \result57%now\, \$v62%now\, \$104_loop1655_result%now\)
        variable \$104_loop1655_arg\, \$122_loop1656_arg\ : value(0 to 63) := (others => '0');
        variable \$v62\, \$v61\, rdy58, \$v63\, \$v60\ : value(0 to 0) := (others => '0');
        variable \$104_loop1655_result\, result57, \$122_loop1656_result\, 
                 \$103_x\ : value(0 to 31) := (others => '0');
        variable state : t_state;
        
    begin
      \$122_loop1656_arg\ := \$122_loop1656_arg%now\;
      \$v60\ := \$v60%now\;
      \$103_x\ := \$103_x%now\;
      \$v63\ := \$v63%now\;
      \$104_loop1655_arg\ := \$104_loop1655_arg%now\;
      rdy58 := \rdy58%now\;
      \$122_loop1656_result\ := \$122_loop1656_result%now\;
      \$v61\ := \$v61%now\;
      result57 := \result57%now\;
      \$v62\ := \$v62%now\;
      \$104_loop1655_result\ := \$104_loop1655_result%now\;
      state := \state%now\;
      
      rdy <= "1";
      rdy58 := "0";
      case state is
      when \$104_LOOP1655\ =>
        \$v61\ := eclat_eq(\$104_loop1655_arg\(0 to 31) & X"0000000" & X"1");
        if \$v61\(0) = '1' then
          \$104_loop1655_result\ := \$104_loop1655_arg\(32 to 63);
          result57 := \$104_loop1655_result\;
          rdy58 := eclat_true;
          state := IDLE59;
        else
          \$v60\ := eclat_eq(eclat_mod(\$104_loop1655_arg\(0 to 31) & X"0000000" & X"2") & X"0000000" & X"0");
          if \$v60\(0) = '1' then
            \$104_loop1655_arg\ := eclat_div(\$104_loop1655_arg\(0 to 31) & X"0000000" & X"2") & eclat_add(\$104_loop1655_arg\(32 to 63) & X"0000000" & X"1");
            state := \$104_LOOP1655\;
          else
            \$104_loop1655_arg\ := eclat_add(eclat_mult(X"0000000" & X"3" & \$104_loop1655_arg\(0 to 31)) & X"0000000" & X"1") & eclat_add(\$104_loop1655_arg\(32 to 63) & X"0000000" & X"1");
            state := \$104_LOOP1655\;
          end if;
        end if;
      when \$122_LOOP1656\ =>
        \$v63\ := eclat_eq(\$122_loop1656_arg\(0 to 31) & X"0000000" & X"1");
        if \$v63\(0) = '1' then
          \$122_loop1656_result\ := \$122_loop1656_arg\(32 to 63);
          \$103_x\ := \$122_loop1656_result\;
          \$104_loop1655_arg\ := \$103_x\ & X"0000000" & X"0";
          state := \$104_LOOP1655\;
        else
          \$v62\ := eclat_eq(eclat_mod(\$122_loop1656_arg\(0 to 31) & X"0000000" & X"2") & X"0000000" & X"0");
          if \$v62\(0) = '1' then
            \$122_loop1656_arg\ := eclat_div(\$122_loop1656_arg\(0 to 31) & X"0000000" & X"2") & eclat_add(\$122_loop1656_arg\(32 to 63) & X"0000000" & X"1");
            state := \$122_LOOP1656\;
          else
            \$122_loop1656_arg\ := eclat_add(eclat_mult(X"0000000" & X"3" & \$122_loop1656_arg\(0 to 31)) & X"0000000" & X"1") & eclat_add(\$122_loop1656_arg\(32 to 63) & X"0000000" & X"1");
            state := \$122_LOOP1656\;
          end if;
        end if;
      when IDLE59 =>
        rdy58 := eclat_false;
        \$122_loop1656_arg\ := argument & X"0000000" & X"0";
        state := \$122_LOOP1656\;
      end case;
      \state%next\ <= state;
      \$122_loop1656_arg%next\ <= \$122_loop1656_arg\;
      \$v60%next\ <= \$v60\;
      \$103_x%next\ <= \$103_x\;
      \$v63%next\ <= \$v63\;
      \$104_loop1655_arg%next\ <= \$104_loop1655_arg\;
      \rdy58%next\ <= rdy58;
      \$122_loop1656_result%next\ <= \$122_loop1656_result\;
      \$v61%next\ <= \$v61\;
      \result57%next\ <= result57;
      \$v62%next\ <= \$v62\;
      \$104_loop1655_result%next\ <= \$104_loop1655_result\;
      
      
      result <= result57;
      rdy <= rdy58;
      end process;
  end architecture;
