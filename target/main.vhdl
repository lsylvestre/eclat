-- code generated from the following source code:
--   ../examples/abcro.ecl
--
-- with the following command:
--
--    ./eclat ../examples/abcro.ecl -main=abcro -arg=(((false,false),false),false);(((true,true),true),false);(((true,true),true),false);(((true,true),true),true);(((false,true),false),false);(((true,false),false),false);(((false,false),false),false);(((false,false),true),false)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;


entity main is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal rdy    : out value(0 to 0);
       signal argument : in value(0 to 3);
       signal result : out value(0 to 0));
       
end entity;
architecture rtl of main is

  type t_state is (IDLE113);
  signal \state%now\, \state%next\: t_state;
  signal \$347%next\, \$347%now\, \$335%next\, \$335%now\, \$340%next\, 
         \$340%now\, \$368%next\, \$368%now\ : value(0 to 1) := (others => '0');
  signal \result111%next\, \result111%now\, \$359%next\, \$359%now\, 
         \$v114%next\, \$v114%now\, \$354%next\, \$354%now\, \$v2%next\, 
         \$v2%now\, \$338%next\, \$338%now\, \$334%next\, \$334%now\, 
         \$339%next\, \$339%now\, \$333%next\, \$333%now\, \$380%next\, 
         \$380%now\, \$375%next\, \$375%now\, \$v115%next\, \$v115%now\, 
         \$v116%next\, \$v116%now\, \$v118%next\, \$v118%now\, \$v117%next\, 
         \$v117%now\, \$v119%next\, \$v119%now\, \$v4%next\, \$v4%now\, 
         \rdy112%next\, \rdy112%now\ : value(0 to 0) := (others => '0');
  
  begin
    process (reset,clk)
      begin
      if reset = '1' then
        \rdy112%now\ <= (others => '0');
        \$368%now\ <= (others => '0');
        \$v4%now\ <= (others => '0');
        \$v119%now\ <= (others => '0');
        \$v117%now\ <= (others => '0');
        \$340%now\ <= (others => '0');
        \$v118%now\ <= (others => '0');
        \$v116%now\ <= (others => '0');
        \$v115%now\ <= (others => '0');
        \$375%now\ <= (others => '0');
        \$380%now\ <= (others => '0');
        \$333%now\ <= (others => '0');
        \$339%now\ <= (others => '0');
        \$334%now\ <= (others => '0');
        \$338%now\ <= (others => '0');
        \$335%now\ <= (others => '0');
        \$v2%now\ <= (others => '0');
        \$354%now\ <= (others => '0');
        \$v114%now\ <= (others => '0');
        \$359%now\ <= (others => '0');
        \result111%now\ <= (others => '0');
        \$347%now\ <= (others => '0');
        \state%now\ <= idle113;
      elsif (rising_edge(clk)) then
        \rdy112%now\ <= \rdy112%next\;
        \$368%now\ <= \$368%next\;
        \$v4%now\ <= \$v4%next\;
        \$v119%now\ <= \$v119%next\;
        \$v117%now\ <= \$v117%next\;
        \$340%now\ <= \$340%next\;
        \$v118%now\ <= \$v118%next\;
        \$v116%now\ <= \$v116%next\;
        \$v115%now\ <= \$v115%next\;
        \$375%now\ <= \$375%next\;
        \$380%now\ <= \$380%next\;
        \$333%now\ <= \$333%next\;
        \$339%now\ <= \$339%next\;
        \$334%now\ <= \$334%next\;
        \$338%now\ <= \$338%next\;
        \$335%now\ <= \$335%next\;
        \$v2%now\ <= \$v2%next\;
        \$354%now\ <= \$354%next\;
        \$v114%now\ <= \$v114%next\;
        \$359%now\ <= \$359%next\;
        \result111%now\ <= \result111%next\;
        \$347%now\ <= \$347%next\;
        \state%now\ <= \state%next\;
      end if;
    end process;
      
      process(argument,\state%now\, clk, \rdy112%now\, \$368%now\, \$v4%now\, \$v119%now\, \$v117%now\, \$340%now\, \$v118%now\, \$v116%now\, \$v115%now\, \$375%now\, \$380%now\, \$333%now\, \$339%now\, \$334%now\, \$338%now\, \$335%now\, \$v2%now\, \$354%now\, \$v114%now\, \$359%now\, \result111%now\, \$347%now\)
        variable \$347\, \$335\, \$340\, \$368\ : value(0 to 1) := (others => '0');
        variable result111, \$359\, \$v114\, \$354\, \$v2\, \$338\, \$334\, 
                 \$339\, \$333\, \$380\, \$375\, \$v115\, \$v116\, \$v118\, 
                 \$v117\, \$v119\, \$v4\, rdy112 : value(0 to 0) := (others => '0');
        variable state : t_state;
        
    begin
      rdy112 := \rdy112%now\;
      \$368\ := \$368%now\;
      \$v4\ := \$v4%now\;
      \$v119\ := \$v119%now\;
      \$v117\ := \$v117%now\;
      \$340\ := \$340%now\;
      \$v118\ := \$v118%now\;
      \$v116\ := \$v116%now\;
      \$v115\ := \$v115%now\;
      \$375\ := \$375%now\;
      \$380\ := \$380%now\;
      \$333\ := \$333%now\;
      \$339\ := \$339%now\;
      \$334\ := \$334%now\;
      \$338\ := \$338%now\;
      \$335\ := \$335%now\;
      \$v2\ := \$v2%now\;
      \$354\ := \$354%now\;
      \$v114\ := \$v114%now\;
      \$359\ := \$359%now\;
      result111 := \result111%now\;
      \$347\ := \$347%now\;
      state := \state%now\;
      
      rdy <= "1";
      rdy112 := "0";
      case state is
      when IDLE113 =>
        rdy112 := eclat_false;
        \$v119\ := eclat_not(\$v2\);
        if \$v119\(0) = '1' then
          \$v2\ := eclat_true;
          \$380\ := eclat_false;
        end if;
        \$380\ := eclat_and(eclat_if(\$380\ & eclat_true & ""&argument(0)) & eclat_not(""&argument(3)));
        \$333\ := \$380\;
        \$v118\ := eclat_not(\$v2\);
        if \$v118\(0) = '1' then
          \$v2\ := eclat_true;
          \$375\ := eclat_false;
        end if;
        \$375\ := eclat_and(eclat_if(\$375\ & eclat_true & ""&argument(1)) & eclat_not(""&argument(3)));
        \$334\ := \$375\;
        \$v117\ := eclat_not(\$v4\);
        if \$v117\(0) = '1' then
          \$v4\ := eclat_true;
          \$368\ := eclat_false & eclat_and(\$333\ & \$334\);
        end if;
        \$368\ := eclat_and(\$333\ & \$334\) & ""&\$368\(0);
        \$335\ := \$368\;
        \$v116\ := eclat_not(\$v2\);
        if \$v116\(0) = '1' then
          \$v2\ := eclat_true;
          \$359\ := eclat_false;
        end if;
        \$359\ := eclat_and(eclat_if(\$359\ & eclat_true & eclat_and(eclat_not(""&\$335\(1)) & eclat_and(\$333\ & \$334\))) & eclat_not(""&argument(3)));
        \$338\ := \$359\;
        \$v115\ := eclat_not(\$v2\);
        if \$v115\(0) = '1' then
          \$v2\ := eclat_true;
          \$354\ := eclat_false;
        end if;
        \$354\ := eclat_and(eclat_if(\$354\ & eclat_true & ""&argument(2)) & eclat_not(""&argument(3)));
        \$339\ := \$354\;
        \$v114\ := eclat_not(\$v4\);
        if \$v114\(0) = '1' then
          \$v4\ := eclat_true;
          \$347\ := eclat_false & eclat_and(\$338\ & \$339\);
        end if;
        \$347\ := eclat_and(\$338\ & \$339\) & ""&\$347\(0);
        \$340\ := \$347\;
        result111 := eclat_and(eclat_not(""&\$340\(1)) & eclat_and(\$338\ & \$339\));
        rdy112 := eclat_true;
        state := IDLE113;
      end case;
      \state%next\ <= state;
      \rdy112%next\ <= rdy112;
      \$368%next\ <= \$368\;
      \$v4%next\ <= \$v4\;
      \$v119%next\ <= \$v119\;
      \$v117%next\ <= \$v117\;
      \$340%next\ <= \$340\;
      \$v118%next\ <= \$v118\;
      \$v116%next\ <= \$v116\;
      \$v115%next\ <= \$v115\;
      \$375%next\ <= \$375\;
      \$380%next\ <= \$380\;
      \$333%next\ <= \$333\;
      \$339%next\ <= \$339\;
      \$334%next\ <= \$334\;
      \$338%next\ <= \$338\;
      \$335%next\ <= \$335\;
      \$v2%next\ <= \$v2\;
      \$354%next\ <= \$354\;
      \$v114%next\ <= \$v114\;
      \$359%next\ <= \$359\;
      \result111%next\ <= result111;
      \$347%next\ <= \$347\;
      
      
      result <= result111;
      rdy <= rdy112;
      end process;
  end architecture;
