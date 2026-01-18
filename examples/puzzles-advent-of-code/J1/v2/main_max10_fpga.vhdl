-- code generated from the following source code:
--   stdlib.ecl
--   ../j1/v2/sol1f_ex_v2.ecl
--
-- with the following command:
--
--    ./eclat -main=main_max10_fpga -intel-max10 ../j1/v2/sol1f_ex_v2.ecl

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;


entity main_max10_fpga is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal argument : in value(0 to 11);
       signal result : out value(0 to 57));
       
end entity;
architecture rtl of main_max10_fpga is

  type t_state is (IDLE94);
  signal \state%now\, \state%next\: t_state;
  signal \$v97%next\, \$v97%now\ : value(0 to 1) := (others => '0');
  signal \$850_cmd%next\, \$850_cmd%now\ : value(0 to 13) := (others => '0');
  signal \$860%next\, \$860%now\ : value(0 to 23) := (others => '0');
  signal \$855%next\, \$855%now\ : value(0 to 12) := (others => '0');
  signal \$852_leds_0_to_4%next\, \$852_leds_0_to_4%now\, 
         \$853_leds_5_to_9%next\, \$853_leds_5_to_9%now\ : value(0 to 4) := (others => '0');
  signal \result92%next\, \result92%now\ : value(0 to 57) := (others => '0');
  signal \$v95%next\, \$v95%now\, \$859_m%next\, \$859_m%now\, 
         \$857_all_zero%next\, \$857_all_zero%now\, \$869_next_pos%next\, 
         \$869_next_pos%now\, \$856_q%next\, \$856_q%now\, \$858%next\, 
         \$858%now\, \$872_step%next\, \$872_step%now\, \$867_az%next\, 
         \$867_az%now\, \$864_azp1%next\, \$864_azp1%now\, \$871_az%next\, 
         \$871_az%now\, \$849_step%next\, \$849_step%now\, 
         \$874_argument%next\, \$874_argument%now\, \$873_step%next\, 
         \$873_step%now\, \$868%next\, \$868%now\ : value(0 to 11) := (others => '0');
  signal \$865_test%next\, \$865_test%now\, \$v98%next\, \$v98%now\, 
         \$866%next\, \$866%now\, \$863%next\, \$863%now\, \label91%next\, 
         \label91%now\, \$870%next\, \$870%now\, \rdy93%next\, \rdy93%now\, 
         \$861_next_pos_is_zero%next\, \$861_next_pos_is_zero%now\ : value(0 to 0) := (others => '0');
  signal \$862_next_zeros%next\, \$862_next_zeros%now\ : value(0 to 31) := (others => '0');
  signal \$851%next\, \$851%now\, \$854%next\, \$854%now\ : value(0 to 56) := (others => '0');
  
  begin
    process (reset,clk)
      begin
      if reset = '1' then
        \$855%now\ <= (others => '0');
        \$868%now\ <= (others => '0');
        \$873_step%now\ <= (others => '0');
        \$854%now\ <= (others => '0');
        \result92%now\ <= (others => '0');
        \$874_argument%now\ <= (others => '0');
        \$853_leds_5_to_9%now\ <= (others => '0');
        \$v97%now\ <= (others => '0');
        \$849_step%now\ <= (others => '0');
        \$860%now\ <= (others => '0');
        \$861_next_pos_is_zero%now\ <= (others => '0');
        \$871_az%now\ <= (others => '0');
        \$864_azp1%now\ <= (others => '0');
        \$867_az%now\ <= (others => '0');
        \$862_next_zeros%now\ <= (others => '0');
        \$872_step%now\ <= (others => '0');
        \rdy93%now\ <= (others => '0');
        \$870%now\ <= (others => '0');
        \$858%now\ <= (others => '0');
        \$851%now\ <= (others => '0');
        \$856_q%now\ <= (others => '0');
        \$869_next_pos%now\ <= (others => '0');
        \$857_all_zero%now\ <= (others => '0');
        \label91%now\ <= (others => '0');
        \$859_m%now\ <= (others => '0');
        \$852_leds_0_to_4%now\ <= (others => '0');
        \$863%now\ <= (others => '0');
        \$866%now\ <= (others => '0');
        \$850_cmd%now\ <= (others => '0');
        \$v95%now\ <= (others => '0');
        \$v98%now\ <= (others => '0');
        \$865_test%now\ <= (others => '0');
        \state%now\ <= idle94;
      elsif (rising_edge(clk)) then
        \$855%now\ <= \$855%next\;
        \$868%now\ <= \$868%next\;
        \$873_step%now\ <= \$873_step%next\;
        \$854%now\ <= \$854%next\;
        \result92%now\ <= \result92%next\;
        \$874_argument%now\ <= \$874_argument%next\;
        \$853_leds_5_to_9%now\ <= \$853_leds_5_to_9%next\;
        \$v97%now\ <= \$v97%next\;
        \$849_step%now\ <= \$849_step%next\;
        \$860%now\ <= \$860%next\;
        \$861_next_pos_is_zero%now\ <= \$861_next_pos_is_zero%next\;
        \$871_az%now\ <= \$871_az%next\;
        \$864_azp1%now\ <= \$864_azp1%next\;
        \$867_az%now\ <= \$867_az%next\;
        \$862_next_zeros%now\ <= \$862_next_zeros%next\;
        \$872_step%now\ <= \$872_step%next\;
        \rdy93%now\ <= \rdy93%next\;
        \$870%now\ <= \$870%next\;
        \$858%now\ <= \$858%next\;
        \$851%now\ <= \$851%next\;
        \$856_q%now\ <= \$856_q%next\;
        \$869_next_pos%now\ <= \$869_next_pos%next\;
        \$857_all_zero%now\ <= \$857_all_zero%next\;
        \label91%now\ <= \label91%next\;
        \$859_m%now\ <= \$859_m%next\;
        \$852_leds_0_to_4%now\ <= \$852_leds_0_to_4%next\;
        \$863%now\ <= \$863%next\;
        \$866%now\ <= \$866%next\;
        \$850_cmd%now\ <= \$850_cmd%next\;
        \$v95%now\ <= \$v95%next\;
        \$v98%now\ <= \$v98%next\;
        \$865_test%now\ <= \$865_test%next\;
        \state%now\ <= \state%next\;
      end if;
    end process;
      
      process(argument,\state%now\, \$855%now\, \$868%now\, \$873_step%now\, \$854%now\, \result92%now\, \$874_argument%now\, \$853_leds_5_to_9%now\, \$v97%now\, \$849_step%now\, \$860%now\, \$861_next_pos_is_zero%now\, \$871_az%now\, \$864_azp1%now\, \$867_az%now\, \$862_next_zeros%now\, \$872_step%now\, \rdy93%now\, \$870%now\, \$858%now\, \$851%now\, \$856_q%now\, \$869_next_pos%now\, \$857_all_zero%now\, \label91%now\, \$859_m%now\, \$852_leds_0_to_4%now\, \$863%now\, \$866%now\, \$850_cmd%now\, \$v95%now\, \$v98%now\, \$865_test%now\)
        variable \$v97\ : value(0 to 1) := (others => '0');
        variable \$850_cmd\ : value(0 to 13) := (others => '0');
        variable \$860\ : value(0 to 23) := (others => '0');
        variable \$855\ : value(0 to 12) := (others => '0');
        variable \$852_leds_0_to_4\, \$853_leds_5_to_9\ : value(0 to 4) := (others => '0');
        variable result92 : value(0 to 57) := (others => '0');
        variable \$v95\, \$859_m\, \$857_all_zero\, \$869_next_pos\, 
                 \$856_q\, \$858\, \$872_step\, \$867_az\, \$864_azp1\, 
                 \$871_az\, \$849_step\, \$874_argument\, \$873_step\, \$868\ : value(0 to 11) := (others => '0');
        variable \$865_test\, \$v98\, \$866\, \$863\, label91, \$870\, rdy93, 
                 \$861_next_pos_is_zero\ : value(0 to 0) := (others => '0');
        variable \$862_next_zeros\ : value(0 to 31) := (others => '0');
        variable \$851\, \$854\ : value(0 to 56) := (others => '0');
        variable state : t_state;
        
    begin
      \$855\ := \$855%now\;
      \$868\ := \$868%now\;
      \$873_step\ := \$873_step%now\;
      \$854\ := \$854%now\;
      result92 := \result92%now\;
      \$874_argument\ := \$874_argument%now\;
      \$853_leds_5_to_9\ := \$853_leds_5_to_9%now\;
      \$v97\ := \$v97%now\;
      \$849_step\ := \$849_step%now\;
      \$860\ := \$860%now\;
      \$861_next_pos_is_zero\ := \$861_next_pos_is_zero%now\;
      \$871_az\ := \$871_az%now\;
      \$864_azp1\ := \$864_azp1%now\;
      \$867_az\ := \$867_az%now\;
      \$862_next_zeros\ := \$862_next_zeros%now\;
      \$872_step\ := \$872_step%now\;
      rdy93 := \rdy93%now\;
      \$870\ := \$870%now\;
      \$858\ := \$858%now\;
      \$851\ := \$851%now\;
      \$856_q\ := \$856_q%now\;
      \$869_next_pos\ := \$869_next_pos%now\;
      \$857_all_zero\ := \$857_all_zero%now\;
      label91 := \label91%now\;
      \$859_m\ := \$859_m%now\;
      \$852_leds_0_to_4\ := \$852_leds_0_to_4%now\;
      \$863\ := \$863%now\;
      \$866\ := \$866%now\;
      \$850_cmd\ := \$850_cmd%now\;
      \$v95\ := \$v95%now\;
      \$v98\ := \$v98%now\;
      \$865_test\ := \$865_test%now\;
      state := \state%now\;
       -- case state is when IDLE94 =>
      rdy93 := eclat_false;
      \$874_argument\ := argument;
      \$849_step\ := eclat_resize(\$874_argument\(0 to 9),12);
      \$850_cmd\ := eclat_if(""&\$874_argument\(10) & "01" & \$849_step\ & "10" & \$849_step\);
      if label91(0) = '1' then
        
      else
        label91 := eclat_true;
        \$854\ := "00000000000000000000000000000000" & "000000000000" & "000000110010" & eclat_false;
      end if;
      \$v98\ := ""&\$874_argument\(11);
      if \$v98\(0) = '1' then
        \$854\ := \$854\;
      else
        \$v97\ := \$850_cmd\(0 to 1);
        \$v95\ := \$850_cmd\(2 to 13);
        case \$v97\ is
        when "10" =>
          \$872_step\ := \$v95\(0 to 11);
          \$855\ := eclat_false & \$872_step\;
        when "01" =>
          \$873_step\ := \$v95\(0 to 11);
          \$855\ := eclat_true & \$873_step\;
        when others =>
          
        end case;
        \$856_q\ := work.Int.div(\$855\(1 to 12), "000001100100");
        \$857_all_zero\ := work.Int.add(\$854\(32 to 43), \$856_q\);
        \$858\ := eclat_if(""&\$855\(0) & work.Int.neg(\$855\(1 to 12)) & \$855\(1 to 12));
        \$859_m\ := work.Int.add(\$854\(44 to 55), \$858\);
        \$864_azp1\ := work.Int.add(\$857_all_zero\, "000000000001");
        \$865_test\ := eclat_if(""&\$855\(0) & work.Int.lt(\$859_m\, "000000000000") & 
                       work.Int.ge(\$859_m\, "000001100100"));
        \$866\ := work.Bool.land(""&\$855\(0), ""&\$854\(56));
        \$867_az\ := eclat_if(\$866\ & \$857_all_zero\ & \$864_azp1\);
        \$868\ := eclat_if(""&\$855\(0) & "000001100100" & work.Int.neg(
                                                           "000001100100"));
        \$869_next_pos\ := work.Int.add(\$859_m\, \$868\);
        \$870\ := work.Values.equal(\$859_m\, "000000000000");
        \$871_az\ := eclat_if(\$870\ & \$864_azp1\ & \$857_all_zero\);
        \$860\ := eclat_if(\$865_test\ & \$867_az\ & \$869_next_pos\ & \$871_az\ & \$859_m\);
        \$861_next_pos_is_zero\ := work.Values.equal(\$860\(12 to 23), "000000000000");
        \$862_next_zeros\ := eclat_if(\$861_next_pos_is_zero\ & work.Int.add(
                                                                \$854\(0 to 31), "00000000000000000000000000000001") & \$854\(0 to 31));
        \$863\ := eclat_if(eclat_true & eclat_unit & eclat_unit);
        \$854\ := \$862_next_zeros\ & \$860\(0 to 11) & \$860\(12 to 23) & \$861_next_pos_is_zero\;
      end if;
      \$851\ := \$854\;
      \$852_leds_0_to_4\ := eclat_resize(\$851\(32 to 43),5);
      \$853_leds_5_to_9\ := eclat_resize(\$851\(0 to 31),5);
      result92 := \$852_leds_0_to_4\ & \$853_leds_5_to_9\ & "000000000000000000000000000000000000000000000000";
      rdy93 := eclat_true;
      state := IDLE94;
      -- end case;
      \state%next\ <= state;
      \$855%next\ <= \$855\;
      \$868%next\ <= \$868\;
      \$873_step%next\ <= \$873_step\;
      \$854%next\ <= \$854\;
      \result92%next\ <= result92;
      \$874_argument%next\ <= \$874_argument\;
      \$853_leds_5_to_9%next\ <= \$853_leds_5_to_9\;
      \$v97%next\ <= \$v97\;
      \$849_step%next\ <= \$849_step\;
      \$860%next\ <= \$860\;
      \$861_next_pos_is_zero%next\ <= \$861_next_pos_is_zero\;
      \$871_az%next\ <= \$871_az\;
      \$864_azp1%next\ <= \$864_azp1\;
      \$867_az%next\ <= \$867_az\;
      \$862_next_zeros%next\ <= \$862_next_zeros\;
      \$872_step%next\ <= \$872_step\;
      \rdy93%next\ <= rdy93;
      \$870%next\ <= \$870\;
      \$858%next\ <= \$858\;
      \$851%next\ <= \$851\;
      \$856_q%next\ <= \$856_q\;
      \$869_next_pos%next\ <= \$869_next_pos\;
      \$857_all_zero%next\ <= \$857_all_zero\;
      \label91%next\ <= label91;
      \$859_m%next\ <= \$59_m\;
      \$852_leds_0_to_4%next\ <= \$852_leds_0_to_4\;
      \$863%next\ <= \$863\;
      \$866%next\ <= \$866\;
      \$850_cmd%next\ <= \$850_cmd\;
      \$v95%next\ <= \$v95\;
      \$v98%next\ <= \$v98\;
      \$865_test%next\ <= \$865_test\;
      
      
      result <= result92;
      end process;
  end architecture;