-- code generated from the following source code:
--   toto.ecl
--
-- with the following command:
--
--    ./eclat -top=x:1|y:1 -clk-top=clk48 -relax toto.ecl

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.runtime.all;


entity top is
  port (signal clk48 : in std_logic;
        signal x : in std_logic;
        signal y : out std_logic
  );
end entity;

architecture rtl of top is

    component main is
        port (signal clk : in std_logic;
              signal reset : in std_logic;
              signal rdy : out value(0 to 0);
              signal argument : in value(0 to 0);
              signal result : out value(0 to 0)
        );
    end component;
    signal RST : std_logic := '1';
    signal argument : value(0 to 0);
    signal next_result : value(0 to 0);
    signal result : value(0 to 0);
    signal ready : value (0 to 0);
    begin
        process (clk48)
            begin
            if (rising_edge(clk48)) then
                if RST = '1' then
                    RST <= '0';
                end if;
            end if;
        end process;
argument <= "" & x;
main_CC : component main
        port map (clk => clk48,
                  reset => RST,
                  rdy => ready,
                  argument => argument,
                  result => next_result
                  );

      process (RST, clk48) 
      begin
        if RST = '1' then
          result <= (others => '0');
        elsif (rising_edge(clk48)) then
          result <= next_result;   -- resynchronize output
        end if;
      end process;
y <= result(0);

end architecture;

