-- code generated from the following source code:
--   ../ocaml-vm/vm/mlvalue.ecl
--   ../ocaml-vm/vm/fail.ecl
--   ../ocaml-vm/vm/ram.ecl
--   ../ocaml-vm/vm/runtime.ecl
--   ../ocaml-vm/vm/debug.ecl
--   ../ocaml-vm/vm/alloc.ecl
--   ../ocaml-vm/vm/prims.ecl
--   ../ocaml-vm/bytecode.ecl
--   ../ocaml-vm/vm/vm.ecl
--   ../ocaml-vm/vm/target-specific/intel-max10/IOs.ecl
--   ../ocaml-vm/vm/target-specific/intel-max10/main.ecl
--
-- with the following command:
--
--    ./eclat -arg ((true,true,true,true,true,true,true,true,true,true),(true,false)) -intel-max10 ../ocaml-vm/vm/mlvalue.ecl ../ocaml-vm/vm/fail.ecl ../ocaml-vm/vm/ram.ecl ../ocaml-vm/vm/runtime.ecl ../ocaml-vm/vm/debug.ecl ../ocaml-vm/vm/alloc.ecl ../ocaml-vm/vm/prims.ecl ../ocaml-vm/bytecode.ecl ../ocaml-vm/vm/vm.ecl ../ocaml-vm/vm/target-specific/intel-max10/IOs.ecl ../ocaml-vm/vm/target-specific/intel-max10/main.ecl

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.runtime.all;


entity top is
  port (signal MAX10_CLK1_50 : in std_logic;
        signal SW : in std_logic_vector(0 to 9);
        signal KEY : in std_logic_vector(0 to 1);
        signal LEDR : out std_logic_vector(0 to 9);
        signal HEX0 : out std_logic_vector(0 to 7);
        signal HEX1 : out std_logic_vector(0 to 7);
        signal HEX2 : out std_logic_vector(0 to 7);
        signal HEX3 : out std_logic_vector(0 to 7);
        signal HEX4 : out std_logic_vector(0 to 7);
        signal HEX5 : out std_logic_vector(0 to 7)
  );
end entity;

architecture rtl of top is

    component main is
        port (signal clk : in std_logic;
              signal run : in std_logic;
              signal reset : in std_logic;
              signal rdy : out value(0 to 0);
              signal argument : in value(0 to 11);
              signal result : out value(0 to 57)
        );
    end component;
    signal RST : std_logic := '1';
    signal argument : value(0 to 11);
    signal result : value(0 to 57);
    signal ready : value (0 to 0);
    begin
        process (MAX10_CLK1_50)
            begin
            if (rising_edge(MAX10_CLK1_50)) then
                if RST = '1' then
                    RST <= '0';
                end if;
            end if;
        end process;
argument <= SW & KEY;
main_CC : component main
        port map (clk => MAX10_CLK1_50,
                  run => '1',
                  reset => RST,
                  rdy => ready,
                  argument => argument,
                  result => result
                  );
LEDR <= result(0 to 9);
HEX0 <= result(10 to 17);
HEX1 <= result(18 to 25);
HEX2 <= result(26 to 33);
HEX3 <= result(34 to 41);
HEX4 <= result(42 to 49);
HEX5 <= result(50 to 57);

end architecture;

