-- code generated from the following source code:
--   stdlib.ecl
--   ../examples/archi/alu.ecl
--   ../examples/archi/proc2.ecl
--
-- with the following command:
--
--    ./eclat ../examples/archi/alu.ecl ../examples/archi/proc2.ecl

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.all;


entity main is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal argument : in Values.t(0 to 0);
       signal result : out Values.t(0 to 1394));
       
end entity;
architecture rtl of main is

  type t_state is (IDLE218);
  signal \state%now\, \state%next\: t_state;
  type t_state_var263 is (IDLE227, \$1811_DIV_AUX165\);
  signal \state_var263%now\, \state_var263%next\: t_state_var263;
  signal \$v238%next\, \$v238%now\, \$v222%next\, \$v222%now\ : Values.t(0 to 1) := (others => '0');
  signal \$1800%next\, \$1800%now\ : Values.t(0 to 138) := (others => '0');
  signal \$1811_div_aux165_arg%next\, \$1811_div_aux165_arg%now\ : Values.t(0 to 95) := (others => '0');
  signal \$1786_code%next\, \$1786_code%now\ : Values.t(0 to 953) := (others => '0');
  signal \$1797%next\, \$1797%now\ : Values.t(0 to 32) := (others => '0');
  signal \$1796_next_mem_wb%next\, \$1796_next_mem_wb%now\ : Values.t(0 to 63) := (others => '0');
  signal \$1801_next_if_de_instr%next\, \$1801_next_if_de_instr%now\ : Values.t(0 to 105) := (others => '0');
  signal \$1799_next_de_ex%next\, \$1799_next_de_ex%now\ : Values.t(0 to 102) := (others => '0');
  signal \$1840%next\, \$1840%now\, \$v237%next\, \$v237%now\, \$1846%next\, 
         \$1846%now\, \$1852%next\, \$1852%now\, \$1838%next\, \$1838%now\, 
         \$1843%next\, \$1843%now\, \$1789%next\, \$1789%now\, \$1849%next\, 
         \$1849%now\, \$1783%next\, \$1783%now\, \$1780%next\, \$1780%now\, 
         \$1791%next\, \$1791%now\, \$1835%next\, \$1835%now\ : Values.t(0 to 6) := (others => '0');
  signal \$1841%next\, \$1841%now\, \$1850%next\, \$1850%now\, \$1781%next\, 
         \$1781%now\, \$1853%next\, \$1853%now\, \$1784%next\, \$1784%now\, 
         \$1839%next\, \$1839%now\, \$1836%next\, \$1836%now\, \$1847%next\, 
         \$1847%now\, \$1834%next\, \$1834%now\, \$1844%next\, \$1844%now\, 
         \$v221%next\, \$v221%now\ : Values.t(0 to 2) := (others => '0');
  signal \$1794%next\, \$1794%now\, \$1854%next\, \$1854%now\, 
         \$1792_config0%next\, \$1792_config0%now\, \result216%next\, 
         \result216%now\ : Values.t(0 to 1394) := (others => '0');
  signal \$1811_div_aux165_id%next\, \$1811_div_aux165_id%now\ : Values.t(0 to 11) := (others => '0');
  signal \$v232%next\, \$v232%now\, \$v236%next\, \$v236%now\ : Values.t(0 to 3) := (others => '0');
  signal \$1842%next\, \$1842%now\, \$1779%next\, \$1779%now\, \$1782%next\, 
         \$1782%now\, \$1790%next\, \$1790%now\, \$1785%next\, \$1785%now\, 
         \$1788%next\, \$1788%now\, \$1851%next\, \$1851%now\, \$1837%next\, 
         \$1837%now\, \$1845%next\, \$1845%now\, \$1813_cmd%next\, 
         \$1813_cmd%now\, \$1848%next\, \$1848%now\, \$v229%next\, 
         \$v229%now\ : Values.t(0 to 4) := (others => '0');
  signal \$1787%next\, \$1787%now\, \$1795_next_reg_file%next\, 
         \$1795_next_reg_file%now\ : Values.t(0 to 1023) := (others => '0');
  signal \$1798%next\, \$1798%now\ : Values.t(0 to 64) := (others => '0');
  signal \$v224%next\, \$v224%now\, \$1812%next\, \$1812%now\, \$v243%next\, 
         \$v243%now\, \$v246%next\, \$v246%now\, \$v248%next\, \$v248%now\, 
         \$1818%next\, \$1818%now\, \$1831%next\, \$1831%now\, \$v251%next\, 
         \$v251%now\, \$1810%next\, \$1810%now\, \$v258%next\, \$v258%now\, 
         \$v242%next\, \$v242%now\, \label214%next\, \label214%now\, 
         \$v241%next\, \$v241%now\, \$v249%next\, \$v249%now\, \$v234%next\, 
         \$v234%now\, \$v223%next\, \$v223%now\, \$v220%next\, \$v220%now\, 
         \$1808%next\, \$1808%now\, \$v245%next\, \$v245%now\, \$v256%next\, 
         \$v256%now\, \$1814_neg_b%next\, \$1814_neg_b%now\, \$v259%next\, 
         \$v259%now\, \$v252%next\, \$v252%now\, \$v250%next\, \$v250%now\, 
         \$v244%next\, \$v244%now\, \$1806%next\, \$1806%now\, \$v253%next\, 
         \$v253%now\, \$v254%next\, \$v254%now\, \$v247%next\, \$v247%now\, 
         \$v219%next\, \$v219%now\, \$v260%next\, \$v260%now\, \$1820%next\, 
         \$1820%now\, \$v230%next\, \$v230%now\, \$v233%next\, \$v233%now\, 
         \$1821%next\, \$1821%now\, \$v228%next\, \$v228%now\, \rdy217%next\, 
         \rdy217%now\, \$1809%next\, \$1809%now\, \$v240%next\, \$v240%now\, 
         \rdy226%next\, \rdy226%now\, \$1828%next\, \$1828%now\, 
         \$1807%next\, \$1807%now\, \$1805%next\, \$1805%now\, \$v255%next\, 
         \$v255%now\, \$v261%next\, \$v261%now\, \$v262%next\, \$v262%now\, 
         \$v257%next\, \$v257%now\, \$1819%next\, \$1819%now\ : Values.t(0 to 0) := (others => '0');
  signal \$1825%next\, \$1825%now\, \$1811_div_aux165_result%next\, 
         \$1811_div_aux165_result%now\, \$1803_next_de_ex_a%next\, 
         \$1803_next_de_ex_a%now\, \result225%next\, \result225%now\, 
         \$1802%next\, \$1802%now\, \$1822_y%next\, \$1822_y%now\, 
         \$1827%next\, \$1827%now\, \$1824%next\, \$1824%now\, \$1815%next\, 
         \$1815%now\, \$1823%next\, \$1823%now\, \$1804_next_de_ex_b%next\, 
         \$1804_next_de_ex_b%now\, \$1826%next\, \$1826%now\, \$1832%next\, 
         \$1832%now\, \$1817_v%next\, \$1817_v%now\, \$1816_x%next\, 
         \$1816_x%now\, \$1833%next\, \$1833%now\ : Values.t(0 to 31) := (others => '0');
  
  begin
    process (reset,clk)
      begin
      if reset = '1' then
        \$1798%now\ <= (others => '0');
        \$1819%now\ <= (others => '0');
        \$v221%now\ <= (others => '0');
        \$v257%now\ <= (others => '0');
        \$v262%now\ <= (others => '0');
        \$v261%now\ <= (others => '0');
        \$v255%now\ <= (others => '0');
        \$1805%now\ <= (others => '0');
        \$1811_div_aux165_arg%now\ <= (others => '0');
        \$1835%now\ <= (others => '0');
        \$1833%now\ <= (others => '0');
        \$1796_next_mem_wb%now\ <= (others => '0');
        \$1791%now\ <= (others => '0');
        \$1780%now\ <= (others => '0');
        \result216%now\ <= (others => '0');
        \$1807%now\ <= (others => '0');
        \$1816_x%now\ <= (others => '0');
        \$v229%now\ <= (others => '0');
        \$1844%now\ <= (others => '0');
        \$1828%now\ <= (others => '0');
        \$1834%now\ <= (others => '0');
        \$v236%now\ <= (others => '0');
        \rdy226%now\ <= (others => '0');
        \$1847%now\ <= (others => '0');
        \$1817_v%now\ <= (others => '0');
        \$v240%now\ <= (others => '0');
        \$1783%now\ <= (others => '0');
        \$1809%now\ <= (others => '0');
        \$1848%now\ <= (others => '0');
        \$v232%now\ <= (others => '0');
        \$1849%now\ <= (others => '0');
        \$1832%now\ <= (others => '0');
        \$1813_cmd%now\ <= (others => '0');
        \$1795_next_reg_file%now\ <= (others => '0');
        \rdy217%now\ <= (others => '0');
        \$v228%now\ <= (others => '0');
        \$1821%now\ <= (others => '0');
        \$1826%now\ <= (others => '0');
        \$v233%now\ <= (others => '0');
        \$1845%now\ <= (others => '0');
        \$1792_config0%now\ <= (others => '0');
        \$1789%now\ <= (others => '0');
        \$v230%now\ <= (others => '0');
        \$1837%now\ <= (others => '0');
        \$1800%now\ <= (others => '0');
        \$1804_next_de_ex_b%now\ <= (others => '0');
        \$1820%now\ <= (others => '0');
        \$1823%now\ <= (others => '0');
        \$v260%now\ <= (others => '0');
        \$v219%now\ <= (others => '0');
        \$v222%now\ <= (others => '0');
        \$1815%now\ <= (others => '0');
        \$v247%now\ <= (others => '0');
        \$v254%now\ <= (others => '0');
        \$1824%now\ <= (others => '0');
        \$v253%now\ <= (others => '0');
        \$1806%now\ <= (others => '0');
        \$1827%now\ <= (others => '0');
        \$1822_y%now\ <= (others => '0');
        \$1843%now\ <= (others => '0');
        \$v244%now\ <= (others => '0');
        \$v250%now\ <= (others => '0');
        \$v252%now\ <= (others => '0');
        \$1838%now\ <= (others => '0');
        \$1836%now\ <= (others => '0');
        \$v259%now\ <= (others => '0');
        \$1814_neg_b%now\ <= (others => '0');
        \$1851%now\ <= (others => '0');
        \$1839%now\ <= (others => '0');
        \$1799_next_de_ex%now\ <= (others => '0');
        \$1802%now\ <= (others => '0');
        \$1788%now\ <= (others => '0');
        \$1784%now\ <= (others => '0');
        \result225%now\ <= (others => '0');
        \$v256%now\ <= (others => '0');
        \$1787%now\ <= (others => '0');
        \$1852%now\ <= (others => '0');
        \$1801_next_if_de_instr%now\ <= (others => '0');
        \$1811_div_aux165_id%now\ <= (others => '0');
        \$1853%now\ <= (others => '0');
        \$v245%now\ <= (others => '0');
        \$1846%now\ <= (others => '0');
        \$1808%now\ <= (others => '0');
        \$v220%now\ <= (others => '0');
        \$v223%now\ <= (others => '0');
        \$1854%now\ <= (others => '0');
        \$1781%now\ <= (others => '0');
        \$1803_next_de_ex_a%now\ <= (others => '0');
        \$v234%now\ <= (others => '0');
        \$v249%now\ <= (others => '0');
        \$v237%now\ <= (others => '0');
        \$v241%now\ <= (others => '0');
        \$1811_div_aux165_result%now\ <= (others => '0');
        \label214%now\ <= (others => '0');
        \$1797%now\ <= (others => '0');
        \$v242%now\ <= (others => '0');
        \$1840%now\ <= (others => '0');
        \$v258%now\ <= (others => '0');
        \$1810%now\ <= (others => '0');
        \$1786_code%now\ <= (others => '0');
        \$v251%now\ <= (others => '0');
        \$1831%now\ <= (others => '0');
        \$1850%now\ <= (others => '0');
        \$1785%now\ <= (others => '0');
        \$1818%now\ <= (others => '0');
        \$v248%now\ <= (others => '0');
        \$1790%now\ <= (others => '0');
        \$1841%now\ <= (others => '0');
        \$1782%now\ <= (others => '0');
        \$1779%now\ <= (others => '0');
        \$1825%now\ <= (others => '0');
        \$v246%now\ <= (others => '0');
        \$v238%now\ <= (others => '0');
        \$v243%now\ <= (others => '0');
        \$1812%now\ <= (others => '0');
        \$v224%now\ <= (others => '0');
        \$1794%now\ <= (others => '0');
        \$1842%now\ <= (others => '0');
        \state%now\ <= idle218;
        \state_var263%now\ <= idle227;
      elsif (rising_edge(clk)) then
        \$1798%now\ <= \$1798%next\;
        \$1819%now\ <= \$1819%next\;
        \$v221%now\ <= \$v221%next\;
        \$v257%now\ <= \$v257%next\;
        \$v262%now\ <= \$v262%next\;
        \$v261%now\ <= \$v261%next\;
        \$v255%now\ <= \$v255%next\;
        \$1805%now\ <= \$1805%next\;
        \$1811_div_aux165_arg%now\ <= \$1811_div_aux165_arg%next\;
        \$1835%now\ <= \$1835%next\;
        \$1833%now\ <= \$1833%next\;
        \$1796_next_mem_wb%now\ <= \$1796_next_mem_wb%next\;
        \$1791%now\ <= \$1791%next\;
        \$1780%now\ <= \$1780%next\;
        \result216%now\ <= \result216%next\;
        \$1807%now\ <= \$1807%next\;
        \$1816_x%now\ <= \$1816_x%next\;
        \$v229%now\ <= \$v229%next\;
        \$1844%now\ <= \$1844%next\;
        \$1828%now\ <= \$1828%next\;
        \$1834%now\ <= \$1834%next\;
        \$v236%now\ <= \$v236%next\;
        \rdy226%now\ <= \rdy226%next\;
        \$1847%now\ <= \$1847%next\;
        \$1817_v%now\ <= \$1817_v%next\;
        \$v240%now\ <= \$v240%next\;
        \$1783%now\ <= \$1783%next\;
        \$1809%now\ <= \$1809%next\;
        \$1848%now\ <= \$1848%next\;
        \$v232%now\ <= \$v232%next\;
        \$1849%now\ <= \$1849%next\;
        \$1832%now\ <= \$1832%next\;
        \$1813_cmd%now\ <= \$1813_cmd%next\;
        \$1795_next_reg_file%now\ <= \$1795_next_reg_file%next\;
        \rdy217%now\ <= \rdy217%next\;
        \$v228%now\ <= \$v228%next\;
        \$1821%now\ <= \$1821%next\;
        \$1826%now\ <= \$1826%next\;
        \$v233%now\ <= \$v233%next\;
        \$1845%now\ <= \$1845%next\;
        \$1792_config0%now\ <= \$1792_config0%next\;
        \$1789%now\ <= \$1789%next\;
        \$v230%now\ <= \$v230%next\;
        \$1837%now\ <= \$1837%next\;
        \$1800%now\ <= \$1800%next\;
        \$1804_next_de_ex_b%now\ <= \$1804_next_de_ex_b%next\;
        \$1820%now\ <= \$1820%next\;
        \$1823%now\ <= \$1823%next\;
        \$v260%now\ <= \$v260%next\;
        \$v219%now\ <= \$v219%next\;
        \$v222%now\ <= \$v222%next\;
        \$1815%now\ <= \$1815%next\;
        \$v247%now\ <= \$v247%next\;
        \$v254%now\ <= \$v254%next\;
        \$1824%now\ <= \$1824%next\;
        \$v253%now\ <= \$v253%next\;
        \$1806%now\ <= \$1806%next\;
        \$1827%now\ <= \$1827%next\;
        \$1822_y%now\ <= \$1822_y%next\;
        \$1843%now\ <= \$1843%next\;
        \$v244%now\ <= \$v244%next\;
        \$v250%now\ <= \$v250%next\;
        \$v252%now\ <= \$v252%next\;
        \$1838%now\ <= \$1838%next\;
        \$1836%now\ <= \$1836%next\;
        \$v259%now\ <= \$v259%next\;
        \$1814_neg_b%now\ <= \$1814_neg_b%next\;
        \$1851%now\ <= \$1851%next\;
        \$1839%now\ <= \$1839%next\;
        \$1799_next_de_ex%now\ <= \$1799_next_de_ex%next\;
        \$1802%now\ <= \$1802%next\;
        \$1788%now\ <= \$1788%next\;
        \$1784%now\ <= \$1784%next\;
        \result225%now\ <= \result225%next\;
        \$v256%now\ <= \$v256%next\;
        \$1787%now\ <= \$1787%next\;
        \$1852%now\ <= \$1852%next\;
        \$1801_next_if_de_instr%now\ <= \$1801_next_if_de_instr%next\;
        \$1811_div_aux165_id%now\ <= \$1811_div_aux165_id%next\;
        \$1853%now\ <= \$1853%next\;
        \$v245%now\ <= \$v245%next\;
        \$1846%now\ <= \$1846%next\;
        \$1808%now\ <= \$1808%next\;
        \$v220%now\ <= \$v220%next\;
        \$v223%now\ <= \$v223%next\;
        \$1854%now\ <= \$1854%next\;
        \$1781%now\ <= \$1781%next\;
        \$1803_next_de_ex_a%now\ <= \$1803_next_de_ex_a%next\;
        \$v234%now\ <= \$v234%next\;
        \$v249%now\ <= \$v249%next\;
        \$v237%now\ <= \$v237%next\;
        \$v241%now\ <= \$v241%next\;
        \$1811_div_aux165_result%now\ <= \$1811_div_aux165_result%next\;
        \label214%now\ <= \label214%next\;
        \$1797%now\ <= \$1797%next\;
        \$v242%now\ <= \$v242%next\;
        \$1840%now\ <= \$1840%next\;
        \$v258%now\ <= \$v258%next\;
        \$1810%now\ <= \$1810%next\;
        \$1786_code%now\ <= \$1786_code%next\;
        \$v251%now\ <= \$v251%next\;
        \$1831%now\ <= \$1831%next\;
        \$1850%now\ <= \$1850%next\;
        \$1785%now\ <= \$1785%next\;
        \$1818%now\ <= \$1818%next\;
        \$v248%now\ <= \$v248%next\;
        \$1790%now\ <= \$1790%next\;
        \$1841%now\ <= \$1841%next\;
        \$1782%now\ <= \$1782%next\;
        \$1779%now\ <= \$1779%next\;
        \$1825%now\ <= \$1825%next\;
        \$v246%now\ <= \$v246%next\;
        \$v238%now\ <= \$v238%next\;
        \$v243%now\ <= \$v243%next\;
        \$1812%now\ <= \$1812%next\;
        \$v224%now\ <= \$v224%next\;
        \$1794%now\ <= \$1794%next\;
        \$1842%now\ <= \$1842%next\;
        \state_var263%now\ <= \state_var263%next\;
        \state%now\ <= \state%next\;
      end if;
    end process;
      
      process(argument,\state%now\, clk,\state_var263%now\, \$1798%now\, \$1819%now\, \$v221%now\, \$v257%now\, \$v262%now\, \$v261%now\, \$v255%now\, \$1805%now\, \$1811_div_aux165_arg%now\, \$1835%now\, \$1833%now\, \$1796_next_mem_wb%now\, \$1791%now\, \$1780%now\, \result216%now\, \$1807%now\, \$1816_x%now\, \$v229%now\, \$1844%now\, \$1828%now\, \$1834%now\, \$v236%now\, \rdy226%now\, \$1847%now\, \$1817_v%now\, \$v240%now\, \$1783%now\, \$1809%now\, \$1848%now\, \$v232%now\, \$1849%now\, \$1832%now\, \$1813_cmd%now\, \$1795_next_reg_file%now\, \rdy217%now\, \$v228%now\, \$1821%now\, \$1826%now\, \$v233%now\, \$1845%now\, \$1792_config0%now\, \$1789%now\, \$v230%now\, \$1837%now\, \$1800%now\, \$1804_next_de_ex_b%now\, \$1820%now\, \$1823%now\, \$v260%now\, \$v219%now\, \$v222%now\, \$1815%now\, \$v247%now\, \$v254%now\, \$1824%now\, \$v253%now\, \$1806%now\, \$1827%now\, \$1822_y%now\, \$1843%now\, \$v244%now\, \$v250%now\, \$v252%now\, \$1838%now\, \$1836%now\, \$v259%now\, \$1814_neg_b%now\, \$1851%now\, \$1839%now\, \$1799_next_de_ex%now\, \$1802%now\, \$1788%now\, \$1784%now\, \result225%now\, \$v256%now\, \$1787%now\, \$1852%now\, \$1801_next_if_de_instr%now\, \$1811_div_aux165_id%now\, \$1853%now\, \$v245%now\, \$1846%now\, \$1808%now\, \$v220%now\, \$v223%now\, \$1854%now\, \$1781%now\, \$1803_next_de_ex_a%now\, \$v234%now\, \$v249%now\, \$v237%now\, \$v241%now\, \$1811_div_aux165_result%now\, \label214%now\, \$1797%now\, \$v242%now\, \$1840%now\, \$v258%now\, \$1810%now\, \$1786_code%now\, \$v251%now\, \$1831%now\, \$1850%now\, \$1785%now\, \$1818%now\, \$v248%now\, \$1790%now\, \$1841%now\, \$1782%now\, \$1779%now\, \$1825%now\, \$v246%now\, \$v238%now\, \$v243%now\, \$1812%now\, \$v224%now\, \$1794%now\, \$1842%now\)
        variable \$v238\, \$v222\ : Values.t(0 to 1) := (others => '0');
        variable \$1800\ : Values.t(0 to 138) := (others => '0');
        variable \$1811_div_aux165_arg\ : Values.t(0 to 95) := (others => '0');
        variable \$1786_code\ : Values.t(0 to 953) := (others => '0');
        variable \$1797\ : Values.t(0 to 32) := (others => '0');
        variable \$1796_next_mem_wb\ : Values.t(0 to 63) := (others => '0');
        variable \$1801_next_if_de_instr\ : Values.t(0 to 105) := (others => '0');
        variable \$1799_next_de_ex\ : Values.t(0 to 102) := (others => '0');
        variable \$1840\, \$v237\, \$1846\, \$1852\, \$1838\, \$1843\, 
                 \$1789\, \$1849\, \$1783\, \$1780\, \$1791\, \$1835\ : Values.t(0 to 6) := (others => '0');
        variable \$1841\, \$1850\, \$1781\, \$1853\, \$1784\, \$1839\, 
                 \$1836\, \$1847\, \$1834\, \$1844\, \$v221\ : Values.t(0 to 2) := (others => '0');
        variable \$1794\, \$1854\, \$1792_config0\, result216 : Values.t(0 to 1394) := (others => '0');
        variable \$1811_div_aux165_id\ : Values.t(0 to 11) := (others => '0');
        variable \$v232\, \$v236\ : Values.t(0 to 3) := (others => '0');
        variable \$1842\, \$1779\, \$1782\, \$1790\, \$1785\, \$1788\, 
                 \$1851\, \$1837\, \$1845\, \$1813_cmd\, \$1848\, \$v229\ : Values.t(0 to 4) := (others => '0');
        variable \$1787\, \$1795_next_reg_file\ : Values.t(0 to 1023) := (others => '0');
        variable \$1798\ : Values.t(0 to 64) := (others => '0');
        variable \$v224\, \$1812\, \$v243\, \$v246\, \$v248\, \$1818\, 
                 \$1831\, \$v251\, \$1810\, \$v258\, \$v242\, label214, 
                 \$v241\, \$v249\, \$v234\, \$v223\, \$v220\, \$1808\, 
                 \$v245\, \$v256\, \$1814_neg_b\, \$v259\, \$v252\, \$v250\, 
                 \$v244\, \$1806\, \$v253\, \$v254\, \$v247\, \$v219\, 
                 \$v260\, \$1820\, \$v230\, \$v233\, \$1821\, \$v228\, 
                 rdy217, \$1809\, \$v240\, rdy226, \$1828\, \$1807\, \$1805\, 
                 \$v255\, \$v261\, \$v262\, \$v257\, \$1819\ : Values.t(0 to 0) := (others => '0');
        variable \$1825\, \$1811_div_aux165_result\, \$1803_next_de_ex_a\, 
                 result225, \$1802\, \$1822_y\, \$1827\, \$1824\, \$1815\, 
                 \$1823\, \$1804_next_de_ex_b\, \$1826\, \$1832\, \$1817_v\, 
                 \$1816_x\, \$1833\ : Values.t(0 to 31) := (others => '0');
        variable state : t_state;
        variable state_var263 : t_state_var263;
        
    begin
      \$1798\ := \$1798%now\;
      \$1819\ := \$1819%now\;
      \$v221\ := \$v221%now\;
      \$v257\ := \$v257%now\;
      \$v262\ := \$v262%now\;
      \$v261\ := \$v261%now\;
      \$v255\ := \$v255%now\;
      \$1805\ := \$1805%now\;
      \$1811_div_aux165_arg\ := \$1811_div_aux165_arg%now\;
      \$1835\ := \$1835%now\;
      \$1833\ := \$1833%now\;
      \$1796_next_mem_wb\ := \$1796_next_mem_wb%now\;
      \$1791\ := \$1791%now\;
      \$1780\ := \$1780%now\;
      result216 := \result216%now\;
      \$1807\ := \$1807%now\;
      \$1816_x\ := \$1816_x%now\;
      \$v229\ := \$v229%now\;
      \$1844\ := \$1844%now\;
      \$1828\ := \$1828%now\;
      \$1834\ := \$1834%now\;
      \$v236\ := \$v236%now\;
      rdy226 := \rdy226%now\;
      \$1847\ := \$1847%now\;
      \$1817_v\ := \$1817_v%now\;
      \$v240\ := \$v240%now\;
      \$1783\ := \$1783%now\;
      \$1809\ := \$1809%now\;
      \$1848\ := \$1848%now\;
      \$v232\ := \$v232%now\;
      \$1849\ := \$1849%now\;
      \$1832\ := \$1832%now\;
      \$1813_cmd\ := \$1813_cmd%now\;
      \$1795_next_reg_file\ := \$1795_next_reg_file%now\;
      rdy217 := \rdy217%now\;
      \$v228\ := \$v228%now\;
      \$1821\ := \$1821%now\;
      \$1826\ := \$1826%now\;
      \$v233\ := \$v233%now\;
      \$1845\ := \$1845%now\;
      \$1792_config0\ := \$1792_config0%now\;
      \$1789\ := \$1789%now\;
      \$v230\ := \$v230%now\;
      \$1837\ := \$1837%now\;
      \$1800\ := \$1800%now\;
      \$1804_next_de_ex_b\ := \$1804_next_de_ex_b%now\;
      \$1820\ := \$1820%now\;
      \$1823\ := \$1823%now\;
      \$v260\ := \$v260%now\;
      \$v219\ := \$v219%now\;
      \$v222\ := \$v222%now\;
      \$1815\ := \$1815%now\;
      \$v247\ := \$v247%now\;
      \$v254\ := \$v254%now\;
      \$1824\ := \$1824%now\;
      \$v253\ := \$v253%now\;
      \$1806\ := \$1806%now\;
      \$1827\ := \$1827%now\;
      \$1822_y\ := \$1822_y%now\;
      \$1843\ := \$1843%now\;
      \$v244\ := \$v244%now\;
      \$v250\ := \$v250%now\;
      \$v252\ := \$v252%now\;
      \$1838\ := \$1838%now\;
      \$1836\ := \$1836%now\;
      \$v259\ := \$v259%now\;
      \$1814_neg_b\ := \$1814_neg_b%now\;
      \$1851\ := \$1851%now\;
      \$1839\ := \$1839%now\;
      \$1799_next_de_ex\ := \$1799_next_de_ex%now\;
      \$1802\ := \$1802%now\;
      \$1788\ := \$1788%now\;
      \$1784\ := \$1784%now\;
      result225 := \result225%now\;
      \$v256\ := \$v256%now\;
      \$1787\ := \$1787%now\;
      \$1852\ := \$1852%now\;
      \$1801_next_if_de_instr\ := \$1801_next_if_de_instr%now\;
      \$1811_div_aux165_id\ := \$1811_div_aux165_id%now\;
      \$1853\ := \$1853%now\;
      \$v245\ := \$v245%now\;
      \$1846\ := \$1846%now\;
      \$1808\ := \$1808%now\;
      \$v220\ := \$v220%now\;
      \$v223\ := \$v223%now\;
      \$1854\ := \$1854%now\;
      \$1781\ := \$1781%now\;
      \$1803_next_de_ex_a\ := \$1803_next_de_ex_a%now\;
      \$v234\ := \$v234%now\;
      \$v249\ := \$v249%now\;
      \$v237\ := \$v237%now\;
      \$v241\ := \$v241%now\;
      \$1811_div_aux165_result\ := \$1811_div_aux165_result%now\;
      label214 := \label214%now\;
      \$1797\ := \$1797%now\;
      \$v242\ := \$v242%now\;
      \$1840\ := \$1840%now\;
      \$v258\ := \$v258%now\;
      \$1810\ := \$1810%now\;
      \$1786_code\ := \$1786_code%now\;
      \$v251\ := \$v251%now\;
      \$1831\ := \$1831%now\;
      \$1850\ := \$1850%now\;
      \$1785\ := \$1785%now\;
      \$1818\ := \$1818%now\;
      \$v248\ := \$v248%now\;
      \$1790\ := \$1790%now\;
      \$1841\ := \$1841%now\;
      \$1782\ := \$1782%now\;
      \$1779\ := \$1779%now\;
      \$1825\ := \$1825%now\;
      \$v246\ := \$v246%now\;
      \$v238\ := \$v238%now\;
      \$v243\ := \$v243%now\;
      \$1812\ := \$1812%now\;
      \$v224\ := \$v224%now\;
      \$1794\ := \$1794%now\;
      \$1842\ := \$1842%now\;
      state := \state%now\;
      state_var263 := \state_var263%now\;
       -- case state is when IDLE218 =>
      rdy217 := work.values.val_false;
      \$v262\ := work.values.val_unit;
      \$1779\ := "0010" & \$v262\;
      \$1780\ := "01" & \$1779\;
      \$v261\ := work.values.val_unit;
      \$1781\ := "01" & \$v261\;
      \$v260\ := work.values.val_unit;
      \$1782\ := "0001" & \$v260\;
      \$1783\ := "01" & \$1782\;
      \$v259\ := work.values.val_unit;
      \$1784\ := "01" & \$v259\;
      \$v258\ := work.values.val_unit;
      \$1785\ := "0010" & \$v258\;
      \$1835\ := "01" & \$1785\;
      \$v257\ := work.values.val_unit;
      \$1836\ := "01" & \$v257\;
      \$v256\ := work.values.val_unit;
      \$1837\ := "0010" & \$v256\;
      \$1838\ := "01" & \$1837\;
      \$v255\ := work.values.val_unit;
      \$1839\ := "01" & \$v255\;
      \$v254\ := work.values.val_unit;
      \$1840\ := "10" & \$v254\&X"0";
      \$v253\ := work.values.val_unit;
      \$1841\ := "01" & \$v253\;
      \$v252\ := work.values.val_unit;
      \$1842\ := "0010" & \$v252\;
      \$1843\ := "01" & \$1842\;
      \$v251\ := work.values.val_unit;
      \$1844\ := "01" & \$v251\;
      \$v250\ := work.values.val_unit;
      \$1845\ := "0100" & \$v250\;
      \$1846\ := "01" & \$1845\;
      \$v249\ := work.values.val_unit;
      \$1847\ := "01" & \$v249\;
      \$v248\ := work.values.val_unit;
      \$1848\ := "0010" & \$v248\;
      \$1849\ := "01" & \$1848\;
      \$v247\ := work.values.val_unit;
      \$1850\ := "01" & \$v247\;
      \$v246\ := work.values.val_unit;
      \$1851\ := "0010" & \$v246\;
      \$1852\ := "01" & \$1851\;
      \$v245\ := work.values.val_unit;
      \$1853\ := "01" & \$v245\;
      \$1786_code\ := \$1780\ & \$1781\ & "00000000000000000000000000000100" & "00000000000000000000000000000000" & "00000000000000000000000011001000" & \$1783\ & \$1784\ & "00000000000000000000000000000101" & "00000000000000000000000000000000" & "00000000000000000000000000001010" & \$1835\ & \$1836\ & "00000000000000000000000000000110" & "00000000000000000000000000000001" & "00000000000000000000000000110111" & \$1838\ & \$1839\ & "00000000000000000000000000000111" & "00000000000000000000000000000000" & "00000000000000000000000001000010" & \$1840\ & \$1841\ & "00000000000000000000000000001000" & "00000000000000000000000000000100" & "00000000000000000000000000001000" & \$1843\ & \$1844\ & "00000000000000000000000000001001" & "00000000000000000000000000001010" & "00000000000000000000000000001010" & \$1846\ & \$1847\ & "00000000000000000000000000001010" & "00000000000000000000000000000100" & "00000000000000000000000000000010" & \$1849\ & \$1850\ & "00000000000000000000000000001000" & "00000000000000000000000000000100" & "00000000000000000000000000000001" & \$1852\ & \$1853\ & "00000000000000000000000000001000" & "00000000000000000000000000000100" & "00000000000000000000000000000001";
      \$1787\ := work.Vect.create(32, 1024, "00000000000000000000000000000000");
      \$v244\ := work.values.val_unit;
      \$1788\ := "0010" & \$v244\;
      \$1789\ := "01" & \$1788\;
      \$v243\ := work.values.val_unit;
      \$1790\ := "0010" & \$v243\;
      \$1791\ := "01" & \$1790\;
      \$v242\ := work.values.val_unit;
      \$1834\ := "01" & \$v242\;
      \$1792_config0\ := \$1787\ & "00000000000000000000000000000000" & "00000000000000000000000000000000" & "00000000000000000000000000000000" & "00000000000000000000000000000000" & \$1789\ & "00000000000000000000000000000000" & "00000000000000000000000000000000" & "00000000000000000000000000000000" & \$1791\ & \$1834\ & "00000000000000000000000000000000" & "00000000000000000000000000000000" & "00000000000000000000000000000000" & work.values.val_unit & "00000000000000000000000000000000" & work.Values.val_true;
      if label214(0) = '1' then
        
      else
        label214 := work.Values.val_true;
        \$1794\ := \$1792_config0\;
      end if;
      \$v241\ := ""&\$1794\(1394);
      if \$v241\(0) = '1' then
        \$1795_next_reg_file\ := work.Vect.copy_with(1088, 1024, \$1794\(0 to 1023), \$1794\(1024 to 1055), \$1794\(1056 to 1087));
      else
        \$1795_next_reg_file\ := \$1794\(0 to 1023);
      end if;
      \$v240\ := ""&\$1794\(1394);
      if \$v240\(0) = '1' then
        \$1796_next_mem_wb\ := \$1794\(1088 to 1119) & \$1794\(1120 to 1151);
      else
        \$1796_next_mem_wb\ := \$1794\(1024 to 1087);
      end if;
      case state_var263 is
      when \$1811_DIV_AUX165\ =>
        \$1831\ := work.Int.lt(\$1811_div_aux165_arg\(0 to 31), \$1811_div_aux165_arg\(32 to 63));
        \$v228\ := \$1831\;
        if \$v228\(0) = '1' then
          \$1811_div_aux165_result\ := \$1811_div_aux165_arg\(64 to 95);
          result225 := \$1811_div_aux165_result\;
          rdy226 := work.Values.val_true;
          state_var263 := IDLE227;
        else
          \$1832\ := work.Int.sub(\$1811_div_aux165_arg\(0 to 31), \$1811_div_aux165_arg\(32 to 63));
          \$1833\ := work.Int.add(\$1811_div_aux165_arg\(64 to 95), "00000000000000000000000000000001");
          \$1811_div_aux165_arg\ := \$1832\ & \$1811_div_aux165_arg\(32 to 63) & \$1833\;
          state_var263 := \$1811_DIV_AUX165\;
        end if;
      when IDLE227 =>
        rdy226 := work.values.val_false;
        \$1854\ := \$1794\;
        \$v237\ := \$1854\(1152 to 1158);
        \$v238\ := \$v237\(0 to 1);
        \$v229\ := \$v237\(2 to 6);
        case \$v238\ is
        when "10" =>
          \$1812\ := \$v229\(0 to 0);
          \$1811_div_aux165_id\ := "000000000001";
          \$1811_div_aux165_arg\ := \$1854\(1191 to 1222) & \$1854\(1223 to 1254) & "00000000000000000000000000000000";
          state_var263 := \$1811_DIV_AUX165\;
        when "01" =>
          \$1813_cmd\ := \$v229\(0 to 4);
          \$v236\ := \$1813_cmd\(0 to 3);
          \$v234\ := ""&\$1813_cmd\(4);
          case \$v236\ is
          when "0001" =>
            \$1828\ := \$v234\(0 to 0);
            \$1814_neg_b\ := work.Values.val_true;
          when others =>
            \$1814_neg_b\ := work.values.val_false;
          end case;
          \$1815\ := work.Int.lsl(\$1854\(1191 to 1222), "00000000000000000000000000000001");
          \$1816_x\ := work.Int.lor(\$1815\, "00000000000000000000000000000001");
          \$v233\ := \$1814_neg_b\;
          if \$v233\(0) = '1' then
            \$1825\ := work.Int.logical_not(\$1854\(1223 to 1254));
            \$1826\ := work.Int.lsl(\$1825\, "00000000000000000000000000000001");
            \$1822_y\ := work.Int.lor(\$1826\, "00000000000000000000000000000001");
          else
            \$1827\ := work.Int.lsl(\$1854\(1223 to 1254), "00000000000000000000000000000001");
            \$1822_y\ := work.Int.lor(\$1827\, "00000000000000000000000000000000");
          end if;
          \$1823\ := work.Int.add(\$1816_x\, \$1822_y\);
          \$1824\ := work.Int.asr(\$1823\, "00000000000000000000000000000001");
          \$1817_v\ := work.Int.resize(32, 32, \$1824\);
          \$v232\ := \$1813_cmd\(0 to 3);
          \$v230\ := ""&\$1813_cmd\(4);
          case \$v232\ is
          when "1000" =>
            \$1818\ := \$v230\(0 to 0);
            result225 := work.Int.asr(\$1854\(1191 to 1222), \$1854\(1223 to 1254));
          when "0100" =>
            \$1819\ := \$v230\(0 to 0);
            result225 := work.Int.lsl(\$1854\(1191 to 1222), \$1854\(1223 to 1254));
          when "0001" =>
            \$1820\ := \$v230\(0 to 0);
            result225 := \$1817_v\;
          when "0010" =>
            \$1821\ := \$v230\(0 to 0);
            result225 := \$1817_v\;
          when others =>
            
          end case;
          rdy226 := work.Values.val_true;
          state_var263 := IDLE227;
        when others =>
          
        end case;
      end case;
      
      if rdy226(0) = '1' then
        
      else
        result225 := "00000000000000000000000000000000";
      end if;
      \$1797\ := result225 & rdy226;
      \$v224\ := ""&\$1797\(32);
      if \$v224\(0) = '1' then
        \$1807\ := work.Print.print_string(clk,work.Util.of_string("---> finished!"));
      else
        \$1807\ := work.values.val_unit;
      end if;
      \$1808\ := work.Print.print_string(clk,work.Util.of_string("ALU_OUT:"));
      \$1809\ := work.Int.print(clk,\$1797\(0 to 31));
      \$1810\ := work.Print.print_newline(clk,work.values.val_unit);
      \$1798\ := \$1794\(1159 to 1190) & \$1797\(0 to 31) & ""&\$1797\(32);
      \$v223\ := ""&\$1798\(64);
      if \$v223\(0) = '1' then
        \$1803_next_de_ex_a\ := work.Vect.nth(1056, 32, \$1794\(0 to 1023), \$1794\(1297 to 1328));
        \$v221\ := \$1794\(1262 to 1264);
        \$v222\ := \$v221\(0 to 1);
        \$v220\ := ""&\$v221\(2);
        case \$v222\ is
        when "10" =>
          \$1805\ := \$v220\(0 to 0);
          \$1804_next_de_ex_b\ := work.Vect.nth(1056, 32, \$1794\(0 to 1023), \$1794\(1329 to 1360));
        when "01" =>
          \$1806\ := \$v220\(0 to 0);
          \$1804_next_de_ex_b\ := \$1794\(1329 to 1360);
        when others =>
          
        end case;
        \$1799_next_de_ex\ := \$1794\(1255 to 1261) & \$1794\(1265 to 1296) & \$1803_next_de_ex_a\ & \$1804_next_de_ex_b\;
      else
        \$1799_next_de_ex\ := \$1794\(1152 to 1254);
      end if;
      \$v219\ := ""&\$1798\(64);
      if \$v219\(0) = '1' then
        \$1801_next_if_de_instr\ := work.Vect.nth(986, 106, \$1786_code\, \$1794\(1362 to 1393));
        \$1802\ := work.Int.add(\$1794\(1362 to 1393), "00000000000000000000000000000001");
        \$1800\ := \$1801_next_if_de_instr\ & work.values.val_unit & \$1802\;
      else
        \$1800\ := \$1794\(1255 to 1361) & \$1794\(1362 to 1393);
      end if;
      \$1794\ := \$1795_next_reg_file\ & \$1796_next_mem_wb\ & \$1798\(0 to 63) & \$1799_next_de_ex\ & \$1800\(0 to 106) & \$1800\(107 to 138) & ""&\$1798\(64);
      result216 := \$1794\;
      rdy217 := work.Values.val_true;
      state := IDLE218;
      -- end case;
      \state%next\ <= state;
      \state_var263%next\ <= state_var263;
      \$1798%next\ <= \$1798\;
      \$1819%next\ <= \$1819\;
      \$v221%next\ <= \$v221\;
      \$v257%next\ <= \$v257\;
      \$v262%next\ <= \$v262\;
      \$v261%next\ <= \$v261\;
      \$v255%next\ <= \$v255\;
      \$1805%next\ <= \$1805\;
      \$1811_div_aux165_arg%next\ <= \$1811_div_aux165_arg\;
      \$1835%next\ <= \$1835\;
      \$1833%next\ <= \$1833\;
      \$1796_next_mem_wb%next\ <= \$1796_next_mem_wb\;
      \$1791%next\ <= \$1791\;
      \$1780%next\ <= \$1780\;
      \result216%next\ <= result216;
      \$1807%next\ <= \$1807\;
      \$1816_x%next\ <= \$1816_x\;
      \$v229%next\ <= \$v229\;
      \$1844%next\ <= \$1844\;
      \$1828%next\ <= \$1828\;
      \$1834%next\ <= \$1834\;
      \$v236%next\ <= \$v236\;
      \rdy226%next\ <= rdy226;
      \$1847%next\ <= \$1847\;
      \$1817_v%next\ <= \$1817_v\;
      \$v240%next\ <= \$v240\;
      \$1783%next\ <= \$1783\;
      \$1809%next\ <= \$1809\;
      \$1848%next\ <= \$1848\;
      \$v232%next\ <= \$v232\;
      \$1849%next\ <= \$1849\;
      \$1832%next\ <= \$1832\;
      \$1813_cmd%next\ <= \$1813_cmd\;
      \$1795_next_reg_file%next\ <= \$1795_next_reg_file\;
      \rdy217%next\ <= rdy217;
      \$v228%next\ <= \$v228\;
      \$1821%next\ <= \$1821\;
      \$1826%next\ <= \$1826\;
      \$v233%next\ <= \$v233\;
      \$1845%next\ <= \$1845\;
      \$1792_config0%next\ <= \$1792_config0\;
      \$1789%next\ <= \$1789\;
      \$v230%next\ <= \$v230\;
      \$1837%next\ <= \$1837\;
      \$1800%next\ <= \$1800\;
      \$1804_next_de_ex_b%next\ <= \$1804_next_de_ex_b\;
      \$1820%next\ <= \$1820\;
      \$1823%next\ <= \$1823\;
      \$v260%next\ <= \$v260\;
      \$v219%next\ <= \$v219\;
      \$v222%next\ <= \$v222\;
      \$1815%next\ <= \$1815\;
      \$v247%next\ <= \$v247\;
      \$v254%next\ <= \$v254\;
      \$1824%next\ <= \$1824\;
      \$v253%next\ <= \$v253\;
      \$1806%next\ <= \$1806\;
      \$1827%next\ <= \$1827\;
      \$1822_y%next\ <= \$1822_y\;
      \$1843%next\ <= \$1843\;
      \$v244%next\ <= \$v244\;
      \$v250%next\ <= \$v250\;
      \$v252%next\ <= \$v252\;
      \$1838%next\ <= \$1838\;
      \$1836%next\ <= \$1836\;
      \$v259%next\ <= \$v259\;
      \$1814_neg_b%next\ <= \$1814_neg_b\;
      \$1851%next\ <= \$1851\;
      \$1839%next\ <= \$1839\;
      \$1799_next_de_ex%next\ <= \$1799_next_de_ex\;
      \$1802%next\ <= \$1802\;
      \$1788%next\ <= \$1788\;
      \$1784%next\ <= \$1784\;
      \result225%next\ <= result225;
      \$v256%next\ <= \$v256\;
      \$1787%next\ <= \$1787\;
      \$1852%next\ <= \$1852\;
      \$1801_next_if_de_instr%next\ <= \$1801_next_if_de_instr\;
      \$1811_div_aux165_id%next\ <= \$1811_div_aux165_id\;
      \$1853%next\ <= \$1853\;
      \$v245%next\ <= \$v245\;
      \$1846%next\ <= \$1846\;
      \$1808%next\ <= \$1808\;
      \$v220%next\ <= \$v220\;
      \$v223%next\ <= \$v223\;
      \$1854%next\ <= \$1854\;
      \$1781%next\ <= \$1781\;
      \$1803_next_de_ex_a%next\ <= \$1803_next_de_ex_a\;
      \$v234%next\ <= \$v234\;
      \$v249%next\ <= \$v249\;
      \$v237%next\ <= \$v237\;
      \$v241%next\ <= \$v241\;
      \$1811_div_aux165_result%next\ <= \$1811_div_aux165_result\;
      \label214%next\ <= label214;
      \$1797%next\ <= \$1797\;
      \$v242%next\ <= \$v242\;
      \$1840%next\ <= \$1840\;
      \$v258%next\ <= \$v258\;
      \$1810%next\ <= \$1810\;
      \$1786_code%next\ <= \$1786_code\;
      \$v251%next\ <= \$v251\;
      \$1831%next\ <= \$1831\;
      \$1850%next\ <= \$1850\;
      \$1785%next\ <= \$1785\;
      \$1818%next\ <= \$1818\;
      \$v248%next\ <= \$v248\;
      \$1790%next\ <= \$1790\;
      \$1841%next\ <= \$1841\;
      \$1782%next\ <= \$1782\;
      \$1779%next\ <= \$1779\;
      \$1825%next\ <= \$1825\;
      \$v246%next\ <= \$v246\;
      \$v238%next\ <= \$v238\;
      \$v243%next\ <= \$v243\;
      \$1812%next\ <= \$1812\;
      \$v224%next\ <= \$v224\;
      \$1794%next\ <= \$1794\;
      \$1842%next\ <= \$1842\;
      
      
      result <= result216;
      end process;
  end architecture;
