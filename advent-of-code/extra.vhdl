library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package Char is
  alias t is std_logic_vector;
  function code(arg: t) return t;
  function chr(arg:t) return t;
  impure function print(signal clk:std_logic;arg:t) return t;
end package;

package body Char is 
  function code(arg: t) return t is
      variable res : t(0 to arg'length - 1);
    begin 
      res(0 to arg'length - 1) := arg;
      return arg;  -- identity
    end;
  function chr(arg:t) return t is
      variable res : t(0 to arg'length - 1);
    begin 
      res(0 to arg'length - 1) := arg;
      return arg;  -- identity
    end;
  impure function print(signal clk:std_logic;arg:t)
    return t is
      variable n : integer;
    begin
      if rising_edge(clk) then
        n := to_integer(unsigned(arg));
        work.Util.echo(Character'image(Character'Val(n)));
      end if;
      return "0";
    end;
end Char;

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.all;

package Bytes is
  alias t is std_logic_vector;
  function make(sa,sr : integer; arg: t) return t;
  function len(arg: t) return t;
  function get(sa,sr : integer; arg1,arg2:t) return t;
  impure function print(signal clk:std_logic;arg:t) return t;
  function to_vect(arg: t) return t;
  function from_vect(arg: t) return t;
  function to_hex(arg: t) return t;
end package;
package body Bytes is 
  function make(sa,sr : integer; arg: t) return t is
    variable res: t(0 to sr-1);
    begin
      for i in 0 to sr/8-1 loop
        for k in 0 to 7 loop
          res(i*8+k) := arg(k);
        end loop;
      end loop;
      return res;
    end;
  function len(arg: t) return t is
    begin 
      return t(to_unsigned(arg'length / 8,32));
    end;
  function get(sa,sr : integer; arg1, arg2:t) return t is
      variable i : integer;
      variable by : t(0 to sa - 32 - 1);
      variable res : t(0 to 7);
    begin
      by(0 to sa - 32 - 1) := arg1;
      i := to_integer(unsigned(arg2));
      for k in 0 to 7 loop
        res(k) := by(i * 8 + k);
      end loop;
      return res;
    end;
  impure function print(signal clk:std_logic;arg:t) return t is
      variable by : t(0 to arg'length - 1);
      variable c : t(0 to 7);
      variable u : t(0 to 0);
    begin
      if rising_edge(clk) then
        by(0 to arg'length - 1) := arg;
        for i in 0 to arg'length/8-1 loop
          for k in 0 to 7 loop
            c(k) := by(i*8+k);
          end loop;
          if c /= (0|1|2|3|4|5|6|7 => '0') then
            u := Char.print(clk,c);
          end if;
        end loop;
      end if;
      return "0";
    end;
  function to_vect(arg: t) return t is 
    begin return arg; end;
  function from_vect(arg: t) return t is
    begin return arg; end;
  function to_hex(arg: t) return t is
      variable n : integer;
      variable c : t(0 to 7) := (others => '0');
      variable tmp : t(0 to (arg'length+7)/8*8-1)  := (others => '0');
      variable res : t(0 to arg'length / 8 * 4 - 1);
    begin
      tmp(0 to ((arg'length+7)/8)*8-1) := arg;
      for i in 0 to tmp'length / 8 - 1 loop
        c(0 to 7) := tmp(i*8 to i*8 + 7);
        n := to_integer(unsigned(c));
        case n is
        when 48 => res(i*4 to i*4+3) := "0000"; -- 0
        when 49 => res(i*4 to i*4+3) := "0001"; -- 1
        when 50 => res(i*4 to i*4+3) := "0010"; -- 2
        when 51 => res(i*4 to i*4+3) := "0011"; -- 3
        when 52 => res(i*4 to i*4+3) := "0100"; -- 4
        when 53 => res(i*4 to i*4+3) := "0101"; -- 5
        when 54 => res(i*4 to i*4+3) := "0110"; -- 6
        when 55 => res(i*4 to i*4+3) := "0111"; -- 7
        when 56 => res(i*4 to i*4+3) := "1000"; -- 8
        when 57 => res(i*4 to i*4+3) := "1001"; -- 9
        when 65 => res(i*4 to i*4+3) := "1010"; -- A
        when 66 => res(i*4 to i*4+3) := "1011"; -- B
        when 67 => res(i*4 to i*4+3) := "1100"; -- C
        when 68 => res(i*4 to i*4+3) := "1101"; -- D
        when 69 => res(i*4 to i*4+3) := "1110"; -- E
        when 70 => res(i*4 to i*4+3) := "1111"; -- F
        when others => res(i*4 to i*4+3) := "0000";
        end case;
      end loop;
      return res;
    end;
end Bytes;

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

package IOFile is
  alias t is std_logic_vector;
  function to_string(arg: t) return string;
  impure function read_file(signal clk:std_logic; sa,sr : integer; name: t) return t;
  impure function write_file(signal clk:std_logic; name, content: t) return t;
end package;
package body IOFile is
  
  function to_string(arg: t) return string is
    variable s : string (0 to arg'length / 8);
  begin
    for i in 0 to arg'length/8-1 loop
      s(i) := Character'val(to_integer(unsigned(arg(i*8 to i*8+7))));
    end loop;
    return s;
  end;

  impure function read_file(signal clk:std_logic;sa,sr : integer; name: t) return t is
     type char_file_t is file of character;     
  file infile : char_file_t;
    variable c: character;
    variable i : integer := 0;
    variable res : t(0 to sr - 1) := (others => '0');
  begin        
    file_open(infile, to_string(name), read_mode);
    while not endfile (infile) loop
      read(infile, c);
      res(i*8 to i*8+7) := t(to_unsigned(Character'pos(c),8));
      i := i + 1;
    end loop;
    file_close(infile);
    return res;
  end;

  impure function write_file(signal clk:std_logic; name, content: t) return t is
    type char_file_t is file of character;     
    file outfile : char_file_t;
    begin        
      file_open(outfile, to_string(name), write_mode);
      for i in 0 to content'length / 8 - 1 loop
        write(outfile, Character'val(to_integer(unsigned(content(i*8 to i*8+7)))));
      end loop;
      return "0";
    end;
   
end IOFile;