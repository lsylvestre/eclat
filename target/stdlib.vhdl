library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package util is
  procedure echo (arg : in string);
  function val_of_bool (b:boolean) return std_logic_vector;
end package;

package body util is
  procedure echo (arg : in string) is
  begin
    std.textio.write(std.textio.output, arg);
  end procedure echo;
  function val_of_bool (b:boolean) return std_logic_vector is
    begin
      if b then
        return "1";
      else 
        return "0";
      end if;
    end;
end util;

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
package Assertion is
  function ok (b:std_logic_vector) return std_logic_vector;
end Assertion;
package body Assertion is
  function ok (b:std_logic_vector) return std_logic_vector is
    begin
      assert b = "1" report "assertion failed" severity error;
      return "0";
    end;
end Assertion;

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package Print is
  alias t is std_logic_vector;
  function to_string (a: std_logic_vector) return string;
  impure function print_value (signal clk:std_logic;arg:t) return t;
  impure function print_string (signal clk:std_logic;arg:t) return t;
  impure function print_newline (signal clk:std_logic;arg:t) return t;
end package;
package body Print is
  
  function to_string (a: std_logic_vector) return string is
    variable b : string (1 to a'length) := (others => NUL);
    variable stri : integer := 1; 
    begin
        for i in a'range loop
            b(stri) := std_logic'image(a((i)))(2);
        stri := stri+1;
        end loop;
    return b;
    end function;

  impure function print_value (signal clk:std_logic;arg:t) return t is
    begin
      work.Util.echo(to_string(arg));
      return "0";
    end function;

  impure function print_newline (signal clk:std_logic;arg:t) return t is
    begin
      if rising_edge(clk) then
        work.Util.echo(" " &LF);
      end if;
      return "0";
    end;

  function char_of_value(arg : std_logic_vector(0 to 7)) return character is
  -- from https://groups.google.com/g/comp.lang.vhdl/c/_kI2ZwxVVVk
  constant xmap :integer :=0;
  variable TEMP :integer :=0;
  begin
    for i in arg'range loop
      temp:=temp*2;
      case arg(i) is
        when '0' | 'L' => null;
        when '1' | 'H' => temp :=temp+1;
        when others => temp :=temp+xmap;
      end case;
    end loop;
    return character'val(temp);
  end function;

  impure function print_string (signal clk:std_logic ; arg:t) return t is
    variable s : string (0 to arg'length / 8);
    begin
      if rising_edge(clk) then
        for i in 0 to s'length-2 loop
            s(i) := char_of_value(arg(i*8 to i*8+7));
        end loop;
        work.Util.echo(s);
      end if;
      return "0";
    end;
end Print;

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package bool is
  alias t is std_logic_vector;
  impure function print (signal clk:std_logic;arg: t) return t;
  function lnot (arg: t) return t;
  function lor  (arg1: t;arg2: t) return t;
  function land (arg1: t;arg2: t) return t;
  function lxor (arg1: t;arg2: t) return t;
end package;

package body bool is

  impure function print(signal clk:std_logic;arg:t) return t is
    begin
      if rising_edge(clk) then
        if arg(0) = '1' then
          work.Util.echo("true");
        else
          work.Util.echo("false");
        end if;
      end if;
      return "0";
    end function;
  function lnot (arg: t) return t is
    begin
      return work.util.val_of_bool(arg(0) = '0');
    end;
  function lor (arg1: t;arg2: t) return t is
    begin
      return work.util.val_of_bool(arg1(0) = '1' or arg2(0) = '1');
    end;
  function land (arg1: t;arg2: t) return t is
    begin
      return work.util.val_of_bool(arg1(0) = '1' and arg2(0) = '1');
    end;
  function lxor (arg1: t;arg2: t) return t is
    begin
      return work.util.val_of_bool(arg1(0) = '1' xor arg2(0) = '1');
    end;
end bool;
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package int is
  alias t is std_logic_vector;
  impure function print(signal clk:std_logic;arg:t) return t;
  function absv   (arg1: t) return t;
  function add    (arg1: t;arg2: t) return t;
  function sub    (arg1: t;arg2: t) return t;
  function neg    (arg: t) return t;
  function mul    (arg1: t;arg2: t) return t;
  function div    (arg1: t;arg2: t) return t;
  function modulo (arg1: t;arg2: t) return t;
  function eq     (arg1: t;arg2: t) return t;
  function neq    (arg1: t;arg2: t) return t;
  function lt     (arg1: t;arg2: t) return t;
  function le     (arg1: t;arg2: t) return t;
  function gt     (arg1: t;arg2: t) return t;
  function ge     (arg1: t;arg2: t) return t;
  function lsl    (arg1: t;arg2: t) return t;
  function lsr    (arg1: t;arg2: t) return t;
  function asr    (arg1: t;arg2: t) return t;
  function lor    (arg1: t;arg2: t) return t;
  function land   (arg1: t;arg2: t) return t;
  function lxor   (arg1: t;arg2: t) return t;
end package;

package body int is -- signed int

  impure function print(signal clk:std_logic;arg:t) return t is
    begin
      if rising_edge(clk) then
        if arg'length <= 63 then
          work.Util.echo(integer'image(to_integer(signed(arg))));
        else
          work.Util.echo("<resize>");
          work.Util.echo(integer'image(to_integer(resize(unsigned(arg),63))));
        end if;
      end if;
      return "0";
    end function;

  function absv (arg1: t) return t is
    begin
      return t(abs(signed(arg1)));
    end;

  function add (arg1: t;arg2: t) return t is
    variable r : signed (0 to arg1'length - 1);
    begin
      r := signed(arg1) + signed(arg2);
      return t(r);
    end;
  
  function sub (arg1: t;arg2: t) return t is
    variable r : signed (0 to arg1'length - 1);
    begin
      r := signed(arg1) - signed(arg2);
      return t(r);
    end;
  
  function neg (arg: t) return t is
    begin
      return t(0 - signed(arg));
    end;

  function mul (arg1: t;arg2: t) return t is
    variable r : signed (0 to arg1'length - 1);
    begin
      r := signed(resize(signed(arg1) * signed(arg2),arg1'length));
      return t(r);
    end;

  function div (arg1: t;arg2: t) return t is
    variable r : signed (0 to arg1'length - 1);
    begin
      if signed(arg2) = 0 then 
        return arg2;
      else 
        r := signed(arg1) / signed(arg2);
        return t(r);
      end if;
    end;

  function modulo (arg1: t;arg2: t) return t is
    variable r : signed (0 to arg1'length - 1);
    begin
      if signed(arg2) = 0 then 
        return arg2;
      else 
        r := signed(arg1) mod signed(arg2);
        return t(r);
      end if;
    end;

  function eq (arg1: t;arg2: t) return t is
    begin
      return work.util.val_of_bool(signed(arg1) = signed(arg2));
    end;

  function neq (arg1: t;arg2: t) return t is
    begin
      return work.util.val_of_bool(signed(arg1) /= signed(arg2));
    end;

  function lt (arg1: t;arg2: t) return t is
    begin
      return work.util.val_of_bool(signed(arg1) < signed(arg2));
    end;

  function gt (arg1: t;arg2: t) return t is
    begin
      return work.util.val_of_bool(signed(arg1) > signed(arg2));
    end;

  function le (arg1: t;arg2: t) return t is
    begin
      return work.util.val_of_bool(signed(arg1) <= signed(arg2));
    end;

  function ge (arg1: t;arg2: t) return t is
    begin
      return work.util.val_of_bool(signed(arg1) >= signed(arg2));
    end;

  function unsigned_integer_of_value(arg: t) return integer is
    variable r : unsigned (0 to arg'length - 1);
  begin 
     r := unsigned(arg);
     return to_integer(r);
  end function;

  function lsl (arg1: t;arg2: t) return t is
    variable r : signed (0 to arg1'length - 1);
    begin
      r := signed(arg1) sll unsigned_integer_of_value(arg2);
      return t(r);
    end;
  
  function lsr (arg1: t;arg2: t) return t is
    variable r : signed (0 to arg1'length - 1);
    begin
      r := signed(arg1) srl unsigned_integer_of_value(arg2);
      return t(r);
    end;
  
  function asr (arg1: t;arg2: t) return t is
    variable r : unsigned (0 to arg1'length - 1);
    begin
      r := unsigned(arg1) srl unsigned_integer_of_value(arg2);  -- ok ?
      return t(r);
    end;
  
  function lor (arg1: t;arg2: t) return t is
    variable r : signed (0 to arg1'length - 1);
    begin
      for i in 0 to arg1'length - 1 loop
        r(i) := arg1(i) or arg2(i);
      end loop;
      return t(r);
    end;

  function land (arg1: t;arg2: t) return t is
    variable r : signed (0 to arg1'length - 1);
    begin
      for i in 0 to arg1'length - 1 loop
        r(i) := arg1(i) and arg2(i);
      end loop;
      return t(r);
    end;

  function lxor (arg1: t;arg2: t) return t is
    variable r : signed (0 to arg1'length - 1);
    begin
      for i in 0 to arg1'length - 1 loop
        r(i) := arg1(i) xor arg2(i);
      end loop;
      return t(r);
    end;
end int;


-- ------------------------------------------------------------------------------

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package uint is
  alias t is std_logic_vector;
  function of_int (arg: t) return t;
  function to_int (arg: t) return t;
  impure function print(signal clk:std_logic;arg:t) return t;
  function add    (arg1: t;arg2: t) return t;
  function sub    (arg1: t;arg2: t) return t;
  function mul    (arg1: t;arg2: t) return t;
  function div    (arg1: t;arg2: t) return t;
  function modulo (arg1: t;arg2: t) return t;
  function eq     (arg1: t;arg2: t) return t;
  function neq    (arg1: t;arg2: t) return t;
  function lt     (arg1: t;arg2: t) return t;
  function le     (arg1: t;arg2: t) return t;
  function gt     (arg1: t;arg2: t) return t;
  function ge     (arg1: t;arg2: t) return t;
end package;


package body uint is
  impure function print(signal clk:std_logic;arg:t) return t is
    begin
      if rising_edge(clk) then
        if arg'length <= 63 then
          work.Util.echo(integer'image(to_integer(unsigned(arg))));
        else
          work.Util.echo("<resize>");
          work.Util.echo(integer'image(to_integer(resize(unsigned(arg),63))));
        end if;
      end if;
      return "0";
    end function;
  -- signed int
  function of_int (arg: t) return t is
    begin
      return arg;
    end;
  function to_int (arg: t) return t is
    begin
      return arg;
    end;

  function add (arg1: t;arg2: t) return t is
    begin
      return t(unsigned(arg1) + unsigned(arg2));
    end;
  
  function sub (arg1: t;arg2: t) return t is
    begin
      return t(unsigned(arg1) - unsigned(arg2));
    end;

  function mul (arg1: t;arg2: t) return t is
    begin
      return t(unsigned(arg1) * unsigned(arg2));
    end;

  function div (arg1: t;arg2: t) return t is
    begin
      if unsigned(arg2) = 0 then 
        return arg2;
      else 
        return t(unsigned(arg1) / unsigned(arg2));
      end if;
    end;

  function modulo (arg1: t;arg2: t) return t is
    begin
      if unsigned(arg2) = 0 then 
        return arg2;
      else 
        return t(unsigned(arg1) mod unsigned(arg2));
      end if;
    end;

  function val_of_bool (b:boolean) return t is
    begin
      if b then
        return "1";
      else 
        return "0";
      end if;
    end;

  function eq (arg1: t;arg2: t) return t is
    begin
      return val_of_bool(unsigned(arg1) = unsigned(arg2));
    end;

  function neq (arg1: t;arg2: t) return t is
    begin
      return val_of_bool(unsigned(arg1) /= unsigned(arg2));
    end;

  function lt (arg1: t;arg2: t) return t is
    begin
      return val_of_bool(unsigned(arg1) < unsigned(arg2));
    end;

  function gt (arg1: t;arg2: t) return t is
    begin
      return val_of_bool(unsigned(arg1) > unsigned(arg2));
    end;

  function le (arg1: t;arg2: t) return t is
    begin
      return val_of_bool(unsigned(arg1) <= unsigned(arg2));
    end;

  function ge (arg1: t;arg2: t) return t is
    begin
      return val_of_bool(unsigned(arg1) >= unsigned(arg2));
    end;
end uint;



library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
package vect is
  alias t is std_logic_vector;
  function create (sa,sr : integer; arg: t) return t;
  function nth (sa,sr : integer; arg1: t; arg2 : t) return t;
  function copy_with (sa,sr : integer; arg1: t;arg2 : t; arg3 : t) return t;
  function infos (sa,sr : integer; arg: t) return t;
end package;

package body vect is
  function create (sa,sr : integer; arg: t) return t is
    constant nb_elements : integer := sr/sa; 
    variable r : t(0 to sr -1); 
    begin
      for i in 0 to nb_elements - 1 loop
      r(i * sa to (i+1) * sa -1) := arg;
      end loop;
      return r;
    end function;
  function infos (sa,sr : integer; arg: t) return t is
    constant zero : t(0 to sr-16-1) := (others => '0');
    begin
      return t(to_unsigned(sa/(sr-16),16))&zero;
    end function;
  function nth (sa,sr : integer; arg1: t; arg2 : t) return t is
    constant i : natural := to_integer(unsigned(arg2));
    variable r : t(0 to sr-1);
    variable u : t(0 to sa - 16 - 1) := t(arg1);    -- needed because, with GHDL, depending on the caller, arg1 is an array that does not start by 0 
    begin
      for j in 0 to sr - 1 loop
        r(j) := u(sr * i + j);
      end loop;
      return t(r);
    end function;
  function copy_with (sa,sr : integer; arg1: t;arg2 : t; arg3 : t) return t is
    constant size_elem : integer := arg3'length;
    constant i : natural := to_integer(unsigned(arg2));
    variable r : t(0 to arg1'length - 1) := arg1; 
    begin
      r(size_elem * i to size_elem * i + size_elem - 1) := arg3;
      return r;
    end function;
end vect;


library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
package matrix is
  alias t is std_logic_vector;
  function create (sa,sr : integer; arg: t) return t;
  function get_line (sa,sr : integer; arg1: t; arg2 : t) return t;
  function copy_with_line (sa,sr : integer; arg1: t;arg2 : t; arg3 : t) return t;
  function infos (sa,sr : integer; arg: t) return t;
end package;

package body matrix is
  function create (sa,sr : integer; arg: t) return t is
    constant nb_elements : integer := sr/sa; 
    variable r : t(0 to sr -1); 
    begin

      for i in 0 to nb_elements - 1 loop
       r(i * sa to (i+1) * sa -1) := arg;
      end loop;
      return r;
    end function;
  --function infos (sa,sr : integer; arg: t) return t is
  --  constant zero : t(0 to sr-16-16-1) := (others => '0');
   -- begin
     -- return t(to_unsigned(sa-16/(sr-16-16),16))&zero;
    --end function;
  function get_line (sa,sr : integer; arg1: t; arg2 : t) return t is
    constant i : natural := to_integer(unsigned(arg2));
    begin
      return arg1(sr * i to sr * i + sr - 1);
    end function;

  function copy_with_line (sa,sr : integer; arg1: t;arg2 : t; arg3 : t) return t is
    constant size_elem : integer := arg3'length;
    constant i : natural := to_integer(unsigned(arg2));
    variable r : t(0 to arg1'length - 1) := arg1; 
    begin
      r(size_elem * i to size_elem * i + size_elem - 1) := arg3;
      return r;
    end function;

  function infos (sa,sr : integer; arg: t) return t is
    constant zero : t(0 to sr-16-1) := (others => '0');
    begin
      return t(to_unsigned(sa/(sr-16),16))&zero;
    end function;
end matrix;


library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
package FixedPoint is
  alias t is std_logic_vector;
  function create (sa,sr : integer; arg: t) return t;
  function significand (sa,sr : integer; arg: t) return t;
  function exponent (sa,sr : integer; arg: t) return t;
  function add    (arg1: t;arg2: t) return t;
  function sub    (arg1: t;arg2: t) return t;
end package;

package body FixedPoint is
  function create (sa,sr : integer; arg: t) return t is
  variable r : unsigned (0 to sr - 1) := (others => '0');
  begin
    for i in 0 to sa - 1 loop
       r(i) := arg(i);
    end loop;
    return t(r);
  end;
  function significand (sa,sr : integer; arg: t) return t is
  begin
    return t(unsigned(arg(0 to sr - 1)));
  end;
  function exponent (sa,sr : integer; arg: t) return t is
  begin
    return t(unsigned(arg(sa - sr to sa - 1)));
  end;
  function add (arg1: t;arg2: t) return t is
    begin
      return t(unsigned(arg1) + unsigned(arg2));
    end;
  function sub (arg1: t;arg2: t) return t is
    begin
      return t(unsigned(arg1) - unsigned(arg2));
    end;
end FixedPoint;

