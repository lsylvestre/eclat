library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package values is
  alias t is std_logic_vector;
  constant val_true : t(0 to 0) := "1";
  constant val_false : t(0 to 0) := "0";
  constant val_unit : t(0 to 0)  := "1";
  function val_int(arg: t) return t;
  function last_bit(arg:t) return std_logic;
  function size(arg:t) return t;
  function equal(arg1,arg2:t) return t;
end package;

package body values is
  function val_int(arg: t) return t is
    begin return arg; end;
   function last_bit(arg:t) return std_logic is
      begin return arg(arg'length - 1); end;
   function size(arg:t) return t is
      begin 
        return std_logic_vector(to_unsigned(arg'length,32)); 
      end;
   function equal(arg1,arg2:t) return t is
        variable b : boolean := true;
        variable x,y : t(0 to arg1'length-1);
      begin
        assert (arg1'length = arg2'length);
        x(0 to arg1'length-1) := arg1;
        y(0 to arg1'length-1) := arg2;
        for i in 0 to x'length - 1 loop 
          b := b and (x(i) = y(i));
        end loop;
        if b then return "1"; else return "0"; end if;
      end;
end;

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package util is
  alias t is std_logic_vector;
  procedure echo (arg : in string);
  function val_of_bool (b:boolean) return std_logic_vector;
  function to_string (a: std_logic_vector) return string;
  function of_string   (s: string)   return t;
  function to_ascii (a: t) return string;
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

  function of_string(s: string) return t is 
        constant ss: string(1 to s'length) := s; 
        variable answer: std_logic_vector(0 to 8 * s'length - 1); 
        variable p: integer; 
        variable c: integer; 
    begin 
        for i in ss'range loop
            p := 8 * i;
            c := character'pos(ss(i));
            answer(p - 8 to p-1) := std_logic_vector(to_unsigned(c,8)); 
        end loop; 
        return answer;
    end;
  
  function to_ascii (a: t) return string is
    variable tmp : t(0 to ((a'length+7)/8)*8-1) := (others => '0');
    variable b : string (1 to tmp'length/8) := (others => NUL);
    variable n : integer;
    begin
        tmp(0 to a'length-1) := a;
        for i in 0 to tmp'length/8-1 loop
            n := to_integer(unsigned(tmp(i*8 to i*8+7)));
            b(i+1) := Character'val(n);
        end loop;
    return b;
  end function;
end util;

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
package Assertion is
  procedure ok (b: in std_logic_vector);
end Assertion;
package body Assertion is
  procedure ok (b:in std_logic_vector) is
    begin
      assert b = "1" report "assertion failed" severity error;
    end;
end Assertion;

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work; 
use work.util;

package Print is
  alias t is std_logic_vector;
  impure function print_value (signal clk: std_logic ; arg: t) return t;
  impure function print_ascii (signal clk: std_logic ; arg: t) return t;
  impure function print_string (signal clk: std_logic ; arg: t) return t;
  impure function print_newline (signal clk: std_logic ; arg: t) return t;
end package;
package body Print is
  impure function print_value (signal clk: std_logic ; arg: t) return t is
      variable res : t(0 to 0) := "0";
    begin
      if rising_edge(clk) then
        work.Util.echo(work.Util.to_string(arg));
      end if;
      return res;
    end;

  impure function print_ascii (signal clk: std_logic ; arg: t) return t is
      variable res : t(0 to 0) := "0";
    begin
      if rising_edge(clk) then
        work.Util.echo(work.Util.to_ascii(arg));
      end if;
      return res;
    end;

  impure function print_newline (signal clk: std_logic ; arg: t) return t is
      variable res : t(0 to 0) := "0";
    begin
      if rising_edge(clk) then
        work.Util.echo(" " &LF);
      end if;
      return res;
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
  end;

  impure function print_string (signal clk: std_logic ; arg: t) return t is
      variable res : t(0 to 0) := "0";
      variable s : string (0 to arg'length / 8);
    begin
      if rising_edge(clk) then
        for i in 0 to s'length-2 loop
            s(i) := char_of_value(arg(i*8 to i*8+7));
        end loop;
        work.Util.echo(s);
      end if;
      return res;
    end;
end Print;


library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package default is
  alias t is std_logic_vector;
  function create (sa,sr: integer;arg:t) return t;
end package;

package body default is
  function create (sa,sr: integer;arg:t) return t is
  variable r : t(0 to sr - 1) := (others => 'U');
  begin return r; end;
end default;

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package consult is -- for efficiently checking first constructor parameter
  alias t is std_logic_vector;
  function consult_signal (a,b: integer;arg:t) return t;
end package;

package body consult is
  function consult_signal (a,b: integer;arg:t) return t is
  begin return arg(a to b - 1) & arg(0); end;
end consult;

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package bool is
  alias t is std_logic_vector;
  impure function print(signal clk: in std_logic; arg: in t) return t;
  function is_true (arg: t) return boolean;
  function lnot (arg: t) return t;
  function lor  (arg1: t;arg2: t) return t;
  function land (arg1: t;arg2: t) return t;
  function lxor (arg1: t;arg2: t) return t;
end package;

package body bool is

  
  impure function print(signal clk: in std_logic; arg: in t) return t is
      variable res : t(0 to 0) := "0";
    begin
      if rising_edge(clk) then
        if arg(0) = '1' then
          work.Util.echo("true");
        else
          work.Util.echo("false");
        end if;
      end if;
      return res;
    end;

  function is_true (arg: t) return boolean is
    begin
      if arg(0) = '1' then
        return true;
      else
        return false; 
      end if;
    end;
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

package if_then_else is
  alias t is std_logic_vector;
  function mux(arg1,arg2,arg3:t) return t;
end package;

package body if_then_else is
  function mux(arg1,arg2,arg3:t) return t is
    begin
      if arg1(0) = '1' then
        return arg2;
      else
        return arg3;
      end if;
    end;
end if_then_else;

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package int is
  alias t is std_logic_vector;
  impure function print(signal clk: std_logic; arg: t) return t;
  function resize(arg:t; k:integer) return t;
  function resize(sa,sr: integer; arg:t) return t;
  function resize(arg1:t; arg2:t) return t;
  function logical_not (arg1: t) return t;
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
  function get_bit(arg1: t;arg2: t) return t;
  function update_bit(arg1: t;arg2: t; arg3:t) return t;
end package;

package body int is -- signed int

  impure function print(signal clk: std_logic; arg: t) return t is
      variable res : t(0 to 0) := "0";
    begin
      if rising_edge(clk) then
        if arg'length <= 63 then
          work.Util.echo(integer'image(to_integer(signed(arg))));
        else
          work.Util.echo("<resize>");
          work.Util.echo(integer'image(to_integer(resize(unsigned(arg),63))));
        end if;
      end if;
      return res;
    end;

  function resize(arg:t; k:integer) return t is
    begin
       return t(resize(unsigned(arg),k));
    end;
  -- (<'s1> * int<'s2>) => int<'s1> ;;
  function resize(arg1:t; arg2:t) return t is
    begin
       return resize(arg2, to_integer(unsigned(arg1)));
    end;
  function resize(sa,sr:integer; arg:t) return t is
    begin
       return resize(arg, sr);
    end;

  function absv (arg1: t) return t is
    begin
      return t(abs(signed(arg1)));
    end;

  function logical_not (arg1: t) return t is
     variable a,res : t(0 to arg1'length - 1);
    begin
      a(0 to arg1'length - 1) := arg1;
      for i in 0 to arg1'length - 1 loop
        res(i) := not(a(i));
      end loop;
      return res;
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
    variable r : signed (0 to arg1'length - 1);
    begin
      r := shift_right(signed(arg1), unsigned_integer_of_value(arg2));  -- ok ?
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

  function get_bit(arg1: t;arg2: t) return t is
      constant i : natural := to_integer(unsigned(arg2));
      variable a : t(0 to arg1'length - 1);
      variable res : t(0 to 0);
      begin
        a(0 to arg1'length - 1) := arg1;
        if i >= arg1'length then
          res(0 to 0) := "0";
        else
          res(0 to 0) := a(i to i);
        end if;
        return res;
      end;
  function update_bit(arg1: t;arg2: t;arg3: t) return t is
        constant i : natural := to_integer(unsigned(arg2));
        variable res : t(0 to arg1'length - 1); 
      begin
        res(0 to arg1'length - 1) := arg1;
        if i < arg1'length then
          res(i to i) := arg3;
        end if;
        return res;
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
  function create (sa,sr : integer; size: t; arg: t) return t;
  function nth (sa,sr : integer; arg1: t; arg2 : t) return t;
  function copy_with (sa,sr : integer; arg1: t;arg2 : t; arg3 : t) return t;
  function infos (sa,sr : integer; arg: t) return t;
  function head (sa,sr : integer; arg: t) return t;
  function tail (sa,sr : integer; arg: t) return t;
  function cons (arg1,arg2: t) return t;
  function concat (arg1,arg2: t) return t;
  function split (arg: t) return t;
  function of_int (arg: t) return t;
  function to_int (arg: t) return t;
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
    end;
  function create (sa,sr : integer; size: t; arg: t) return t is
    begin return create(sa-to_integer(unsigned(size)),sr,arg);
  end;
  -- knowing argument size and result size is not sufficient to
  -- find the number of element of a vector,
  -- hence this auxiliary function 
  function infos (sa,sr : integer; arg: t) return t is
    constant zero : t(0 to sr-16-1) := (others => '0');
    begin
      return t(to_unsigned(sa/(sr-16),16))&zero;
    end;
  -- `a vect<'N> * int<32> => `a
  function nth (sa,sr : integer; arg1: t; arg2 : t) return t is
    constant i : natural := to_integer(unsigned(arg2));
    constant size_vect : natural := arg1'length / sr;
    constant size_elem : natural := sr;
    variable res : t(0 to size_elem - 1) := (others => '0');
    variable a : t(0 to arg1'length - 1); 
    begin
      if (i+1) * size_elem >= arg1'length then
        return res; -- return 0 in case of index out of bounds
      else
        a(0 to arg1'length - 1) := t(arg1); -- needed because, with GHDL, depending on the caller, arg1 is an array that does not start by 0 
        res := a(i * size_elem to (i+1) * size_elem - 1);
        return res;
      end if;
    end;
  
  -- `a vect<'N> * int<32> * `a => `a vect<'N>
  function copy_with (sa,sr : integer; arg1: t;arg2 : t; arg3 : t) return t is
    constant size_elem : integer := arg3'length;
    constant i : natural := to_integer(unsigned(arg2));
    variable r : t(0 to arg1'length - 1) := arg1; 
    begin
      for j in 0 to sr/size_elem - 1 loop
        if i = j then
           -- note: synthetizer 
           r(size_elem * j to size_elem * j + size_elem - 1) := arg3;
        end if;
      end loop;
      return r;
  end;

  -- `a vect<'N+1> => `a
  function head (sa,sr : integer; arg: t) return t is
    variable res : t(0 to sr - 1);
  begin
    res := arg(0 to sr - 1);
    return res;
  end;
  -- `a vect<'N+1> => `a vect<'N>
  function tail (sa,sr : integer; arg: t) return t is
    variable res : t(0 to sr - 1);
  begin
    res := arg(sa-sr to sa - 1);
    return res;
  end;
  -- (`a * `a vect<'N>) => `a vect<'N+1>
  function cons (arg1,arg2: t) return t is
    variable res : t(0 to arg1'length + arg2'length - 1);
  begin
    res(0 to arg1'length - 1) := arg1;
    res(arg1'length to arg1'length + arg2'length - 1) := arg2;
    return res;
  end;
  function concat (arg1,arg2: t) return t is
  begin
    return arg1 & arg2;
  end;
  function split (arg: t) return t is
  begin
    return arg;
  end;
  function of_int (arg: t) return t is
    begin
      return arg;
    end;
  function to_int (arg: t) return t is
    begin
      return arg;
    end;
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
    constant zero : t(0 to sr-32-1) := (others => '0');
    begin
      return t(to_unsigned(sa/(sr-32),32))&zero;
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




library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
package Lock is
  alias t is std_logic_vector;
  function locked (arg: t) return t;
  procedure acquire (variable arg: out t);
  procedure release (variable arg: out t);
end package;

package body Lock is
  function locked (arg: t) return t is
    begin return arg(0) & ""; end;
  procedure acquire (variable arg: out t) is
    begin arg(0) := '1'; end;
  procedure release (variable arg: out t) is
    begin arg(0) := '0'; end;
end Lock;

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use work.int;
use work.print;


package ram is
  alias t is std_logic_vector;
  type array8 is array (natural range <>) of t(0 to 7);
  procedure start_read(x : inout std_logic_vector;
                       i : in std_logic_vector;
                       sz_val : in integer);
  procedure start_write(x : inout std_logic_vector;
                        i, v : in std_logic_vector;
                        sz_val : in integer);
  procedure end_write(x : out std_logic_vector);
end package;

package body ram is
  procedure start_read(x : inout std_logic_vector;
                       i : in std_logic_vector;
                       sz_val : in integer) is
    begin 
      assert (x(0) = '1');
      x(1) := '0';
      x(2 to x'length-2*sz_val-1) := int.resize(i,x'length-2*sz_val-2);
    end;
  procedure start_write(x : inout std_logic_vector;
                        i, v : in std_logic_vector;
                        sz_val : in integer) is
    begin
      assert (x(0) = '1');
      x(1) := '1';
      -- report "The size is " & integer'image(x'length-2*sz_val-2);
      x(2 to x'length-2*sz_val-1) := int.resize(i,x'length-2*sz_val-2);
      x(x'length-v'length-v'length to x'length-v'length-1) := v;
    end;
  procedure end_write(x : out std_logic_vector) is
    begin
      x(1) := '0';
  end;
end ram;

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
    while (not endfile (infile)) and i*8+7 < res'length loop
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