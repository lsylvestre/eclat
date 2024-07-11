
library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package runtime is
  alias value is std_logic_vector;
  constant eclat_true : value(0 to 0) := "1";
  constant eclat_false : value(0 to 0)  := "0";
  constant eclat_unit : value(0 to 0)  := "1";
  procedure eclat_skip (arg : in value);
  function eclat_abs   (arg: value)  return value;
  function eclat_add   (arg: value)  return value;
  function eclat_sub   (arg: value)  return value;
  function eclat_neg   (arg: value)  return value;
  function eclat_mult  (arg: value)  return value;
  function eclat_div   (arg: value)  return value;
  function eclat_mod   (arg: value)  return value;
  function eclat_eq    (arg: value)  return value;
  function eclat_neq   (arg: value)  return value;
  function eclat_lt    (arg: value)  return value;
  function eclat_gt    (arg: value)  return value;
  function eclat_le    (arg: value)  return value;
  function eclat_ge    (arg: value)  return value;
  function eclat_if    (arg : value) return value;
  function eclat_and   (arg : value) return value;
  function eclat_or    (arg : value) return value;
  function eclat_xor   (arg : value) return value;
  function eclat_not   (arg : value) return value;
  function eclat_id    (arg : value) return value;
  function eclat_lor   (arg : value) return value;
  function eclat_land  (arg : value) return value;
  function eclat_lxor  (arg : value) return value;
  function eclat_lsl   (arg: value) return value;
  function eclat_lsr   (arg: value) return value;
  function eclat_asr   (arg: value) return value;
  function eclat_getBit(arg: value) return value;
  function eclat_updateBit(arg: value) return value;
  function eclat_size_create(arg: value) return value;
  function eclat_vector_make (size:natural;arg: value) return value;
  function eclat_vector_get (arg: value;constant k:integer) return value;
  function eclat_vector_update (arg: value;constant k:integer) return value;
  function eclat_tuple_get(arg: value; k:integer) return value;
  function eclat_tuple_update(arg: value; k:integer) return value;
  function eclat_size_val(arg: value) return value;
  function of_string   (s: string)   return value; 
  function to_string   (a: std_logic_vector) return string;

  function integer_of_value (arg: value) return integer; 
  function eclat_compute_address (caml_heap_base:value;a:value) return value;
  
  function eclat_string_length (arg:value) return value;
  function eclat_resize (arg:value; k:integer) return value;
  function eclat_int_of_bvect (arg:value) return value;
  function eclat_bvect_of_int (arg:value) return value;
  procedure default_zero (xvar: out value);
  procedure eclat_print (arg:value);
  procedure eclat_print_string (arg:value);
  procedure eclat_print_int (arg:value);
  procedure eclat_print_newline (arg:value);

  procedure acquire(variable lock : inout value(0 to 0));
  procedure release(variable lock : inout value(0 to 0));
end package;


package body runtime is

  procedure eclat_skip (arg : in value) is
    begin
    end procedure;

  procedure echo (arg : in string) is
    begin
      std.textio.write(std.textio.output, arg);
    end procedure echo;

  function of_string(s: string) return value is 
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
    end function; 

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

  function eclat_abs (arg: value) return value is
    variable r : signed (0 to arg'length-1);
    begin
      r := abs(signed(arg)); 
      return value(r);
    end;

  function eclat_add (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := signed(arg(0 to length-1)) + signed(arg(length to arg'length - 1));
      return value(r);
    end;

  function eclat_sub (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := signed(arg(0 to length-1)) - signed(arg(length to arg'length - 1)); 
      return value(r);
    end;

  function eclat_neg (arg: value) return value is
    variable r : signed (0 to arg'length-1);
    begin
      r := 0 - signed(arg); 
      return value(r);
    end;

  function eclat_mult (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := resize(signed(arg(0 to length-1)) * signed(arg(length to arg'length - 1)),length);
      return value(r);
    end;

  function eclat_div (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := resize(signed(arg(0 to length-1)) / signed(arg(length to arg'length - 1)),length);
      return value(r);
    end;

  function eclat_mod (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := resize(signed(arg(0 to length-1)) mod signed(arg(length to arg'length - 1)),length);
      return value(r);
    end;


  function eclat_eq (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) = signed(arg(length to arg'length - 1)) then
        r := "1";
      else
        r := "0";
      end if;
      return r;
    end;

    function eclat_neq (arg: value) return value is
      constant length: natural := arg'length / 2;
      variable r : value (0 to 0);
      begin
        if signed(arg(0 to length-1)) = signed(arg(length to arg'length - 1)) then
          r := "0";
        else
          r := "1";
        end if;
        return r;
      end;

  function eclat_lt (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) < signed(arg(length to arg'length - 1)) then
        r := "1";
      else
        r := "0";
      end if;
      return r;
    end;

  function eclat_gt (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) > signed(arg(length to arg'length - 1)) then
        r := "1";
      else
        r := "0";
      end if;
      return r;
    end;

  function eclat_le (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) <= signed(arg(length to arg'length - 1)) then
        r := "1";
      else
        r := "0";
      end if;
      return r;
    end;

  function eclat_ge (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) >= signed(arg(length to arg'length - 1)) then
        r := "1";
      else
        r:= "0";
      end if;
      return r;
    end;

  function eclat_if(arg : value) return value is
    constant length: natural := (arg'length - 1) / 2;
    variable r : value (0 to length-1);
    begin 
      if arg(0) = '1' then
         r := arg(1 to length);
      else 
         r := arg(length + 1 to length * 2);
      end if;
      return r;
    end;

  function eclat_and(arg : value) return value is
    variable r : value (0 to 0);
    begin 
      if (arg(0) = '1' and arg(1) = '1') then
        r := "1";
      else
        r := "0";
      end if;
      return r;
    end;

  function eclat_or(arg : value) return value is
    variable r : value (0 to 0);
    begin 
      if (arg(0) = '1' or arg(1) = '1') then
        r := "1";
      else
        r := "0";
      end if;
      return r;
    end;

  function eclat_xor(arg : value) return value is
    variable r : value (0 to 0);
    begin 
      if (arg(0) = '1' xor arg(1) = '1') then
        r := "1";
      else
        r := "0";
      end if;
      return r;
    end;

  function eclat_not(arg : value) return value is
    variable r : signed (0 to arg'length - 1);
    begin
      for i in 0 to arg'length-1 loop
        r(i) := not arg(i);
      end loop;
      return value(r);
    end;

  function eclat_lor (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      for i in 0 to length-1 loop
        r(i) := arg(i) or arg(i+length);
      end loop;
      return value(r);
    end;

  function eclat_land (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      for i in 0 to length-1 loop
        r(i) := arg(i) and arg(i+length);
      end loop;
      return value(r);
    end;

  function eclat_lxor (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      for i in 0 to length-1 loop
        r(i) := arg(i) xor arg(i+length);
      end loop;
      return value(r);
    end;

  function eclat_lsl (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : unsigned (0 to length - 1);
    begin
      r := unsigned(arg(0 to length-1)) sll integer_of_value(arg(length to arg'length - 1));
      return value(r);
    end;

  function eclat_lsr (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : unsigned (0 to length - 1);
    begin
      r := unsigned(arg(0 to length-1)) srl integer_of_value(arg(length to arg'length - 1));
      return value(r);
    end;

  function eclat_asr (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : unsigned (0 to length - 1);
    constant n : integer range 0 to length - 1 := integer_of_value(arg(length to arg'length - 1));
    variable sign : std_logic;
    begin
      r := unsigned(arg(0 to length-1)) srl n; -- todo: check if is ok
      return value(r);
    end;

  function eclat_getBit (arg: value) return value is
    constant length: natural := arg'length-32;
    variable r : unsigned (0 to length - 1);
    constant n : unsigned (0 to 31) := unsigned(arg(length to arg'length - 1));
    constant n_int: integer := to_integer(resize(n,31));
    begin
      r := unsigned(arg(0 to length-1));
      --echo(integer'image(n_int));
      --echo(" " &LF);
      return value(r(length-1-n_int to length-1-n_int));
    end;

  function eclat_updateBit (arg: value) return value is
    constant length: natural := (arg'length-1)-32;
    variable r : unsigned (0 to length - 1);
    constant n : integer range 0 to 31 := integer_of_value(arg(length to arg'length - 2));
    constant z : natural := arg'length-1;
    begin
      r := unsigned(arg(0 to length-1));
      r(length-1-n) := arg(z);
      return value(r);
    end;

  function eclat_tuple_get(arg: value; k:integer) return value is
    constant length: natural := (arg'length-32);
    constant l: natural := length/k;
    variable i : integer;
    begin
      i := to_integer(unsigned(arg(length to arg'length-1)));
      return arg(l * i to (l * (i +1))-1);
    end;

  function eclat_tuple_update(arg: value; k:integer) return value is
    constant length: natural := (arg'length-32);
    constant l: natural := length/(k+1); -- size of each element in the tuple
    variable i : integer;
    variable r : value (0 to l*k-1);
    begin
      i := to_integer(unsigned(arg(l*k to arg'length-l-1)));
      r := arg(0 to l*k-1);
      r(l * i to l * (i + 1) - 1) := arg(arg'length-l to arg'length - 1);
      return r;
    end;


  function eclat_size_val (arg: value) return value is
    begin
      return value(to_unsigned(arg'length,32));
    end;
  function eclat_size_create (arg: value) return value is
    begin
      return eclat_resize(arg,16);
    end;

  function eclat_vector_make (size:natural;arg: value) return value is
    constant l: natural := arg'length;
    variable r : value(0 to l*size-1);
    begin
      for k in 0 to size-1 loop
        r(l*k to l*(k+1)-1) := arg;
      end loop;
      return value(r);
    end;

  function eclat_vector_get (arg: value;constant k:integer) return value is
    constant length: natural := (arg'length-32);
    constant i : natural := to_integer(unsigned(arg(length to arg'length-1)));
    constant s : natural := k * i;
    variable r : value(0 to k-1);
    begin
      for j in 0 to k - 1 loop
        r(j) := arg(s+j);
      end loop;
      return r;
    end;

  function eclat_vector_update(arg: value; constant k:integer) return value is
    constant length: natural := (arg'length-32);
    constant l: natural := length/(k+1);
    variable i : natural;
    variable r : value (0 to l*k-1);
    begin
      i := to_integer(unsigned(arg(l*k to l*k+31))); 
      r := arg(0 to l*k-1);

      for j in 0 to l-1 loop
        r(l*i+j) := arg(arg'length-l+j);
      end loop;
      return r;
    end;

 -- function eclat_vector_update(arg: value;k:integer) return value is
   -- constant length: natural := (arg'length-16);
 --   variable i : integer;
   -- begin
     -- i := to_integer(unsigned(arg(length to arg'length-1)));
   --   return arg(k * i to (k * (i +1))-1);
    --end;
  --function eclat_size_val (arg: value) return value is
  --  constant length: natural (arg'length);
  --  begin
  --    r := unsigned(length);
  --    return value(r);
  --  end;

  function eclat_id(arg : value) return value is
    begin 
      return arg;
    end;

  function integer_of_value(arg: value) return integer is
    variable r : unsigned (0 to arg'length - 1);
  begin 
     r := unsigned(arg);
     return to_integer(r);
  end function;

  function eclat_compute_address(caml_heap_base:value;a:value) return value is
    begin
      return value(signed(caml_heap_base) + signed(a(0 to 31)) + (signed(a(32 to 63)) * 4));
    end function;

  function eclat_string_length(arg:value) return value is
    begin
      return std_logic_vector(to_signed(arg'length / 8,16));
    end;

  function eclat_resize(arg:value; k:integer) return value is
    begin
       return std_logic_vector(resize(unsigned(arg),k));
    end;

  function eclat_int_of_bvect (arg:value) return value is
    begin
       return arg;
    end;

  function eclat_bvect_of_int (arg:value) return value is
    begin
       return arg;
    end;

  procedure default_zero (xvar: out value) is
  begin
    for i in 0 to xvar'length - 1 loop
      xvar(0) := '0';
    end loop;
  end procedure;

  procedure eclat_print(arg:value) is
    begin
      echo(to_string(arg));
    end procedure;
  
  function char_of_value(arg : value(0 to 7)) return character is
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

  procedure eclat_print_string(arg:value) is
    variable s : string (0 to arg'length / 8);
    begin
      for i in 0 to s'length-2 loop
          s(i) := char_of_value(arg(i*8 to i*8+7));
      end loop;
      echo(s);
    end procedure;

  procedure eclat_print_int(arg:value) is
    begin
      if arg'length <= 63 then
        echo(integer'image(to_integer(signed(arg))));
      else
        echo("<resize>");
        echo(integer'image(to_integer(resize(unsigned(arg),63))));
      end if;
    end procedure;

  procedure eclat_print_newline(arg:value) is
    begin
        echo(" " &LF);
    end procedure;

  procedure acquire(variable lock : inout value(0 to 0)) is
    begin 
      assert lock(0) = '0' report "lock already acquired" severity error;
      lock(0) := '1';
    end procedure;  
  
  procedure release(variable lock : inout value(0 to 0)) is
    begin
      assert lock(0) = '1' report "lock already released" severity error;
      lock(0) := '0';
    end procedure;    
end runtime;
