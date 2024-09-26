(** generate a top module for synthesis
    of the program as a whole architecture

    I/Os of this module top have to be given as a string
    with the following format :

    ios := "<inputs>|<outputs>"
    inputs,outputs := io, ... io
    io := <name>:<size>

    For instance, string "a:1,b:2|o:8,c:1"
    corresonds to the VHDL interface :

      entity top is
        port (signal clk : in  std_logic;
              signal a : in std_logic;
              signal b : in std_logic_vector(0 to 1);
              signal o : out std_logic_vector(0 to 7);
              signal c : out std_logic
        );
      end entity;

    The name of the global clock ([clk] in the example above)
    can be customized.

*)

open Format

let split c l = List.map String.trim @@ String.split_on_char c l ;;

exception BadFormat

let parse (s:string) : (((string * int) list) * ((string * int) list) * ((string * int) list)) =
  try
    (match split '|' s with
    | [inputs;outputs] ->
        let xs = split ',' inputs
        and ys = split ',' outputs in
        let parse_io x =
          match split ':' x with
          | [x;size] -> (x,int_of_string size)
          | _ -> raise BadFormat in
        let parse_ios xs =
          if xs = [] then (raise BadFormat) else
          List.map parse_io xs
        in
        (parse_ios xs, parse_ios ys, [])
    | [inputs;outputs;inouts] ->
        let xs = split ',' inputs
        and ys = split ',' outputs
        and zs = split ',' inouts in
        let parse_io x =
          match split ':' x with
          | [x;size] -> (x,int_of_string size)
          | _ -> raise BadFormat in
        let parse_ios xs =
          if xs = [] then (raise BadFormat) else
          List.map parse_io xs
        in
        (parse_ios xs, parse_ios ys, parse_ios zs)
    | _ -> raise BadFormat)
  with
  | BadFormat ->
      Prelude.Errors.error (fun fmt ->
        fprintf fmt "bad format for top module I/Os.")




let pp_type name fmt size =
  if size = 1 then fprintf fmt "std_logic" (* by convention, I/Os of size one are std_logic *)
  else if name = "GSENSOR_INT" then fprintf fmt "std_logic_vector(2 downto 1)"  (* TODO: avoid this case (platform dependent) *)
  else fprintf fmt "std_logic_vector(0 to %d)" (size - 1)

let list_take_n l n =
  let rec loop l n acc =
    match l, n with
    | [], _ | _ , 0 -> List.rev acc
    | v :: ll, n -> loop ll (n-1) (v::acc)
  in loop l n []

let pp_top ?(top="top") ~vhdl_comment ~argument:(_,ta) ~result:(_,tr) ?(clock="clk") fmt (inputs,outputs,inouts) =

  let size_list xs =
    List.fold_left (+) 0 @@ List.map snd xs in

  let inouts_size = size_list inouts in
  let argument_size = size_list inputs + inouts_size in
  let result_size = size_list outputs + inouts_size in

  if match ta with
     | None -> false
     | Some t -> argument_size <> Fsm_typing.size_ty t
  then
    Prelude.Errors.error (fun fmt ->
        fprintf fmt "bad format for top module I/Os -- expect an argument of size %d" argument_size);

  if match tr with
     | None -> false
     | Some t -> result_size <> Fsm_typing.size_ty t
  then
    Prelude.Errors.error (fun fmt ->
        fprintf fmt "bad format for top module I/Os -- expect a result of size %d" result_size);
  
  let open Format in
  fprintf fmt "@[<v>%s@]" vhdl_comment;
  fprintf fmt "@[<v>library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.runtime.all;

@[<v 2>entity %s is@,@[<v>port (@[<v>signal %s : in std_logic" top clock;
  List.iter (fun (xi,size) ->
    fprintf fmt ";@,signal %s : in %a" xi (pp_type xi) size) inputs;

  List.iter (fun (xi,size) ->
    fprintf fmt ";@,signal %s : out %a" xi (pp_type xi) size) outputs;  
    
  List.iter (fun (xi,size) ->
    fprintf fmt ";@,signal %s : inout %a" xi (pp_type xi) size) inouts;  
  fprintf fmt "@]@]@,);@]
end entity;

architecture rtl of %s is

    component main is
        port (signal clk : in std_logic;
             -- signal run : in std_logic;
              signal reset : in std_logic;
             -- signal rdy : out value(0 to 0);
              signal argument : in value(0 to %d);
              signal result : out value(0 to %d)
        );
    end component;" top (argument_size-1) (result_size-1);

  fprintf fmt "
    signal RST : std_logic := '1';
    signal argument : value(0 to %d);
    signal result : value(0 to %d);
    signal ready : value (0 to 0);" (argument_size-1) (result_size-1);

  fprintf fmt "
    begin
        process (%s)
            begin
            if (rising_edge(%s)) then
                if RST = '1' then
                    RST <= '0';
                end if;
            end if;
        end process;@," clock clock;

    fprintf fmt "argument <= %s;@," (let l = inputs @ inouts in match l with
                                     | [(x,1)] -> "\"\" & "^x
                                     | _ -> String.concat " & " @@ List.map fst l);

  fprintf fmt "main_CC : component main
        port map (clk => %s,
                  reset => RST,
                  --rdy => ready,
                  argument => argument,
                  result => result
                  );@," clock;

    let rec loop pos = function
    | [] -> ()
    | (xo,n)::bs ->
       (if n = 1 then
         fprintf fmt "%s <= result(%d);@," xo pos
        else fprintf fmt "%s <= result(%d to %d);@," xo pos (pos+n-1)
       );
       loop (pos+n) bs
    in loop 0 (outputs @ inouts);

  fprintf fmt "
end architecture;
@]@."




let gen_wrapper ?top ~name ~vhdl_comment ~argument ~result ~clock ~dst s =
  let oc = open_out dst in
  let fmt = Format.formatter_of_out_channel oc in
  pp_top ?top ~vhdl_comment ~argument ~result ~clock fmt (parse s);
  close_out oc

