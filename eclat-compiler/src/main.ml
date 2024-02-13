(** A type-system, an interpreter and a hardware compiler
   for a small language mixing general-purpose computation
   and instantaneous interaction
 *)

(* input files *)
let inputs : string list ref = ref []

let target = ref "../target"

(* option configuration *)

let show_ty_and_exit_flag = ref false
let show_ast_and_exit_flag = ref false
let interp_flag = ref false
let top_flag = ref false
let relax_flag = ref false
let simul_flag = ref true

let prop_fsm_flag = ref false

let arguments = ref ""
let top_wrapper = ref ""
let clock_top = ref "clk"

let clock_top_intel_max10 = "MAX10_CLK1_50"

let top_wrapper_intel_max10 =
  "SW:10,KEY:2|LEDR:10,HEX0:8,HEX1:8,HEX2:8,HEX3:8,HEX4:8,HEX5:8"

(* main configuration *)
let () =
  let add_input (f:string) : unit =
    inputs := !inputs @ [f]
  in
  Arg.parse [
    ("-main",    Arg.Set_string Ast_mk.main_symbol,
                 "entry point (a function name)");

    ("-toploop", Arg.Set top_flag,
                 "Interaction loop");

    ("-int",     Arg.Int Fix_int_lit_size.set_size,
                 "force litteral integers to be of the given size");

    ("-interp",   Arg.Set interp_flag,
                 "interprete and exit.");

    ("-arg",      Arg.Set_string arguments,
                  "specify a list of inputs (one at each clock tick)\
                  \ for interpretation of the source program or simulation\
                  \ of the generated VHDL code");
    ("-ast",      Arg.Set show_ast_and_exit_flag,
                 "print input program and exit.");

    ("-ty",       Arg.Set show_ty_and_exit_flag,
                  "type and exit.");

    ("-pp",      Arg.String Display_internal_steps.set_print_mode,
                 "display the output of the specified (intermediates)\
                 \ compilation pass specified.\n\tPossible values:\
                 \ [front;float;lift;spec;inl;prop;match;anf;middle-end].");

    ("-pp-fsm", Arg.String Display_target.set_print_mode,
                 "display the output of the specified (low-level)\
                 \ compilation pass.\n\tPossible values: [fsm;flat].");

    ("-prop-fsm",Arg.Set prop_fsm_flag,
                 "constant/copy progragation on the generated code");

    ("-hexa",    Arg.Set Ast_pprint.hexa_int_pp_flag,
                 "printer using hexadecimal");

    ("-relax",   Arg.Set relax_flag,
                 "allow the main function to be non-instantaneous (such program is no longer reactive!)");

    ("-noassert",   Arg.Set Operators.flag_no_assert, "remove assertion after typing");
    ("-noprint",   Arg.Set Operators.flag_no_print, "remove printing primitives after typing");

    ("-bus",     Arg.Set_int Interp.flag_bus_proba,
                "[for -interp mode only] set the probability to wait a new\
                 \ clock tick during a bus transation");

    ("-top",     Arg.Set_string top_wrapper,
                "generate a top wrapper for the whole architecture");

    ("-clk-top", Arg.Set_string clock_top,
                "name of the top wrapper global clock");

    ("-intel-max10", Arg.Unit (fun () ->
                                 Operators.flag_no_assert := true;
                                 Operators.flag_no_print := true;
                                 Gen_vhdl.ram_inference := true;
                                 Gen_vhdl.intel_max10_target := true;
                                 clock_top := clock_top_intel_max10;
                                 top_wrapper := top_wrapper_intel_max10),
     "synthesis for Intel MAX 10 FPGA");

    ("-yosys-ecp5", Arg.Unit (fun () ->
                                 Operators.flag_no_assert := true;
                                 Operators.flag_no_print := true;
                                 Gen_vhdl.ram_inference := true;
                                 clock_top := "clk48"),
     "synthesis for Intel MAX 10 FPGA");

    ("-i", Arg.Set Typing.flag_print_signature,
     "Print inferred interface");
    ("-no-prop-linear", Arg.Clear Propagation.flag_propagate_combinational_linear,
                 "do not propagate linear combinational expression.");
    ]
      add_input "Usage:\n  ./mixc file"
;;

let main () : unit =
  (** Lexing/parsing of source code *)
  let (pi,arg_list) =
    Frontend.frontend ~inputs:!inputs !top_flag
                      ~when_repl:Typing.when_repl
                      ~relax:!relax_flag
                      !Ast_mk.main_symbol
                      !arguments
  in

  (** Pretty print *)
  if !show_ast_and_exit_flag then begin
    Format.(fprintf std_formatter "@[<v>%a@,@]" Ast_pprint.pp_pi pi);
    exit 0
  end;

  (** Typing *)
  let (ty,response_time) = Typing.typing_with_argument pi arg_list in

  (** Type only, when [show_ty_and_exit_flag] is setted. *)
  if !show_ty_and_exit_flag then begin
    let open Ast_pprint in
    Format.fprintf Format.std_formatter "@,%a\n" pp_ty ty;
    exit 0;
  end;

  let pi = if Operators.(!Operators.flag_no_assert || !Operators.flag_no_print)
           then Clean_simul.clean_pi
                  ~no_assert:!Operators.flag_no_assert
                  ~no_print:!Operators.flag_no_print pi
           else pi in

  (** remove all decorations (locations) in the source program *)
  let (pi, arg_list) =
    let open Ast_undecorated in
    (remove_deco_pi pi, List.map remove_deco arg_list)
  in

  (** Interprete only, when [interp_flag] is setted.  *)
  if !interp_flag then begin
      Interp.interp_pi pi arg_list |> ignore;
      exit 0
  end;

  let vhdl_comment = "-- code generated from the following source code:\n--   " ^ 
                   String.concat "\n--   " !inputs ^
                   "\n--\n-- with the following command:\n--\n--    " ^ 
                   (String.concat " " @@ List.of_seq @@ Array.to_seq Sys.argv) ^ "\n" in

  (** standard compilation mode *)

  let name = "main" in
  let vhdl_name = !target^"/"^name^".vhdl" in
  let oc_vhdl = open_out vhdl_name in
  let oc_tb = open_out (!target^"/tb_"^name^".vhdl") in
  let fmt_vhdl = Format.formatter_of_out_channel oc_vhdl in
  let fmt_tb = Format.formatter_of_out_channel oc_tb in
  let (argument,result,typing_env) = Compile.compile ~vhdl_comment ~prop_fsm:!prop_fsm_flag arg_list name ty fmt_vhdl pi in
  let args = List.map (Fsm_comp.to_a ~sums:pi.sums) arg_list in

  Gen_testbench.gen_testbench fmt_tb ~vhdl_comment typing_env name ty (argument,result) args;

  Format.fprintf Format.std_formatter
      "\nvhdl code generated in %s/main.vhdl\
      \ \ntestbench generated in %s/tb_main.vhdl for software RTL simulation using GHDL.\n" !target !target;

  close_out oc_vhdl;
  close_out oc_tb ;


  if !top_wrapper <> "" then (
    Gen_top.gen_wrapper ~vhdl_comment
                        ~argument
                        ~result
                        ~clock:!clock_top
                        ~dst:(!target^"/top.vhdl") !top_wrapper
)

;;

(* enty point of the tool *)

let () =
  try main () with Prelude.Errors.Caml_error -> exit 1;;
