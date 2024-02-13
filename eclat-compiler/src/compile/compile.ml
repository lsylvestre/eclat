open Fsm_syntax
open Fsm_comp

module D = Display_internal_steps

let print_elaborated_code_flag = ref true


let compile ?(vhdl_comment="") ?(prop_fsm=false) arg_list name ty fmt pi =

  let pi = Middle_end.compile arg_list pi in
  D.display_pi D.MiddleEnd pi;

  let pi = Rename_main_arg.rename_main_arg_pi pi in

  let (rdy,result,compute,fsm) as design = Fsm_comp.compile pi in

  let statics = List.map (function x,Ast.Static_array(c,n) -> x,Fsm_syntax.Static_array(Fsm_comp.to_c c,n)) pi.statics in

  Display_target.(display Fsm fsm);

  let fsm = Flat_let_atom.flat_let_atom fsm in
  Display_target.(display Flat fsm);

  let fsm = if prop_fsm then Target_propagation.propagation_fsm fsm else fsm in

  let typing_env = Fsm_typing.typing_circuit ~statics ty (rdy,result,fsm) in

  let fsm = List_machines.list_machines fsm in

  let name = "main" in
  let state_var = "state" in
  let argument = "argument" in

  let fsm = Remove_assigned_but_not_read_vars.clean_fsm ~rdy ~result fsm typing_env in

  let (argument,result) = Gen_vhdl.pp_component fmt ~vhdl_comment ~name ~state_var ~argument ~result ~compute ~rdy ~statics typing_env (let infos = SMap.empty in infos) fsm in
  (argument,result,typing_env)
