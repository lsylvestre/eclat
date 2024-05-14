open Fsm_syntax
open Fsm_comp


let globalize_flag = ref true


module D = Display_internal_steps

let print_elaborated_code_flag = ref true


let compile ?(vhdl_comment="") ?(prop_fsm=false) arg_list name ty fmt pi =

  let pi = Middle_end.compile ~globalize:!globalize_flag arg_list pi in
  
  D.display_pi D.MiddleEnd pi;

  let pi = Rename_main_arg.rename_main_arg_pi pi in

  let (rdy,result,compute,fsm) as design = Fsm_comp.compile pi in

  let statics = pi.statics |> List.filter (function 
    | (_,Ast.Static_array_of _) -> true 
    | (_,Ast.Static_array _) -> true 
    | (_,Ast.Static_matrix _) -> true 
    | _ -> false)
        |>
    List.map (function 
              | x,Ast.Static_array_of (ty,_) ->
                  x,Fsm_syntax.Static_array_of (Fsm_typing.translate_ty ty)
              | x,Ast.Static_array(c,n) ->
                  x,Fsm_syntax.Static_array(Fsm_comp.to_c c,n)
              | x,Ast.Static_matrix(c,n_list) -> 
                  x,Fsm_syntax.Static_matrix(Fsm_comp.to_c c,n_list)
              | _ -> assert false (* already expanded *)
           ) in

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
