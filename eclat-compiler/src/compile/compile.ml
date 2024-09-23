open Fsm_syntax
open Fsm_comp

let globalize_flag = ref true


module D = Display_internal_steps

let print_elaborated_code_flag = ref true


let compile ?(vhdl_comment="") ?(prop_fsm=true) arg_list name ty fmt pi =

  let pi = Middle_end.compile ~globalize:!globalize_flag arg_list ty pi in
  
  D.display_pi D.MiddleEnd pi;

  let pi = Rename_main_arg.rename_main_arg_pi pi in

  let (rdy,result,idle,fsm) = Fsm_comp.compile pi in

  Count_externals.count_externals_fsm fsm;

  let statics = pi.statics |> List.filter (function 
    | (_,Ast.Static_array_of _) -> true 
    | (_,Ast.Static_array _) -> true 
    | _ -> false)
        |>
    List.map (function 
              | x,Ast.Static_array_of (ty,_) ->
                  x,Fsm_syntax.Static_array_of (Fsm_typing.translate_ty ty)
              | x,Ast.Static_array(c,n) ->
                  x,Fsm_syntax.Static_array(Fsm_comp.to_c ~sums:pi.sums c,n)
              | _ -> assert false (* already expanded *)
           ) in

  Display_target.(display Fsm fsm);

  let fsm = if prop_fsm then Target_propagation.propagation_fsm fsm else fsm in

  let fsm = Flat_let_atom.flat_let_atom fsm in
  Display_target.(display Flat fsm);

  let _typing_env = Fsm_typing.typing_circuit ~externals:pi.externals ~statics ty (rdy,result,fsm) in

  let fsm = List_machines.list_machines fsm in

  let state_var = "state" in
  let argument = "argument" in

  (* let fsm = Remove_assigned_but_not_read_vars.clean_fsm ~rdy ~result fsm _typing_env in *)
  let typing_env = Fsm_typing.typing_circuit ~externals:pi.externals ~statics ty (rdy,result,fsm) in

  let gen = if !Flag_mealy.mealy_flag then  Gen_vhdl.pp_component 
            else Gen_vhdl_moore.pp_component in
  let (argument,result) = 
      gen fmt ~vhdl_comment ~name ~externals:pi.externals ~state_var
                                ~argument ~result ~idle ~rdy ~statics 
                                typing_env (let infos = SMap.empty in infos) fsm
  in
  (argument,result,typing_env)
