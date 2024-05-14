open Ast
open Ast_subst

let insert_bound_checking_flag = ref true

let stop = "fatal_error$"

let check_bound e x eidx len =
  let x = gensym () in
  E_letIn(P_var x,eidx,
  E_if(E_app(E_const(Op(Runtime(And))),
            E_tuple[E_app(E_const(Op(Runtime(Ge))),E_tuple[E_var x;E_const(Int(0,Types.unknown()))]);
                    E_app(E_const(Op(Runtime(Lt))),E_tuple[E_var x;len])]),
            e,(E_app(E_var stop,E_const Unit))))

let bound_mod cc e x eidx len =
  E_app(cc,E_tuple[E_var x;E_app(E_const(Op(Runtime(Mod))),E_tuple[eidx;len])])

let rec insert e =
  match e with
  | E_array_get(x,eidx) ->
      check_bound e x eidx (E_array_length x)
  | E_array_set(x,eidx,_) ->
      check_bound e x eidx (E_array_length x)
  | E_app((E_const(Op(Runtime(Vector_get t))) as c),ee) ->
       let x = gensym () in
       let idx = gensym () in
       E_letIn(P_tuple[P_var x;P_var idx],ee,
       bound_mod c e x (E_var idx) (E_app(E_const(Op(Runtime(Operators.Vector_length (Types.unknown(),Types.unknown())))),E_var x)))
  | E_app(E_const(Op(Runtime(Vector_update t))),E_tuple[E_var x;eidx;_]) ->
       e (*
       check_bound e x eidx (E_app(E_const(Op(Runtime(Operators.Vector_length (Types.unknown(),Types.unknown())))),E_var x))
 *) | e -> Ast_mapper.map insert e

let declare_toplevel_error e =
  E_letIn(P_var stop,E_fix(stop,(P_unit, 
                        E_letIn(P_unit, 
                          (if !Operators.flag_no_print then E_const Unit 
                           else E_app(E_const(Op(Runtime(Print_string))),E_const(String("index out of bounds")))),
                          E_app(E_var stop,E_const Unit)))),e) 

let insert_pi pi =
  if !insert_bound_checking_flag then pi else
  let main = insert pi.main in
  let main = declare_toplevel_error main in
  { pi with main }
