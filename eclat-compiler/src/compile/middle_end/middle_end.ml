open Ast

open Display_internal_steps

(* normalization: needed before code generation *)
let normalize (pi:pi) : pi =
  (** ensure that each long-running sub-expression is bound to a [let] *)
  let pi = Anf.anf_pi pi in
  display_pi Anf pi;
  (** ensure each function called within an [exec] or [( || )] construct
      is defined locally in this construct *)
  let pi = Move_down_gfun_under_exec_and_par.move_down_gfun_under_exec_and_par_pi pi in
  (* assign a fresh label to each [reg] and [exec] construct *)
  let pi = Instantiate.instantiate_pi pi in
  
  let pi = Monomorphize.monomorphize pi in

  (** renaming all bindings in the source program *)
  let pi = Ast_rename.rename_pi pi in
  (** enforce each recursive function [fix f (fun p -> e)] is bound to the name [f]
      (i.e., [let f = fix f (fun p -> e)) in ...] *)
  let pi = Rename_fix.rename_pi pi in
  (* ensures the program is of the form [fun x -> e] (where the value
     of expression [e] denotes the ouput of the program for the given input [x]) *)
  let pi = Fun_shape_entry_point.fun_shape_entry_point pi in

  pi



let compile ?globalize
            ?(propagation=true)
            arg_list
            (pi:pi) : pi =

  let pi = Ast_rename.rename_pi pi in
  (* todo: should works without renaming,
     but failed on a large example (the OCaml VM written in Eclat)
   *)

  display_pi Front pi;

  (** ensure that each function is defined by a let-binding *)
  let pi = Fun_assign_name.name_pi pi in

  let pi = Specialize.specialize_pi pi in
  display_pi Specialize pi;

  (** make explicit all lexical environments *)
  let pi = Lambda_lifting.lambda_lifting_pi ?globalize pi in
   (* display_pi Lambda_lifting pi;*)
(* let pi = Ast_rename.rename_pi pi in *)
display_pi Lambda_lifting pi;

  let pi = Macro.inl_pi pi in (* macro expansion *)

  (** inline non-recursive functions *)
  let pi = Inline.inl_pi pi in
  let pi = Propagation.propagation_pi pi in
  display_pi Inline pi;
  let pi = Specialize_ref.specialize_ref pi in
  (*  let pi = Let_floating.let_floating_pi pi in
    let pi = Propagation.propagation_pi pi in*)
  display_pi Specialize_ref pi;
  (** compile pattern matching *)
  let pi = Matching.matching_pi pi in
   display_pi Matching pi;

  (** normalization *)
  let pi = normalize pi in

  (** optimization *)
  let pi = if propagation then Propagation.propagation_pi pi else pi in
  display_pi Propagation pi;

  (** ensure that transformations preserve typing *)
  let _ = Typing.typing_with_argument pi arg_list in
  pi
