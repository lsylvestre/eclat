CAMLC=ocamlc
CAMLLEX=ocamllex
CAMLDEP=ocamldep

EXE=eclat

CAML_FLAGS=

SRC_DIR=src

INCLUDES=-I $(SRC_DIR) -I $(SRC_DIR)/syntax \
         -I $(SRC_DIR)/frontend \
         -I $(SRC_DIR)/typing \
         -I $(SRC_DIR)/interp \
         -I $(SRC_DIR)/compile \
         -I $(SRC_DIR)/compile/middle_end \
         -I $(SRC_DIR)/compile/middle_end/optim \
         -I $(SRC_DIR)/compile/middle_end/transform \
         -I $(SRC_DIR)/compile/middle_end/normalize \
         -I $(SRC_DIR)/compile/inspect \
         -I $(SRC_DIR)/compile/backend \
         -I $(SRC_DIR)/compile/backend/optim \
         -I $(SRC_DIR)/compile/backend/target \
         -I $(SRC_DIR)/compile/gen_top \
         -I $(SRC_DIR)/compile/gen_testbench

MENHIR=menhir --unused-tokens --unused-precedence-levels --infer --ocamlc "$(CAMLC) -i $(INCLUDES)"

OBJS= $(SRC_DIR)/prelude.cmo \
      \
      $(SRC_DIR)/frontend/current_filename.cmo \
      \
      $(SRC_DIR)/typing/types.cmo \
      $(SRC_DIR)/operators.cmo \
      $(SRC_DIR)/syntax/ast.cmo \
      $(SRC_DIR)/syntax/pattern.cmo \
      $(SRC_DIR)/syntax/map_pi.cmo \
      $(SRC_DIR)/syntax/ast_mapper.cmo \
      $(SRC_DIR)/syntax/ast_pprint.cmo \
      $(SRC_DIR)/syntax/ast_subst.cmo \
      $(SRC_DIR)/syntax/ast_rename.cmo \
      $(SRC_DIR)/syntax/free_vars.cmo \
      $(SRC_DIR)/syntax/rename_fix.cmo \
      $(SRC_DIR)/syntax/ast_undecorated.cmo \
      \
      $(SRC_DIR)/frontend/fix_int_lit_size.cmo \
      \
      $(SRC_DIR)/compile/combinational.cmo \
      $(SRC_DIR)/typing/typing.cmo \
      $(SRC_DIR)/syntax/ast_mk.cmo \
      $(SRC_DIR)/compile/middle_end/normalize/fun_assign_name.cmo \
      \
      $(SRC_DIR)/compile/middle_end/optim/propagation.cmo \
      \
      $(SRC_DIR)/compile/middle_end/normalize/anf.cmo \
      $(SRC_DIR)/compile/middle_end/normalize/anf2.cmo \
      \
      $(SRC_DIR)/compile/middle_end/transform/let_floating.cmo \
      $(SRC_DIR)/compile/middle_end/transform/globalize_ref.cmo \
      $(SRC_DIR)/compile/middle_end/transform/specialize_ref.cmo \
      $(SRC_DIR)/compile/middle_end/transform/specialize.cmo \
      $(SRC_DIR)/compile/middle_end/transform/lambda_lifting.cmo \
      $(SRC_DIR)/compile/middle_end/transform/macro.cmo \
      $(SRC_DIR)/compile/middle_end/transform/inline.cmo \
      $(SRC_DIR)/compile/middle_end/transform/matching.cmo \
      \
      $(SRC_DIR)/compile/middle_end/normalize/move_down_gfun_under_exec_and_par.cmo \
      $(SRC_DIR)/compile/middle_end/normalize/instantiate.cmo \
      \
      $(SRC_DIR)/compile/middle_end/normalize/fun_shape_entry_point.cmo \
      $(SRC_DIR)/compile/middle_end/rename_main_arg.cmo \
      $(SRC_DIR)/compile/backend/naming_convention.cmo \
      $(SRC_DIR)/compile/inspect/display_internal_steps.cmo \
      $(SRC_DIR)/compile/middle_end/monomorphize.cmo \
      $(SRC_DIR)/compile/middle_end/middle_end.cmo \
      \
      $(SRC_DIR)/compile/backend/target/fsm_syntax.cmo \
      $(SRC_DIR)/compile/inspect/display_target.cmo \
      $(SRC_DIR)/compile/backend/target/fsm_typing.cmo \
      $(SRC_DIR)/compile/backend/fsm_comp.cmo \
      \
      $(SRC_DIR)/compile/backend/flat_let_atom.cmo \
      $(SRC_DIR)/compile/backend/list_machines.cmo \
      \
      $(SRC_DIR)/compile/backend/optim/target_propagation.cmo \
      $(SRC_DIR)/compile/backend/optim/remove_assigned_but_not_read_vars.cmo \
      \
      $(SRC_DIR)/compile/backend/gen_vhdl.cmo \
      \
      $(SRC_DIR)/compile/compile.cmo \
      $(SRC_DIR)/compile/gen_testbench/gen_testbench.cmo\
      \
      $(SRC_DIR)/interp/norm.cmo \
      $(SRC_DIR)/interp/interp.cmo \
      \
      $(SRC_DIR)/frontend/parser.cmo \
      $(SRC_DIR)/frontend/lexer.cmo \
      $(SRC_DIR)/frontend/frontend.cmo \
      $(SRC_DIR)/frontend/clean_simul.ml \
      $(SRC_DIR)/compile/gen_top/gen_top.cmo \
      \
      $(SRC_DIR)/main.cmo


SRCS= `find $(SRC_DIR) -name "*.ml*"`

all:	depend $(SRC_DIR)/frontend/parser.cmi $(OBJS)
	$(CAMLC) $(CAML_FLAGS) $(INCLUDES) -o $(EXE) $(OBJS)

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi

.ml.cmo:
	$(CAMLC) $(INCLUDES) $(CAML_FLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(INCLUDES) $(CAML_FLAGS) -c $<

.mly.ml:
	$(MENHIR) $<

.mll.ml:
	$(CAMLLEX) $<

depend:
	$(CAMLDEP) $(INCLUDES) $(SRCS) > $(SRC_DIR)/.depend
	menhir --depend $(SRC_DIR)/frontend/parser.mly >> $(SRC_DIR)/.depend

include $(SRC_DIR)/.depend

NS=2000

simul:
	cd ../target; make NS=$(NS)

clean:
	rm -f `find . -name "*.cmo"`
	rm -f `find . -name "*.cmi"`
	rm -f $(EXE)

clean-all:	clean
	make -f vhdl/Makefile clean-all
