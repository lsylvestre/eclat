CAMLC=ocamlopt
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
         -I $(SRC_DIR)/compile/middle_end/transform \
         -I $(SRC_DIR)/compile/middle_end/normalize \
         -I $(SRC_DIR)/compile/inspect \
         -I $(SRC_DIR)/compile/backend \
         -I $(SRC_DIR)/compile/backend/optim \
         -I $(SRC_DIR)/compile/backend/target \
         -I $(SRC_DIR)/compile/gen_top \
         -I $(SRC_DIR)/compile/gen_testbench

MENHIR=menhir --unused-tokens --unused-precedence-levels --infer --ocamlc "ocamlc -i $(INCLUDES)"

OBJS= $(SRC_DIR)/prelude.cmx \
      \
      $(SRC_DIR)/frontend/current_filename.cmx \
      $(SRC_DIR)/compile/backend/flag_mealy.cmx \
      \
      $(SRC_DIR)/typing/types.cmx \
      $(SRC_DIR)/operators.cmx \
      $(SRC_DIR)/syntax/ast.cmx \
      $(SRC_DIR)/syntax/pattern.cmx \
      $(SRC_DIR)/syntax/map_pi.cmx \
      $(SRC_DIR)/syntax/ast_mapper.cmx \
      $(SRC_DIR)/syntax/ast_pprint.cmx \
      $(SRC_DIR)/syntax/ast_subst.cmx \
      $(SRC_DIR)/syntax/ast_rename.cmx \
      $(SRC_DIR)/syntax/free_vars.cmx \
      $(SRC_DIR)/syntax/rename_fix.cmx \
      $(SRC_DIR)/syntax/ast_undecorated.cmx \
      \
      $(SRC_DIR)/frontend/fix_int_lit_size.cmx \
      \
      $(SRC_DIR)/compile/instantaneous.cmx \
      $(SRC_DIR)/typing/typing.cmx \
      $(SRC_DIR)/syntax/ast_mk.cmx \
      $(SRC_DIR)/compile/middle_end/check_tail_call.cmx \
      $(SRC_DIR)/compile/middle_end/check_exec_mem.cmx \
      $(SRC_DIR)/compile/middle_end/normalize/fun_assign_name.cmx \
      \
      $(SRC_DIR)/compile/middle_end/normalize/propagation.cmx \
      \
      $(SRC_DIR)/compile/middle_end/normalize/anf.cmx \
      $(SRC_DIR)/compile/middle_end/normalize/let_floating.cmx \
      $(SRC_DIR)/compile/middle_end/transform/insert_bound_checking.cmx \
      $(SRC_DIR)/compile/middle_end/transform/globalize_arrays.cmx \
      $(SRC_DIR)/compile/middle_end/transform/inline.cmx \
      $(SRC_DIR)/compile/middle_end/transform/specialize_ref.cmx \
      $(SRC_DIR)/compile/middle_end/transform/specialize.cmx \
      $(SRC_DIR)/compile/middle_end/transform/lambda_lifting.cmx \
      $(SRC_DIR)/compile/middle_end/transform/unroll.cmx \
      $(SRC_DIR)/compile/middle_end/transform/matching.cmx \
      $(SRC_DIR)/compile/middle_end/transform/expand.cmx \
      \
      $(SRC_DIR)/compile/middle_end/normalize/move_down_gfun_under_exec_and_par.cmx \
      $(SRC_DIR)/compile/middle_end/normalize/instantiate.cmx \
      \
      $(SRC_DIR)/compile/middle_end/normalize/fun_shape_entry_point.cmx \
      $(SRC_DIR)/compile/middle_end/rename_main_arg.cmx \
      $(SRC_DIR)/compile/backend/naming_convention.cmx \
      $(SRC_DIR)/compile/inspect/display_internal_steps.cmx \
      $(SRC_DIR)/compile/middle_end/monomorphize.cmx \
      $(SRC_DIR)/compile/middle_end/middle_end.cmx \
      \
      $(SRC_DIR)/compile/backend/target/fsm_syntax.cmx \
      $(SRC_DIR)/compile/inspect/display_target.cmx \
      $(SRC_DIR)/compile/backend/target/fsm_typing.cmx \
      $(SRC_DIR)/compile/backend/fsm_comp.cmx \
      $(SRC_DIR)/compile/backend/count_externals.cmx \
      \
      $(SRC_DIR)/compile/backend/flat_let_atom.cmx \
      $(SRC_DIR)/compile/backend/list_machines.cmx \
      \
      $(SRC_DIR)/compile/backend/optim/target_propagation.cmx \
      $(SRC_DIR)/compile/backend/optim/remove_assigned_but_not_read_vars.cmx \
      \
      $(SRC_DIR)/compile/backend/gen_vhdl_aux.cmx \
      $(SRC_DIR)/compile/backend/gen_vhdl.cmx \
      $(SRC_DIR)/compile/backend/gen_vhdl_moore.cmx \
      \
      $(SRC_DIR)/compile/compile.cmx \
      $(SRC_DIR)/compile/gen_testbench/gen_testbench.cmx\
      \
      $(SRC_DIR)/interp/special_subst.cmx \
      $(SRC_DIR)/interp/interp.cmx \
      \
      $(SRC_DIR)/frontend/parser.cmx \
      $(SRC_DIR)/frontend/lexer.cmx \
      $(SRC_DIR)/frontend/frontend.cmx \
      $(SRC_DIR)/frontend/clean_simul.ml \
      $(SRC_DIR)/compile/gen_top/gen_top.cmx \
      \
      $(SRC_DIR)/main.cmx


SRCS= `find $(SRC_DIR) -name "*.ml*"`

all:	depend $(SRC_DIR)/frontend/parser.ml $(OBJS)
	$(CAMLC) $(CAML_FLAGS) $(INCLUDES) -o $(EXE) $(OBJS)

.SUFFIXES: .mll .mly .ml .mli .cmx .cmi

.ml.cmx:
	$(CAMLC) $(INCLUDES) $(CAML_FLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(INCLUDES) $(CAML_FLAGS) -c $<

.mly.ml:
	$(MENHIR) $<

.mll.ml:
	$(CAMLLEX) $<

#%.cmo: %.ml
#	ocamlc $(INCLUDES) $(CAML_FLAGS) -c $<

depend:
	$(CAMLDEP) $(INCLUDES) $(SRCS) > $(SRC_DIR)/.depend
	$(MENHIR) --depend $(SRC_DIR)/frontend/parser.mly >> $(SRC_DIR)/.depend

include $(SRC_DIR)/.depend

NS=2000

simul:
	cd ../target; make NS=$(NS)

clean:
	rm -f `find . -name "*.cmx"`
	rm -f `find . -name "*.cmi"`
	rm -f `find . -name "*.o"`
	rm -f $(EXE)

clean-all:	clean
	# rm -f src/frontend/parser.ml src/frontend/parser.mli
	make -f ../target/Makefile clean-all

TEST_PATH=tests
TESTS=find $(TEST_PATH) -name *.ecl

test:
	@$(foreach t,$(shell $(TESTS)), echo "============================\n== $(t)"; \
              $(shell head -n 2 ${t}  | tail -n 1) ;)


