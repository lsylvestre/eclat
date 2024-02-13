NS=50000   # number of clock cycles (for simulation)

FLAGS=
SRC=ocaml-vm/tests/fibo.ml

all: vm


ECLAT_COMPILER=eclat-compiler
OCAML_VM=ocaml-vm
VM_FROM_ECLAT_COMPILER=../$(OCAML_VM)
TARGET_DIR="target"
GEN_BYTECODE_DIR=$(OCAML_VM)/gen_bytecode

byte:
	(cd $(GEN_BYTECODE_DIR) ; make SRC=../../$(SRC))


CUSTOM=
CUSTOM_PATH=$(foreach f,$(CUSTOM),../$(f))$

CLK=clk

vm:	byte $(ECLAT_COMPILER)/eclat
	(cd $(ECLAT_COMPILER); ./eclat \
		     -arg "((true,true,true,true,true,true,true,true,true,true),(true,false))" \
		    $(FLAGS) \
	        $(VM_FROM_ECLAT_COMPILER)/vm/mlvalue.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/fail.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/ram.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/runtime.ecl \
			$(VM_FROM_ECLAT_COMPILER)/vm/debug.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/alloc.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/prims.ecl $(CUSTOM_PATH) \
			$(VM_FROM_ECLAT_COMPILER)/bytecode.ecl \
			$(VM_FROM_ECLAT_COMPILER)/vm/vm.ecl \
			$(VM_FROM_ECLAT_COMPILER)/vm/target-specific/intel-max10/IOs.ecl \
			$(VM_FROM_ECLAT_COMPILER)/vm/target-specific/intel-max10/main.ecl)

$(ECLAT_COMPILER)/eclat:
	(cd $(ECLAT_COMPILER); make)

simul:
	(cd $(TARGET_DIR); make NS=$(NS))

clean:
	(cd $(ECLAT_COMPILER) ; make clean)
	(cd $(GEN_BYTECODE_DIR) ; make clean)
	rm -f `find . -name "*.cmo"`
	rm -f `find . -name "*.cmi"`
	(cd $(TARGET_DIR); make clean)

