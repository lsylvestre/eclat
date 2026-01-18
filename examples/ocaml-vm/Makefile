NS=50000   # number of clock cycles (for simulation)

FLAGS=
SRC=tests/fibo.ml

all: vm


ECLAT_COMPILER=../../eclat-compiler
ECLAT_FLAGS=
OCAML_VM=ocaml-vm
VM_FROM_ECLAT_COMPILER=../examples/$(OCAML_VM)
TARGET_DIR=../../target
GEN_BYTECODE_DIR=gen_bytecode

BCOPT=

CUSTOM=
CUSTOM_PATH=$(foreach f,$(CUSTOM),../$(f))$
CUSTOM_NAMES=
CUSTOM_DECL=$(foreach f,$(CUSTOM_NAMES),-custom $(f))$


byte:
	(cd $(GEN_BYTECODE_DIR) ; make SRC="$(SRC)" BCOPT="$(CUSTOM_DECL)")



BOARD=intel-max10
ARG="((true, true, true, true, true, true, true, true, true, true),(true,true))"
CLK=clk

externals:
	(cd $(ECLAT_COMPILER); ./eclat -main=comparator $(ECLAT_FLAGS) \
	   stdlib.ecl \
	   $(VM_FROM_ECLAT_COMPILER)/vm/int.ecl \
	   $(VM_FROM_ECLAT_COMPILER)/vm/opcode.ecl \
	   $(VM_FROM_ECLAT_COMPILER)/vm/externals/comparator.ecl)

vm: $(ECLAT_COMPILER)/eclat
	(cd $(ECLAT_COMPILER); ./eclat $(ECLAT_FLAGS) \
		    $(FLAGS) -arg=$(ARG) \
		    stdlib.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/int.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/static.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/mlvalue.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/opcode.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/runtime.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/ram.ecl \
	         $(VM_FROM_ECLAT_COMPILER)/vm/fail.ecl \
            $(VM_FROM_ECLAT_COMPILER)/vm/debug.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/alloc.ecl \
	        $(VM_FROM_ECLAT_COMPILER)/vm/prims.ecl \
	        $(CUSTOM_PATH) \
			$(VM_FROM_ECLAT_COMPILER)/bytecode.ecl \
			$(VM_FROM_ECLAT_COMPILER)/vm/vm.ecl \
			$(VM_FROM_ECLAT_COMPILER)/vm/vm_step.ecl \
			$(VM_FROM_ECLAT_COMPILER)/vm/target-specific/$(BOARD)/IOs.ecl \
			$(VM_FROM_ECLAT_COMPILER)/vm/target-specific/$(BOARD)/main.ecl)

code: 
	(cd $(ECLAT_COMPILER); ./eclat -nonormalization $(ECLAT_FLAGS) \
       $(VM_FROM_ECLAT_COMPILER)/vm/int.ecl \
       $(VM_FROM_ECLAT_COMPILER)/vm/opcode.ecl \
       $(VM_FROM_ECLAT_COMPILER)/vm/mlvalue.ecl \
       $(VM_FROM_ECLAT_COMPILER)/get_code.ecl \
       -main=get_code)
	(cd $(TARGET_DIR); make --silent NAME=get_code NS=10 | sed '$$d' | tr -d '\000' > code.mif)

data: 
	(cd $(ECLAT_COMPILER); ./eclat $(ECLAT_FLAGS) \
       $(VM_FROM_ECLAT_COMPILER)/vm/int.ecl \
       $(VM_FROM_ECLAT_COMPILER)/vm/opcode.ecl \
       $(VM_FROM_ECLAT_COMPILER)/vm/mlvalue.ecl \
       $(VM_FROM_ECLAT_COMPILER)/init_data.ecl \
       -main=init_data)
	(cd $(TARGET_DIR); make --silent NAME=init_data NS=10 | sed '$$d' | tr -d '\000' > data_rom.mif)


ml:
	(cd $(GEN_BYTECODE_DIR) ; make SRC=../$(SRC) BCOPT="-load-code -load-data -ml-syntax")
	echo "(* CODE GENERATED FROM ECLAT SOURCE PROGRAM *)\n" > vm.ml
	cat vm/target-specific/ocaml/int.ml \
	    vm/target-specific/ocaml/static.ml \
	    vm/mlvalue.ecl \
	    vm/opcode.ecl \
	    vm/fail.ecl \
	    vm/ram.ecl \
	    vm/runtime.ecl \
        vm/debug.ecl \
	    vm/alloc.ecl \
	    vm/prims.ecl \
	    bytecode.ecl \
	    vm/vm.ecl \
	    vm/target-specific/ocaml/vm_main.ml >> vm.ml

bytecode:
	(cd $(GEN_BYTECODE_DIR) ; make SRC=../$(SRC) BCOPT="-load-code -load-data $(CUSTOM_DECL)")


simul:	bytecode
	make vm

synth:	byte
	make vm "FLAGS=-ram-init=code -ram-init=data_rom" SRC=$(SRC)


$(ECLAT_COMPILER)/eclat:
	(cd $(ECLAT_COMPILER); make)

clean:
	(cd $(ECLAT_COMPILER) ; make clean)
	(cd $(GEN_BYTECODE_DIR) ; make clean)
	rm -f get_code.ecl
	rm -f `find . -name "*.cmo"`
	rm -f `find . -name "*.cmi"`
	(cd $(TARGET_DIR); make clean)

