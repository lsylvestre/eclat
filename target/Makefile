GHDL=ghdl
GHDLOPTS=


NAME=main

MAIN=$(NAME).vhdl
EXTERNALS=
RUNTIME=runtime.vhdl stdlib.vhdl



NS=2000

vhdl.run: $(RUNTIME) $(MAIN) tb_$(NAME).vhdl
	$(GHDL) -a $(GHDLOPTS) $(RUNTIME) $(EXTERNALS)
	$(GHDL) -a $(GHDLOPTS) $(MAIN)
	$(GHDL) -a $(GHDLOPTS) tb_$(NAME).vhdl
	$(GHDL) -e $(GHDLOPTS) tb_$(NAME)
	$(GHDL) -r $(GHDLOPTS) tb_$(NAME) --vcd=tb.vcd --stop-time=$(NS)ns

clean:
	rm -f *~
	rm -f *.o *.dot *.vhd *.cf
	rm -f tb tb.vcd
	(cd synth/yosys-ecp5; make clean)

clean-all:	clean
	rm -f main.vhdl tb_main.vhdl