YOSYS_HQ=~/oss-cad-suite

YOSYS_FLAGS= -abc2 -retime


UNCONSTRAINT=--lpf-allow-unconstrained 


init:
	source $(YOSYS_HQ)/environment


front:
	ghdl -a ../../runtime.vhdl ../../main.vhdl ../../top.vhdl

synth:
	yosys -g -m ghdl.so -p "ghdl --latches top; synth_ecp5 -top top $(YOSYS_FLAGS) -json top.json"

# place and route
pnr:
	nextpnr-ecp5 --json top.json --textcfg top_out.config --85k --package CSFBGA285 --lpf orangecrab_r0.2.1.pcf --log log.txt --freq 48 $(UNCONSTRAINT)

pack:
	ecppack --compress --input top_out.config --bit top.bit

dfu:
	cp -a top.bit top.dfu
	dfu-suffix -v 1209 -p 5af0 -a top.dfu
	dfu-util -a 0 -D top.dfu

clean:
	rm -rf log.* work-obj93.cf top.json top_out.config

