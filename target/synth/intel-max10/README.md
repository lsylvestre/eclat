## Synthesis on a Terassic DE-10Lite board (Intel MAX10 FPGA) using Quartus II


Installation
------------

To build from source, the pre-requisites are:

* Quartus II (>= 22.1) tool suite (to be downloaded from [Intel FPGA](https://fpgasoftware.intel.com) website)

From this directory:

```
$ cp ../../main.vhdl main.vhdl
$ cp ../../runtime.vhdl runtime.vhdl
$ cp ../../top.vhdl top.vhdl
$ ./{SOMEWHERE}/intelFPGA_lite/22.1std/quartus/bin/quartus  top.qpf
```
Then:

  - click `Processing > Start Compilation` to synthetize the hardware design
  - connect a DE-10Lite FPGA board (by USB)
  - click `Tools > Programmer > Start` to upload the hardware configuration

