
# Racing the Beam Ray Tracer

This project implements a Real-Time Ray-Tracer on a small FPGA.

You can read all about it [here](https://tomverbeure.github.io/rtl/2018/11/26/Racing-the-Beam-Ray-Tracer.html).

## HW Requirements

* A Pano Logic G1 (one with VGA port)
* A JTAG programmer to load the bitstream into the FPGA

## Loading the Bitstream

If you simply want to run the finished project on your Pano, then all you need to
do is load a pre-made bitstream into the device.

The bitstream can be found [here](./xilinx/Pano.bit).

## Changing the software

You don't to resynthesize the bitstream to change the software that runs on the MR1 RISC-V processor.

Once you have made the changes to your C code, just do the following:

```
cd xilinx
make update_ram
```

This will update the Pano.bit file with your latest binary.

## Building Everything from Scratch

* Install RISC-V GCC compiler suite.

    See the [picorv32](https://github.com/cliffordwolf/picorv32) github project on how to do that.
    It can take a long time to build!

* Install [SpinalHDL](https://github.com/SpinalHDL/SpinalHDL).

    The actual installation instructions can be found in the [VexRisc]( https://github.com/SpinalHDL/VexRiscv#dependencies)
    project.

* Clone my [math](https://github.com/tomverbeure/math) github repository to `~/projects/math`.


* Clone this github repository to `~/projects/rt`.

* Patch `~/projects/rt/build.sbt` so that it points to your local math library. Use absolute directory paths!

* Build font binary file

```
cd ~/projects/rt/fonts
make
```

* Build the firmware. This creates a file called `./sw/progmem8k.bin`.

```
cd ~/projects/rt/sw
make
```

* Build the Verilog for synthesis.

    When you run this, not only will you create the `Pano.v` file, but also a bunch of
    `.bin` file that contain RAM initialization contents, which will be loaded by
    Xilinx ISE during synthesis

```
cd ~/projects/rt
make syn
```

* Fire up Xilinx ISE

```
cd ~/projects/rt/xilinx
make ise
```

* Create the bitstream

    * File -> Open Project -> ~/projects/rt/xilinx/xilinx.xise
    * Double click on 'Generate Programming File'


* Fire up Xilinx Impact

```
cd ~/projects/rt/xilinx
make impact
```

* Load bitstream into the device

    <To be done>

## Simulation

I have an eye-balling-only simulation set up.

To use it, you first need to make 1 change in the RTL:

Change [this line](https://github.com/tomverbeure/rt/blob/23d486e72243ae706c7d2630ca06af3c1aecb0c1/src/main/scala/rt/RT.scala#L52) to
```
    val hwMulGlobal = false
```

After that, just do `make sim`.

The simulation can take quite a while because it simulates a full frame at 648x480.

To see waveforms, do `make waves`.

Don't forget to do the `hwMulGlobal` change before you generate the synthesis Pano.v, because otherwise the design won't fit in the FPGA!

