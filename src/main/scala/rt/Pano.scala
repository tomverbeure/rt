
package rt

import spinal.core._
import spinal.lib.Counter
import spinal.lib.CounterFreeRun
import spinal.lib.GrayCounter
import spinal.lib.master
import spinal.lib.io.{ReadableOpenDrain, TriStateArray, TriState}
import math._


class Pano extends Component {

    val io = new Bundle {
        val osc_clk             = in(Bool)

        val vo_clk              = out(Bool)
        val vo                  = out(VgaData())

        val led_green           = out(Bool)
        val led_blue            = out(Bool)

        val usb_reset_n         = out(Bool)
        val usb_clkin           = out(Bool)

        val usb_a               = out(UInt(17 bits))
        val usb_d               = master(TriStateArray(16 bits))
        val usb_cs_             = out(Bool)
        val usb_rd_             = out(Bool)
        val usb_wr_             = out(Bool)
        val usb_irq             = in(Bool)
    }

    noIoPrefix()

    //============================================================
    // Create osc_clk clock domain
    //============================================================
    val resetCtrlClockDomain = ClockDomain(
        clock = io.osc_clk,
        frequency = FixedFrequency(100 MHz),
        config = ClockDomainConfig(
                    resetKind = BOOT
        )
    )

    //============================================================
    // Create osc_clk clock domain
    //============================================================
    val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
        val reset_unbuffered_ = True

        val reset_cntr = Reg(UInt(5 bits)) init(0)
        when(reset_cntr =/= U(reset_cntr.range -> true)){
            reset_cntr := reset_cntr + 1
            reset_unbuffered_ := False
        }

        val reset_ = RegNext(reset_unbuffered_)

        // Create div4 clock
        val clk_cntr = Reg(UInt(2 bits)) init(0)
        clk_cntr := clk_cntr + 1
        val clk25  = RegNext(clk_cntr(1))
    }


    val clkVoClockDomain = ClockDomain(
        clock = resetCtrl.clk25,
        reset = resetCtrl.reset_,
        config = ClockDomainConfig(
            resetKind = SYNC,
            resetActiveLevel = LOW
        )
    )

    io.vo_clk <> resetCtrl.clk25

    val core = new ClockingArea(clkVoClockDomain) {

        val u_pano_core = new PanoCore()
        u_pano_core.io.vo           <> io.vo

        u_pano_core.io.led_green    <> io.led_green
        u_pano_core.io.led_blue     <> io.led_blue

        u_pano_core.io.usb_a        <> io.usb_a
        u_pano_core.io.usb_d        <> io.usb_d
        u_pano_core.io.usb_cs_      <> io.usb_cs_
        u_pano_core.io.usb_rd_      <> io.usb_rd_
        u_pano_core.io.usb_wr_      <> io.usb_wr_
        u_pano_core.io.usb_irq      <> io.usb_irq
    }

}
