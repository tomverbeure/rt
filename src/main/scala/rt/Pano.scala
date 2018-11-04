
package rt

import spinal.core._
import spinal.lib.Counter
import spinal.lib.CounterFreeRun
import spinal.lib.GrayCounter
import math._


class Pano extends Component {

    val io = new Bundle {
        val osc_clk             = in(Bool)

        val vo_clk              = out(Bool)
        val vo                  = out(VgaData())

        val led_green           = out(Bool)
        val led_blue            = out(Bool)
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
    }

}
