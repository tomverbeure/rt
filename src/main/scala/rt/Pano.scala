
package rt

import spinal.core._


class Pano extends Component {

    val io = new Bundle {
        val osc_clk             = in(Bool)

//        val vga                 = Vga()

        val led_green           = out(Bool)
        val led_blue            = out(Bool)
    }

    noIoPrefix()

    val resetCtrlClockDomain = ClockDomain(
        clock = io.osc_clk,
        frequency = FixedFrequency(100 MHz),
        config = ClockDomainConfig(
                    resetKind = BOOT
        )
    )

    val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
        val reset_unbuffered_ = True

        val reset_cntr = Reg(UInt(5 bits)) init(0)
        when(reset_cntr =/= U(reset_cntr.range -> true)){
            reset_cntr := reset_cntr + 1
            reset_unbuffered_ := False
        }

        val reset_ = RegNext(reset_unbuffered_)
    }

    val coreClockDomain = ClockDomain(
        clock = io.osc_clk,
        reset = resetCtrl.reset_,
        config = ClockDomainConfig(
            resetKind = SYNC,
            resetActiveLevel = LOW
        )
    )

    val core = new ClockingArea(coreClockDomain) {

        val led_cntr = Reg(UInt(26 bits)) init(0)

        when(led_cntr === U(led_cntr.range -> true)){
            led_cntr := 0
        }
        .otherwise {
            led_cntr := led_cntr +1
        }

        io.led_green    := led_cntr.msb
        io.led_blue     := !led_cntr.msb
    }

}
