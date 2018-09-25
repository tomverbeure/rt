
package rt

import spinal.core._


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

    io.vo_clk := resetCtrl.clk25

    val core = new ClockingArea(clkVoClockDomain) {

        val led_cntr = Reg(UInt(24 bits)) init(0)

        when(led_cntr === U(led_cntr.range -> true)){
            led_cntr := 0
        }
        .otherwise {
            led_cntr := led_cntr +1
        }

        io.led_green    := led_cntr.msb

if (false){
        val op_a    = Reg(UInt(32 bits)) init(0)
        val op_b    = Reg(UInt(32 bits)) init(0)

        val op_c    = Reg(UInt(32 bits)) init(0)
        val op_d    = Reg(UInt(32 bits)) init(0)

        op_a := op_a + 1
        op_b := op_b + 1
        op_c := op_c + 1
        op_d := op_d + 1

        val op_ab    = Reg(Reg(UInt(32 bits)) init(0))
        val op_cd    = Reg(Reg(UInt(32 bits)) init(0))

        op_ab := ((op_a >> 8) * (op_b >> 8))(op_ab.range)
        op_cd := ((op_c >> 8) * (op_d >> 8))(op_cd.range)

        val op_abcd = Reg(UInt(32 bits)) init(0)
        op_abcd := op_ab + op_cd

        io.led_blue     := !op_abcd.msb
}
else{
        io.led_blue     := !led_cntr.msb
}

        val timings = VideoTimings()
        timings.h_active := 640
        timings.h_fp := 16
        timings.h_sync := 96
        timings.h_bp := 48
        timings.h_sync_positive := False
        timings.h_total_m1 := (timings.h_active + timings.h_fp + timings.h_sync + timings.h_bp -1).resize(timings.h_total_m1.getWidth)

        timings.v_active := 480
        timings.v_fp := 11
        timings.v_sync := 2
        timings.v_bp := 31
        timings.v_sync_positive := False
        timings.v_total_m1 := (timings.v_active + timings.v_fp + timings.v_sync + timings.v_bp -1).resize(timings.v_total_m1.getWidth)

        val vi_gen = new VideoTimingGen()
        vi_gen.io.timings := timings

        val vo = new VideoOut()
        vo.io.timings := timings
        vo.io.pixel_in <> vi_gen.io.pixel_out
        vo.io.vga_out <> io.vo

        val cam_sweep = new CamSweep()

    }

}
