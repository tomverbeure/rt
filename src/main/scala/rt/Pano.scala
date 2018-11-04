
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

        val button              = in(Bool)
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
        //io.led_blue     := !led_cntr.msb
}
        val rtConfig = RTConfig()

        val video = new Area {
            val timings = VideoTimings()
            timings.h_active        := 640
            timings.h_fp            := 16
            timings.h_sync          := 96
            timings.h_bp            := 48
            timings.h_sync_positive := False
            timings.h_total_m1      := (timings.h_active + timings.h_fp + timings.h_sync + timings.h_bp -1).resize(timings.h_total_m1.getWidth)

            timings.v_active        := 480
            timings.v_fp            := 11
            timings.v_sync          := 2
            timings.v_bp            := 31
            timings.v_sync_positive := False
            timings.v_total_m1      := (timings.v_active + timings.v_fp + timings.v_sync + timings.v_bp -1).resize(timings.v_total_m1.getWidth)

            val vi_gen = new VideoTimingGen()
            vi_gen.io.timings := timings

            val vo = new VideoOut()
            vo.io.timings := timings
            vo.io.pixel_in <> vi_gen.io.pixel_out
            vo.io.vga_out <> io.vo
        }

        val cam_sweep = new CamSweep(rtConfig)

        val vec_test = new Area {

            val cntr0 = CounterFreeRun((1<<24))
            val cntr1 = GrayCounter(24, True)
            val cntr2 = Counter(1, 1<<24, True)

            val v_a = new Vec3(rtConfig)
            val v_b = new Vec3(rtConfig)
            val dot_r = new Fpxx(rtConfig.fpxxConfig)

            val button_fp = new Fpxx(rtConfig.fpxxConfig)
            button_fp.fromVec( B( Range(0, 1 + rtConfig.fpxxConfig.exp_size + rtConfig.fpxxConfig.mant_size, 1) -> io.button) )

            val vec0 = new Fpxx(rtConfig.fpxxConfig)
            vec0.fromVec( cntr0(0, rtConfig.fpxxConfig.full_size bits).asBits )

            val vec1 = new Fpxx(rtConfig.fpxxConfig)
            vec1.fromVec( cntr1(1, rtConfig.fpxxConfig.full_size bits).asBits )

            val vec2 = new Fpxx(rtConfig.fpxxConfig)
            vec2.fromVec( cntr2(2, rtConfig.fpxxConfig.full_size bits).asBits )

            v_a.x := vec0
            v_a.y := vec1
            v_a.z := vec2

            v_b.x := vec2
            v_b.y := vec1
            v_b.z := vec0

            if (false){
                val v_a_norm = Vec3(rtConfig)
                val v_a_norm_vld = Bool

                val u_normalize = new Normalize(rtConfig)
                u_normalize.io.op_vld      := True
                u_normalize.io.op          := v_a

                u_normalize.io.result_vld  <> v_a_norm_vld
                u_normalize.io.result      <> v_a_norm

                val u_dot = new DotProduct(rtConfig)
                u_dot.io.op_vld := v_a_norm_vld
                u_dot.io.op_a := v_a_norm
                u_dot.io.op_b := v_b
                dot_r := u_dot.io.result
            }


            //============================================================
            // Plane definition
            //============================================================

            val plane = new Plane(rtConfig)

            plane.origin.x.fromDouble(0.0)
            plane.origin.y.fromDouble(0.0)
            plane.origin.z.fromDouble(0.0)

            plane.normal.x.fromDouble(0.0)
            plane.normal.y.fromDouble(1.0)
            plane.normal.z.fromDouble(0.0)

            //============================================================
            // Ray definition
            //============================================================

            val ray = new Ray(rtConfig)

            ray.origin.x.fromDouble(  0.0)
            ray.origin.y.fromDouble( 10.0)
            ray.origin.z.fromDouble(-10.0)

            ray.direction.x := vec0
            ray.direction.y := vec1
            ray.direction.z.fromDouble(-1.0)

            //============================================================
            // ray_normalized
            //============================================================

            val ray_normalized_vld = Bool
            val ray_normalized = new Ray(rtConfig)

            ray_normalized.origin := ray.origin

            val u_normalize_ray = new Normalize(rtConfig)
            u_normalize_ray.io.op_vld      := True
            u_normalize_ray.io.op          <> ray.direction

            u_normalize_ray.io.result_vld  <> ray_normalized_vld
            u_normalize_ray.io.result      <> ray_normalized.direction

            //============================================================
            // plane intersect
            //============================================================

            val plane_intersect_vld     = Bool
            val plane_intersects        = Bool
            val plane_intersect_t       = Fpxx(rtConfig.fpxxConfig)
            val plane_intersection      = Vec3(rtConfig)

            val u_plane_intersect = new PlaneIntersect(rtConfig)
            u_plane_intersect.io.plane_vld  := ray_normalized_vld
            u_plane_intersect.io.plane      <> plane
            u_plane_intersect.io.ray        <> ray_normalized

            u_plane_intersect.io.result_vld             <> plane_intersect_vld
            u_plane_intersect.io.result_intersects      <> plane_intersects
            u_plane_intersect.io.result_t               <> plane_intersect_t
            u_plane_intersect.io.result_intersection    <> plane_intersection

            io.led_blue := plane_intersect_vld |
                           plane_intersects |
                           plane_intersect_t.toVec().orR |
                           plane_intersection.x.toVec().orR |
                           plane_intersection.y.toVec().orR |
                           plane_intersection.z.toVec().orR
        }

    }

}
