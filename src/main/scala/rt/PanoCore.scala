
package rt

import spinal.core._
import spinal.lib.Counter
import spinal.lib.CounterFreeRun
import spinal.lib.GrayCounter
import math._


class PanoCore extends Component {

    val io = new Bundle {
        val vo                  = out(VgaData())

        val led_green           = out(Bool)
        val led_blue            = out(Bool)
    }


    val leds = new Area {
        val led_cntr = Reg(UInt(24 bits)) init(0)

        when(led_cntr === U(led_cntr.range -> true)){
            led_cntr := 0
        }
        .otherwise {
            led_cntr := led_cntr +1
        }

        io.led_green    := led_cntr.msb
    }

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

    val vi_gen_pixel_out = PixelStream()

    val vi_gen = new VideoTimingGen()
    vi_gen.io.timings       <> timings
    vi_gen.io.pixel_out     <> vi_gen_pixel_out

    val rt = new Area {
        val rtConfig = RTConfig()

        val cam_sweep_pixel = PixelStream()
        val ray             = Ray(rtConfig)

        val u_cam_sweep = new CamSweep(rtConfig)
        u_cam_sweep.io.pixel_in     <> vi_gen_pixel_out
        u_cam_sweep.io.pixel_out    <> cam_sweep_pixel
        u_cam_sweep.io.ray          <> ray

        //============================================================
        // Scene definition
        //============================================================

        val plane = new Plane(rtConfig)

        plane.origin.x.fromDouble(0.0)
        plane.origin.y.fromDouble(0.0)
        plane.origin.z.fromDouble(0.0)

        plane.normal.x.fromDouble(0.0)
        plane.normal.y.fromDouble(1.0)
        plane.normal.z.fromDouble(0.0)

        val calc_sphere_pos = new Area {
            // Sphere center y location is a parabola from 3 -> 10 -> 0 in 90 frames.
            // y = a*x*(x-90)+3 -> a = -7/(45*45) = -0.0034567901
            val cycle_time = 90.0
            val min_height = 3.0
            val max_height = 10.0
            val a = -(max_height - min_height)/(cycle_time/2)/(cycle_time/2)

            val s_time = Reg(SInt(8 bits)) init (45)
    
            when(vi_gen_pixel_out.req && vi_gen_pixel_out.eof){

                when(s_time === S(89, 8 bits)){
                    s_time := 0
                }
                .otherwise{
                    s_time := s_time + 1
                }
            }

            val s_time_m90 = SInt(8 bits)
            s_time_m90 := s_time - 90
    
            val s_time_q = Reg(SInt((s_time.getWidth*2) bits))
            s_time_q := s_time * s_time_m90
    
            val s_time_q_fp_vld = Bool
            val s_time_q_fp     = Fpxx(rtConfig.fpxxConfig)
    
            val u_s_time_q_fp = new SInt2Fpxx(s_time_q.getWidth, rtConfig.fpxxConfig)
            u_s_time_q_fp.io.op_vld        <> (vi_gen_pixel_out.req && vi_gen_pixel_out.eof)
            u_s_time_q_fp.io.op            <> s_time_q
            u_s_time_q_fp.io.result_vld    <> s_time_q_fp_vld
            u_s_time_q_fp.io.result        <> s_time_q_fp
    
            val const_a = Fpxx(rtConfig.fpxxConfig)
            const_a.fromDouble(a)
    
            val a_term_vld = Bool
            val a_term     = Fpxx(rtConfig.fpxxConfig)
    
            val u_a_term = new FpxxMul(rtConfig.fpxxConfig, Constants.fpxxMulConfig)
            u_a_term.io.op_vld       <> s_time_q_fp_vld
            u_a_term.io.op_a         <> s_time_q_fp
            u_a_term.io.op_b         <> const_a
        
            u_a_term.io.result_vld   <> a_term_vld
            u_a_term.io.result       <> a_term

            val const_min_y = Fpxx(rtConfig.fpxxConfig)
            const_min_y.fromDouble(min_height)

            val sphere_y_vld = Bool
            val sphere_y     = Fpxx(rtConfig.fpxxConfig)

            val u_sphere_y = new FpxxAdd(rtConfig.fpxxConfig, Constants.fpxxAddConfig)
            u_sphere_y.io.op_vld     <> a_term_vld
            u_sphere_y.io.op_a       <> a_term
            u_sphere_y.io.op_b       <> const_min_y
        
            u_sphere_y.io.result_vld <> sphere_y_vld
            u_sphere_y.io.result     <> sphere_y
        }

        val sphere = new Sphere(rtConfig)

        sphere.center.x.fromDouble(3.0)
        sphere.center.y := calc_sphere_pos.sphere_y
        sphere.center.z.fromDouble(10.0)

        sphere.radius2.fromDouble(9.0)

        //============================================================
        // Ray definition
        //============================================================

        if (false){
            // For debugging...
            ray.origin.x.fromDouble(  0.0)
            ray.origin.y.fromDouble( 10.0)
            ray.origin.z.fromDouble(-10.0)

            ray.direction.x.fromDouble(0.125)
            ray.direction.y.fromDouble(-0.0749969482)
            ray.direction.z.fromDouble(1.0)
        }

        //============================================================
        // ray_normalized
        //============================================================

        val ray_normalized_vld = Bool
        val ray_normalized = new Ray(rtConfig)

        ray_normalized.origin := ray.origin

        val u_normalize_ray = new Normalize(rtConfig)
        u_normalize_ray.io.op_vld      <> cam_sweep_pixel.req
        u_normalize_ray.io.op          <> ray.direction

        u_normalize_ray.io.result_vld  <> ray_normalized_vld
        u_normalize_ray.io.result      <> ray_normalized.direction

        //============================================================
        // sphere intersect
        //============================================================

        val sphere_result_vld   = Bool
        val sphere_intersects   = Bool
        val sphere_reflect_ray  = Ray(rtConfig)
        val sphere_ray          = Ray(rtConfig)

        val u_sphere_intersect = new SphereIntersect(rtConfig)
        u_sphere_intersect.io.op_vld    <> ray_normalized_vld
        u_sphere_intersect.io.sphere    <> sphere
        u_sphere_intersect.io.ray       <> ray_normalized

        u_sphere_intersect.io.result_vld            <> sphere_result_vld
        u_sphere_intersect.io.result_intersects     <> sphere_intersects
        u_sphere_intersect.io.result_reflect_ray    <> sphere_reflect_ray
        u_sphere_intersect.io.result_ray            <> sphere_ray
        // u_sphere_intersect.io.result_t
        // u_sphere_intersect.io.result_intersection
        // u_sphere_intersect.io.result_normal

        //============================================================
        // plane intersect
        //============================================================

        val plane_ray = Ray(rtConfig)
        plane_ray := sphere_intersects ? sphere_reflect_ray | sphere_ray

        val plane_intersect_vld     = Bool
        val plane_intersects        = Bool
        val plane_intersect_t       = Fpxx(rtConfig.fpxxConfig)
        val plane_intersection      = Vec3(rtConfig)

        val u_plane_intersect = new PlaneIntersect(rtConfig)
        u_plane_intersect.io.op_vld     <> sphere_result_vld
        u_plane_intersect.io.plane      <> plane
        u_plane_intersect.io.ray        <> plane_ray

        u_plane_intersect.io.result_vld             <> plane_intersect_vld
        u_plane_intersect.io.result_intersects      <> plane_intersects
        u_plane_intersect.io.result_t               <> plane_intersect_t
        u_plane_intersect.io.result_intersection    <> plane_intersection

        //============================================================
        // Plane Checker Board
        //============================================================

        val plane_intersection_x_abs = Fpxx(rtConfig.fpxxConfig)
        val plane_intersection_z_abs = Fpxx(rtConfig.fpxxConfig)

        plane_intersection_x_abs := plane_intersection.x.abs()
        plane_intersection_z_abs := plane_intersection.z.abs()

        val plane_x_int_vld = Bool
        val plane_x_int     = SInt(20 bits)

        val u_plane_x_int = new Fpxx2SInt(8, 12, rtConfig.fpxxConfig)
        u_plane_x_int.io.op_vld     <> plane_intersect_vld
        u_plane_x_int.io.op         <> plane_intersection_x_abs

        u_plane_x_int.io.result_vld <> plane_x_int_vld
        u_plane_x_int.io.result     <> plane_x_int

        val plane_z_int_vld = Bool
        val plane_z_int     = SInt(20 bits)

        val u_plane_z_int = new Fpxx2SInt(8, 12, rtConfig.fpxxConfig)
        u_plane_z_int.io.op_vld     <> plane_intersect_vld
        u_plane_z_int.io.op         <> plane_intersection_z_abs

        u_plane_z_int.io.result_vld <> plane_z_int_vld
        u_plane_z_int.io.result     <> plane_z_int

        //============================================================
        // plane color
        //============================================================

        val (plane_intersection_x_vld_delayed, plane_intersection_x_sign_delayed, plane_x_int_delayed_0) = MatchLatency(
                                                                plane_intersect_vld,
                                                                plane_intersect_vld, plane_intersection.x.sign,
                                                                plane_x_int_vld,     plane_x_int)

        val (plane_intersection_z_vld_delayed, plane_intersection_z_sign_delayed, plane_z_int_delayed_0) = MatchLatency(
                                                                plane_intersect_vld,
                                                                plane_intersect_vld, plane_intersection.z.sign,
                                                                plane_z_int_vld,     plane_z_int)

        val checker_color = plane_x_int(14) ^ plane_z_int(14) ^ plane_intersection_x_sign_delayed ^ plane_intersection_z_sign_delayed

        //============================================================
        // plane_in_bounds
        //============================================================

        val (plane_intersect_vld_delayed, plane_intersects_delayed, plane_x_int_delayed_1) = MatchLatency(
                                                                plane_intersect_vld,
                                                                plane_intersect_vld, plane_intersects,
                                                                plane_x_int_vld,     plane_x_int)

        val plane_in_bounds = plane_x_int(12, 8 bits) < 16 && plane_z_int(12, 8 bits) < 28
        val plane_intersects_final = plane_in_bounds && plane_intersects_delayed

        //============================================================
        // Final color
        //============================================================


        val (sphere_result_delayed_vld, sphere_intersects_delayed, plane_x_int_delayed_2) = MatchLatency(
                                                                sphere_result_vld,
                                                                sphere_result_vld,   sphere_intersects,
                                                                plane_x_int_vld,     plane_x_int)

        val (cam_sweep_pixel_delayed_vld, cam_sweep_pixel_delayed, plane_x_int_delayed_3) = MatchLatency(
                                                                cam_sweep_pixel.req,
                                                                cam_sweep_pixel.req, cam_sweep_pixel,
                                                                plane_x_int_vld,     plane_x_int)


        var sky = Pixel()
        sky.setColor(0.0, 0.0, 0.9)

        var yellow = Pixel()
        yellow.setColor(0.9, 0.9, 0.0)

        var red = Pixel()
        red.setColor(1.0, 0.0, 0.0)

        var green = Pixel()
        green.setColor(0.0, 1.0, 0.0)

        var yellow_red = Pixel()
        yellow_red.setColor(0.95, 0.7, 0.0)

        var yellow_green = Pixel()
        yellow_green.setColor(0.95, 0.0, 0.7)


        var cyan = Pixel()
        cyan.setColor(0.0, 1.0, 1.0)

        val rt_pixel = PixelStream()
        rt_pixel := cam_sweep_pixel_delayed

        when(sphere_intersects_delayed && !plane_intersects_final){
            rt_pixel.pixel := yellow
        }
        .elsewhen(sphere_intersects_delayed && plane_intersects_final){
            rt_pixel.pixel := checker_color ? yellow_red | yellow_green
        }
        .elsewhen(plane_intersects_final){
            rt_pixel.pixel := checker_color ? red | green
        }
        .otherwise{
            rt_pixel.pixel := sky
        }

    }

    val vo = new VideoOut()
    vo.io.timings := timings
    vo.io.pixel_in <> rt.rt_pixel
    vo.io.vga_out <> io.vo

    io.led_blue := True
}


