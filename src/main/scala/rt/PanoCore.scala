
package rt

import spinal.core._
import spinal.lib.Counter
import spinal.lib.CounterFreeRun
import spinal.lib.GrayCounter

import math._
import mr1._


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

    val rtConfig = RTConfig()

    val eof_final = Bool

    val mr1Config = MR1Config()
    val u_mr1_top = new MR1Top(mr1Config, rtConfig)
    u_mr1_top.io.led1       <> io.led_blue
    u_mr1_top.io.switch_    <> True
    u_mr1_top.io.eof        <> eof_final

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

        val sphere = new Sphere(rtConfig)
        sphere.center.x := u_mr1_top.io.sphere_pos_x
        sphere.center.y := u_mr1_top.io.sphere_pos_y
        sphere.center.z := u_mr1_top.io.sphere_pos_z

        sphere.radius2.fromDouble(9.0)

        // Shadow rays are going the opposite way of the light
        val shadow_ray_direction = Vec3(rtConfig)

        {
            var light_dir_x = 1.0
            var light_dir_y = 2.0
            var light_dir_z = 0.5

            val length = scala.math.sqrt((light_dir_x * light_dir_x) + (light_dir_y * light_dir_y) + (light_dir_z * light_dir_z))

            light_dir_x = light_dir_x / length
            light_dir_y = light_dir_y / length
            light_dir_z = light_dir_z / length

            shadow_ray_direction.x.fromDouble(light_dir_x)
            shadow_ray_direction.y.fromDouble(light_dir_y)
            shadow_ray_direction.z.fromDouble(light_dir_z)
        }

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

        when(True){
            //ray.origin.x := RegNext(sphere.center.x)
            //ray.origin.y := RegNext(sphere.center.y)

            ray.origin.x := u_mr1_top.io.camera_pos_x
            ray.origin.y := u_mr1_top.io.camera_pos_y
            ray.origin.z := u_mr1_top.io.camera_pos_z
        }

        //============================================================
        // Tilt camera up or down
        //============================================================

        val rot_x_sin   = Fpxx(rtConfig.fpxxConfig)
        val rot_x_cos   = Fpxx(rtConfig.fpxxConfig)

        // 20 degrees
        if (false){
            rot_x_sin.fromDouble(0.33688985339222005)
            rot_x_cos.fromDouble(0.94154406518302081)
        }
        else{
            rot_x_sin := u_mr1_top.io.rot_x_sin
            rot_x_cos := u_mr1_top.io.rot_x_cos
        }

        val ray_dir_rot_x_vld   = Bool
        val ray_dir_rot_x       = Vec3(rtConfig)

        val u_ray_dir_rot_x = new RotateX(rtConfig, Constants.rotateHwMulConfig)
        u_ray_dir_rot_x.io.op_vld       <> cam_sweep_pixel.req
        u_ray_dir_rot_x.io.op           <> ray.direction
        u_ray_dir_rot_x.io.sin          <> rot_x_sin
        u_ray_dir_rot_x.io.cos          <> rot_x_cos

        u_ray_dir_rot_x.io.result_vld   <> ray_dir_rot_x_vld
        u_ray_dir_rot_x.io.result       <> ray_dir_rot_x

        //============================================================
        // Pan camera left or right
        //============================================================

        val rot_y_sin   = Fpxx(rtConfig.fpxxConfig)
        val rot_y_cos   = Fpxx(rtConfig.fpxxConfig)

        // 10 degrees
        if (false){
            rot_y_sin.fromDouble(0.17096188876030122)
            rot_y_cos.fromDouble(0.98527764238894122)
        }
        else{
            rot_y_sin := u_mr1_top.io.rot_y_sin
            rot_y_cos := u_mr1_top.io.rot_y_cos
        }

        val ray_dir_rot_y_vld   = Bool
        val ray_dir_rot_y       = Vec3(rtConfig)

        val u_ray_dir_rot_y = new RotateY(rtConfig, Constants.rotateHwMulConfig)
        u_ray_dir_rot_y.io.op_vld       <> ray_dir_rot_x_vld
        u_ray_dir_rot_y.io.op           <> ray_dir_rot_x
        u_ray_dir_rot_y.io.sin          <> rot_y_sin
        u_ray_dir_rot_y.io.cos          <> rot_y_cos

        u_ray_dir_rot_y.io.result_vld   <> ray_dir_rot_y_vld
        u_ray_dir_rot_y.io.result       <> ray_dir_rot_y


        //============================================================
        // ray_normalized
        //============================================================

        val ray_normalized_vld = Bool
        val ray_normalized = new Ray(rtConfig)

        ray_normalized.origin := ray.origin

        val u_normalize_ray = new Normalize(rtConfig)
        u_normalize_ray.io.op_vld      <> ray_dir_rot_y_vld
        u_normalize_ray.io.op          <> ray_dir_rot_y

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

        val checker_color_vld = plane_x_int_vld
        val checker_color     = plane_x_int(14) ^ plane_z_int(14) ^ plane_intersection_x_sign_delayed ^ plane_intersection_z_sign_delayed

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
        // sphere shadow
        //============================================================

        val shadow_ray = Ray(rtConfig)
        shadow_ray.origin       := plane_intersection
        shadow_ray.direction    := shadow_ray_direction

        val shadow_sphere_intersects_vld   = Bool
        val shadow_sphere_intersects       = Bool

        val u_shadow_sphere_intersect = new SphereIntersect(rtConfig)
        u_shadow_sphere_intersect.io.op_vld    <> plane_intersect_vld
        u_shadow_sphere_intersect.io.sphere    <> sphere
        u_shadow_sphere_intersect.io.ray       <> shadow_ray

        u_shadow_sphere_intersect.io.early_intersects_vld <> shadow_sphere_intersects_vld
        u_shadow_sphere_intersect.io.early_intersects     <> shadow_sphere_intersects

        //============================================================
        // sphere light spot
        //============================================================

        val spot_light_vld  = Bool
        val spot_light      = Fpxx(rtConfig.fpxxConfig)

        val u_dot_spot_light = new DotProduct(rtConfig, Constants.dotHwMulConfig)
        u_dot_spot_light.io.op_vld <> sphere_result_vld
        u_dot_spot_light.io.op_a   <> sphere_reflect_ray.direction
        u_dot_spot_light.io.op_b   <> shadow_ray_direction

        u_dot_spot_light.io.result_vld <> spot_light_vld
        u_dot_spot_light.io.result     <> spot_light

        val spot_light_int_full_vld = Bool
        val spot_light_int_full     = SInt(20 bits)

        val u_spot_light_int = new Fpxx2SInt(8, 12, rtConfig.fpxxConfig)
        u_spot_light_int.io.op_vld     <> spot_light_vld
        u_spot_light_int.io.op         <> spot_light

        u_spot_light_int.io.result_vld <> spot_light_int_full_vld
        u_spot_light_int.io.result     <> spot_light_int_full

        val spot_light_int_vld = spot_light_int_full_vld
        val spot_light_int     = RegNext(spot_light.sign) ? U(0, 9 bits) | spot_light_int_full(spot_light_int_full.getWidth-8-8, 9 bits).asUInt

        val spot_light_e2_vld  = RegNext(spot_light_int_vld)
        val spot_light_e2      = RegNext((spot_light_int * spot_light_int >> 8).resize(9))

        val spot_light_e4_vld  = RegNext(spot_light_e2_vld)
        val spot_light_e4      = RegNext((spot_light_e2 * spot_light_e2 >> 8).resize(9))

        val spot_light_e8_vld  = RegNext(spot_light_e4_vld)
        val spot_light_e8      = RegNext((spot_light_e4 * spot_light_e4 >> 8).resize(9))

        val spot_light_final_vld = spot_light_e8_vld
        val spot_light_final     = spot_light_e8.msb ? U(255, 8 bits) | spot_light_e8.resize(8)

        //============================================================
        // Final color
        //============================================================

        // Need 4 parameters to define final color:
        // plane_intersects_final, checker_color, shadow_sphere_intersects, sphere_intersects, spot_light, and cam_sweep_pixel

        val (plane_intersects_final_vld_delayed, plane_intersects_final_delayed, shadow_sphere_intersects_delayed_0) = MatchLatency(
                                                                plane_intersect_vld,
                                                                plane_x_int_vld,              plane_intersects_final,
                                                                shadow_sphere_intersects_vld, shadow_sphere_intersects)

        val (checker_color_delayed_vld, checker_color_delayed, shadow_sphere_intersects_delayed_1) = MatchLatency(
                                                                plane_intersect_vld,
                                                                checker_color_vld,            checker_color,
                                                                shadow_sphere_intersects_vld, shadow_sphere_intersects)

        val (sphere_result_delayed_vld, sphere_intersects_delayed, plane_x_int_delayed_2) = MatchLatency(
                                                                sphere_result_vld,
                                                                sphere_result_vld,                   sphere_intersects,
                                                                plane_intersects_final_vld_delayed,  plane_intersects_final_delayed)

//        val (spot_light_final_delayed_vld, spot_light_final_delayed, plane_intersects_final_delayed_0) = MatchLatency(
//                                                                ray_normalized_vld,
//                                                                spot_light_final_vld,                   spot_light_final_vld,
//                                                                plane_intersects_final_vld_delayed,  plane_intersects_final_delayed)

        val (spot_light_final_delayed_vld, spot_light_final_delayed) = MatchLatency(
                                                                ray_normalized_vld,
                                                                spot_light_final_vld, spot_light_final,
                                                                plane_intersects_final_vld_delayed)



        val (cam_sweep_pixel_delayed_vld, cam_sweep_pixel_delayed, plane_intersects_final_delayed_1) = MatchLatency(
                                                                cam_sweep_pixel.req,
                                                                cam_sweep_pixel.req, cam_sweep_pixel,
                                                                plane_intersects_final_vld_delayed,  plane_intersects_final_delayed)

        val sky_s    = (0.0, 0.0, 0.9)
        val red_s    = (0.9, 0.0, 0.0)
        val green_s  = (0.0, 0.9, 0.0)
        val yellow_s = (0.9, 0.9, 0.0)

        val shadow_factor = 0.5
        val shadow_red_s   = (red_s._1   * shadow_factor, red_s._2   * shadow_factor, red_s._3   * shadow_factor)
        val shadow_green_s = (green_s._1 * shadow_factor, green_s._2 * shadow_factor, green_s._3 * shadow_factor)

        val sky = Pixel()
        sky.setColor(sky_s._1, sky_s._2, sky_s._3)

        val yellow = Pixel()
        yellow.setColor(yellow_s._1,yellow_s._2, yellow_s._3)

        val red = Pixel()
        red.setColor(red_s._1,red_s._2, red_s._3)

        val green = Pixel()
        green.setColor(green_s._1,green_s._2, green_s._3)

        val shadow_red = Pixel()
        shadow_red.setColor(shadow_red_s._1,shadow_red_s._2, shadow_red_s._3)

        val shadow_green = Pixel()
        shadow_green.setColor(shadow_green_s._1,shadow_green_s._2, shadow_green_s._3)

        val sphere_alpha = 0.5

        val yellow_red = Pixel()
        yellow_red.setColor(
                yellow_s._1 * (1-sphere_alpha) + red_s._1 * sphere_alpha,
                yellow_s._2 * (1-sphere_alpha) + red_s._2 * sphere_alpha,
                yellow_s._3 * (1-sphere_alpha) + red_s._3 * sphere_alpha)

        val yellow_green = Pixel()
        yellow_green.setColor(
                yellow_s._1 * (1-sphere_alpha) + green_s._1 * sphere_alpha,
                yellow_s._2 * (1-sphere_alpha) + green_s._2 * sphere_alpha,
                yellow_s._3 * (1-sphere_alpha) + green_s._3 * sphere_alpha)

        val yellow_shadow_red = Pixel()
        yellow_shadow_red.setColor(
                yellow_s._1 * (1-sphere_alpha) + shadow_red_s._1 * sphere_alpha,
                yellow_s._2 * (1-sphere_alpha) + shadow_red_s._2 * sphere_alpha,
                yellow_s._3 * (1-sphere_alpha) + shadow_red_s._3 * sphere_alpha)

        val yellow_shadow_green = Pixel()
        yellow_shadow_green.setColor(
                yellow_s._1 * (1-sphere_alpha) + shadow_green_s._1 * sphere_alpha,
                yellow_s._1 * (1-sphere_alpha) + shadow_green_s._2 * sphere_alpha,
                yellow_s._1 * (1-sphere_alpha) + shadow_green_s._3 * sphere_alpha)

        val yellow_sky = Pixel()
        yellow_sky.setColor(
                yellow_s._1 * (1-sphere_alpha) + sky_s._1 * sphere_alpha,
                yellow_s._1 * (1-sphere_alpha) + sky_s._2 * sphere_alpha,
                yellow_s._1 * (1-sphere_alpha) + sky_s._3 * sphere_alpha)

        val rt_pixel = PixelStream()
        rt_pixel := cam_sweep_pixel_delayed


        when(plane_intersects_final_vld_delayed || sphere_result_delayed_vld || cam_sweep_pixel_delayed_vld || checker_color_delayed_vld) {

        val color = Pixel()

        when(sphere_intersects_delayed && !plane_intersects_final_delayed){
            color := yellow_sky
        }
        .elsewhen(sphere_intersects_delayed && plane_intersects_final_delayed){
            when(shadow_sphere_intersects){
                color := checker_color_delayed ? yellow_shadow_red | yellow_shadow_green
            }.
            otherwise{
                color := checker_color_delayed ? yellow_red | yellow_green
            }
        }


        when(sphere_intersects_delayed){

            val sphere_color = Pixel()

            when(!plane_intersects_final_delayed){
                sphere_color := yellow_sky
            }
            .otherwise{
                when(shadow_sphere_intersects){
                    sphere_color := checker_color_delayed ? yellow_shadow_red | yellow_shadow_green
                }
                .otherwise{
                    sphere_color := checker_color_delayed ? yellow_red | yellow_green
                }
            }

            val r = UInt(9 bits)
            val g = UInt(9 bits)
            val b = UInt(9 bits)

            r := sphere_color.r.resize(9) + (spot_light_final_delayed >> 1).resize(9)
            g := sphere_color.g.resize(9) + (spot_light_final_delayed >> 1).resize(9)
            b := sphere_color.b.resize(9) + (spot_light_final_delayed >> 1).resize(9)

            color.r := r.msb ? U(255, 8 bits) | r.resize(8)
            color.g := g.msb ? U(255, 8 bits) | g.resize(8)
            color.b := b.msb ? U(255, 8 bits) | b.resize(8)
        }
        .elsewhen(plane_intersects_final_delayed){
            when(shadow_sphere_intersects){
                color := checker_color_delayed ? shadow_red | shadow_green
            }.
            otherwise{
                color := checker_color_delayed ? red | green
            }
        }
        .otherwise{
            color:= sky
        }

        rt_pixel.pixel := color

        }

        eof_final := rt_pixel.eof
    }


    val vo = new VideoOut()
    vo.io.timings := timings
    vo.io.pixel_in <> rt.rt_pixel
    vo.io.vga_out <> io.vo
}


