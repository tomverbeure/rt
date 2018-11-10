
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

    val vi_gen = new VideoTimingGen()
    vi_gen.io.timings := timings

    val rt = new Area {
        val rtConfig = RTConfig()

        val cam_sweep_pixel = PixelStream()
        val ray             = Ray(rtConfig)

        val u_cam_sweep = new CamSweep(rtConfig)
        u_cam_sweep.io.pixel_in     <> vi_gen.io.pixel_out
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

        sphere.center.x.fromDouble(3.0)
        sphere.center.y.fromDouble(10.0)
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

//        io.led_blue := plane_intersect_vld |
//                       plane_intersects |
//                       plane_intersect_t.toVec().orR |
//                       plane_intersection.x.toVec().orR |
//                       plane_intersection.y.toVec().orR |
//                       plane_intersection.z.toVec().orR

        //============================================================
        // Final color
        //============================================================

        val (sphere_result_delayed_vld, sphere_intersects_delayed, plane_intersects_delayed_0) = MatchLatency(
                                                                sphere_result_vld,
                                                                sphere_result_vld,   sphere_intersects,
                                                                plane_intersect_vld, plane_intersects)

        val (cam_sweep_pixel_delayed_vld, cam_sweep_pixel_delayed, plane_intersects_delayed_1) = MatchLatency(
                                                                cam_sweep_pixel.req,
                                                                cam_sweep_pixel.req, cam_sweep_pixel,
                                                                plane_intersect_vld, plane_intersects)


        var red = Pixel()
        red.setColor(1.0, 0.0, 0.0)

        var green = Pixel()
        green.setColor(0.0, 1.0, 0.0)

        var blue = Pixel()
        blue.setColor(0.0, 0.0, 1.0)

        var yellow = Pixel()
        yellow.setColor(1.0, 1.0, 0.0)

        var cyan = Pixel()
        cyan.setColor(0.0, 1.0, 1.0)

        val rt_pixel = PixelStream()
        rt_pixel := cam_sweep_pixel_delayed
        when(sphere_intersects_delayed && !plane_intersects){
            rt_pixel.pixel := yellow
        }
        .elsewhen(sphere_intersects_delayed && plane_intersects){
            rt_pixel.pixel := cyan
        }
        .elsewhen(plane_intersects){
            rt_pixel.pixel := green
        }
        .otherwise{
            rt_pixel.pixel := blue
        }

    }

    val vo = new VideoOut()
    vo.io.timings := timings
    vo.io.pixel_in <> rt.rt_pixel
    vo.io.vga_out <> io.vo

    io.led_blue := True
}


