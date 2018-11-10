

package rt

import spinal.core._

import spinal.lib.LatencyAnalysis
import spinal.lib.Delay

import math._

case class Plane(c: RTConfig) extends Bundle {
    val origin  = Vec3(c)
    val normal  = Vec3(c)
}


class PlaneIntersect(c: RTConfig) extends Component {

    val io = new Bundle {
        val op_vld          = in(Bool)
        val plane           = in(Plane(c))
        val ray             = in(Ray(c))

        val result_vld          = out(Bool)
        val result_intersects   = out(Bool)
        val result_t            = out(Fpxx(c.fpxxConfig))
        val result_intersection = out(Vec3(c))
    }

    //============================================================
    // denom
    //============================================================

    val denom_vld = Bool
    val denom = Fpxx(c.fpxxConfig)

    val u_dot_norm_dir = new DotProduct(c)
    u_dot_norm_dir.io.op_vld     <> io.op_vld
    u_dot_norm_dir.io.op_a       <> io.plane.normal
    u_dot_norm_dir.io.op_b       <> io.ray.direction

    u_dot_norm_dir.io.result_vld <> denom_vld
    u_dot_norm_dir.io.result     <> denom

    // Some random very small number
    val intersects_par_vld = RegNext(denom_vld)
    val intersects_par     = (RegNext(denom.exp) >= 3)

    //============================================================
    // p0r0
    //============================================================

    val p0r0_vld = Bool
    val p0r0 = new Vec3(c)

    val u_p0r0 = new SubVecVec(c)
    u_p0r0.io.op_vld        <> io.op_vld
    u_p0r0.io.op_a          <> io.plane.origin
    u_p0r0.io.op_b          <> io.ray.origin

    u_p0r0.io.result_vld    <> p0r0_vld
    u_p0r0.io.result        <> p0r0

    //============================================================
    // p0r0_dot_norm
    //============================================================

    val p0r0_latency = LatencyAnalysis(io.op_vld, p0r0_vld)
    val plane_normal_delayed = Delay(io.plane.normal, cycleCount = p0r0_latency)

    val p0r0_dot_norm_vld = Bool
    val p0r0_dot_norm = Fpxx(c.fpxxConfig)

    val u_dot_p0r0_norm = new DotProduct(c)
    u_dot_p0r0_norm.io.op_vld     <> p0r0_vld
    u_dot_p0r0_norm.io.op_a       <> p0r0
    u_dot_p0r0_norm.io.op_b       <> plane_normal_delayed

    u_dot_p0r0_norm.io.result_vld <> p0r0_dot_norm_vld
    u_dot_p0r0_norm.io.result     <> p0r0_dot_norm

    //============================================================
    // t
    //============================================================

    val p0r0_dot_norm_latency = LatencyAnalysis(io.op_vld, p0r0_dot_norm_vld)
    val denom_latency         = LatencyAnalysis(io.op_vld, denom_vld)

    val p0r0_dot_norm_delayed = Fpxx(c.fpxxConfig)
    val denom_delayed         = Fpxx(c.fpxxConfig)

    val denom_delayed_vld     = Bool

    if (p0r0_dot_norm_latency > denom_latency){
        p0r0_dot_norm_delayed := p0r0_dot_norm
        denom_delayed         := Delay(denom, cycleCount = p0r0_dot_norm_latency - denom_latency)
        denom_delayed_vld     := p0r0_dot_norm_vld
    }
    else if (p0r0_dot_norm_latency < denom_latency){
        p0r0_dot_norm_delayed := Delay(p0r0_dot_norm, cycleCount = denom_latency - p0r0_dot_norm_latency)
        denom_delayed         := denom
        denom_delayed_vld     := denom_vld
    }
    else{
        p0r0_dot_norm_delayed := p0r0_dot_norm
        denom_delayed         := denom
        denom_delayed_vld     := denom_vld
    }

    val t_vld = Bool
    val t = Fpxx(c.fpxxConfig)

    val u_div_p0r0_dot_norm_denom = new FpxxDiv(c.fpxxConfig, FpxxDivConfig(pipeStages = 5))
    u_div_p0r0_dot_norm_denom.io.op_vld     <> denom_delayed_vld
    u_div_p0r0_dot_norm_denom.io.op_a       <> p0r0_dot_norm_delayed
    u_div_p0r0_dot_norm_denom.io.op_b       <> denom_delayed

    u_div_p0r0_dot_norm_denom.io.result_vld <> t_vld
    u_div_p0r0_dot_norm_denom.io.result     <> t

    val t_vld_p1 = RegNext(t_vld)
    val t_p1 = RegNext(t)

    //============================================================
    // intersects t>=0
    //============================================================

    val (intersects_par_delayed_vld, intersects_par_delayed, t_p1_delayed) = MatchLatency(
                                                        io.op_vld,
                                                        intersects_par_vld, intersects_par,
                                                        t_vld_p1, t_p1)

    val intersects_t_gt0_vld = t_vld_p1
    val intersects_t_gt0     = intersects_par_delayed && (!t_p1.sign && !t_p1.is_nan() && !t_p1.is_infinite())

    //============================================================
    // intersection
    //============================================================

    val (dir_delayed_vld, dir_delayed, t_delayed_0) = MatchLatency(
                                                        io.op_vld,
                                                        io.op_vld, io.ray.direction,
                                                        t_vld,    t)

    val (origin_delayed_vld, origin_delayed, t_delayed_1) = MatchLatency(
                                                        io.op_vld,
                                                        io.op_vld, io.ray.origin,
                                                        t_vld,    t)

    val intersection_vld = Bool
    val intersection     = Vec3(c)

    val u_intersection = new Intersection(c)
    u_intersection.io.op_vld     <> t_vld
    u_intersection.io.ray_origin <> origin_delayed
    u_intersection.io.ray_dir    <> dir_delayed
    u_intersection.io.t          <> t

    u_intersection.io.result_vld <> intersection_vld
    u_intersection.io.result     <> intersection


    //============================================================
    // result
    //============================================================

    val (t_delayed_vld, t_delayed, intersection_delayed_0) = MatchLatency(
                                                        io.op_vld,
                                                        t_vld, t,
                                                        intersection_vld, intersection)

    val (intersects_t_gt0_delayed_vld, intersects_t_gt0_delayed, intersection_delayed_1) = MatchLatency(
                                                        io.op_vld,
                                                        intersects_t_gt0_vld, intersects_t_gt0,
                                                        intersection_vld, intersection)


    io.result_vld           := intersection_vld
    io.result_intersects    := intersects_t_gt0_delayed
    io.result_t             := t_delayed
    io.result_intersection  := intersection
}



