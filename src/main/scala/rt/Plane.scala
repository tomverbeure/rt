

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
        val plane_vld       = in(Bool)
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
    u_dot_norm_dir.io.op_vld     <> io.plane_vld
    u_dot_norm_dir.io.op_a       <> io.plane.normal
    u_dot_norm_dir.io.op_b       <> io.ray.direction

    u_dot_norm_dir.io.result_vld <> denom_vld
    u_dot_norm_dir.io.result     <> denom

    //============================================================
    // p0r0
    //============================================================

    val p0r0_vld = Bool
    val p0r0 = new Vec3(c)

    val u_p0r0 = new SubVecVec(c)
    u_p0r0.io.op_vld        <> io.plane_vld
    u_p0r0.io.op_a          <> io.plane.origin
    u_p0r0.io.op_b          <> io.ray.origin

    u_p0r0.io.result_vld    <> p0r0_vld
    u_p0r0.io.result        <> p0r0

    //============================================================
    // p0r0_dot_norm
    //============================================================

    val p0r0_latency = LatencyAnalysis(io.plane_vld, p0r0_vld)
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

    val p0r0_dot_norm_latency = LatencyAnalysis(io.plane_vld, p0r0_dot_norm_vld)
    val denom_latency         = LatencyAnalysis(io.plane_vld, denom_vld)

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

    //============================================================
    // t_mul_dir
    //============================================================

    val t_latency = LatencyAnalysis(io.plane_vld, t_vld)
    val dir_delayed = Delay(io.ray.direction, cycleCount = t_latency)

    val t_mul_dir_vld = Bool
    val t_mul_dir = Vec3(c)

    val u_mul_t_dir = new MulVecScalar(c)
    u_mul_t_dir.io.op_vld       <> t_vld
    u_mul_t_dir.io.op_vec       <> dir_delayed
    u_mul_t_dir.io.op_scalar    <> t

    u_mul_t_dir.io.result_vld   <> t_mul_dir_vld
    u_mul_t_dir.io.result       <> t_mul_dir

    //============================================================
    // intersection
    //============================================================

    val t_mul_dir_latency = LatencyAnalysis(io.plane_vld, t_mul_dir_vld)
    val ray_origin_delayed = Delay(io.ray.origin, cycleCount = t_mul_dir_latency)

    val intersection_vld = Bool
    val intersection = Vec3(c)

    val u_add_origin_t_mul_dir = new AddVecVec(c)
    u_add_origin_t_mul_dir.io.op_vld        <> t_mul_dir_vld
    u_add_origin_t_mul_dir.io.op_a          <> t_mul_dir
    u_add_origin_t_mul_dir.io.op_b          <> ray_origin_delayed

    u_add_origin_t_mul_dir.io.result_vld    <> intersection_vld
    u_add_origin_t_mul_dir.io.result        <> intersection

    //============================================================
    // result
    //============================================================

    val intersection_latency = LatencyAnalysis(io.plane_vld, intersection_vld)
    val t_delayed = Delay(t, cycleCount = (intersection_latency - t_latency))

    io.result_vld           := intersection_vld
    io.result_intersects    := True
    io.result_t             := t_delayed
    io.result_intersection  := intersection
}



