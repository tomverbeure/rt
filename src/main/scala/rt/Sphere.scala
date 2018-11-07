

package rt

import spinal.core._

import spinal.lib.LatencyAnalysis
import spinal.lib.Delay

import math._

case class Sphere(c: RTConfig) extends Bundle {
    val center  = Vec3(c)
    val radius2 = Fpxx(c.fpxxConfig)
}

class SphereIntersect(c: RTConfig) extends Component {

    val io = new Bundle {
        val sphere_vld      = in(Bool)
        val sphere          = in(Sphere(c))
        val ray             = in(Ray(c))

        val result_vld          = out(Bool)
        val result_intersects   = out(Bool)
        val result_t            = out(Fpxx(c.fpxxConfig))
        val result_intersection = out(Vec3(c))
        val result_normal       = out(Vec3(c))
        val result_reflec_ray   = out(Ray(c))
    }

    //============================================================
    // c0r0
    //============================================================

    val c0r0_vld = Bool
    val c0r0     = Vec3(c)

    val u_c0r0 = new SubVecVec(c)
    u_c0r0.io.op_vld        <> io.sphere_vld
    u_c0r0.io.op_a          <> io.sphere.center
    u_c0r0.io.op_b          <> io.ray.origin

    u_c0r0.io.result_vld    <> c0r0_vld
    u_c0r0.io.result        <> c0r0

    //============================================================
    // tca
    //============================================================

    val c0r0_latency = LatencyAnalysis(io.sphere_vld, c0r0_vld)
    val ray_dir_delayed_c0r0 = Delay(io.ray.direction, cycleCount = c0r0_latency)

    val tca_vld = Bool
    val tca     = Fpxx(c.fpxxConfig)

    val u_dot_tca = new DotProduct(c)
    u_dot_tca.io.op_vld     <> c0r0_vld
    u_dot_tca.io.op_a       <> c0r0
    u_dot_tca.io.op_b       <> ray_dir_delayed_c0r0

    u_dot_tca.io.result_vld <> tca_vld
    u_dot_tca.io.result     <> tca

    val intersects_tca = !tca.sign

    //============================================================
    // c0r0_c0r0
    //============================================================

    val c0r0_c0r0_vld = Bool
    val c0r0_c0r0     = Fpxx(c.fpxxConfig)

    val u_dot_c0r0_c0r0 = new DotProduct(c)
    u_dot_c0r0_c0r0.io.op_vld     <> c0r0_vld
    u_dot_c0r0_c0r0.io.op_a       <> c0r0
    u_dot_c0r0_c0r0.io.op_b       <> c0r0

    u_dot_c0r0_c0r0.io.result_vld <> c0r0_c0r0_vld
    u_dot_c0r0_c0r0.io.result     <> c0r0_c0r0

    //============================================================
    // tca_tca
    //============================================================

    val tca_tca_vld = Bool
    val tca_tca     = Fpxx(c.fpxxConfig)

    val u_tca_tca = new FpxxMul(c.fpxxConfig, pipeStages = 5)
    u_tca_tca.io.op_vld <> tca_vld
    u_tca_tca.io.op_a   <> tca
    u_tca_tca.io.op_b   <> tca

    u_tca_tca.io.result_vld <> tca_tca_vld
    u_tca_tca.io.result     <> tca_tca

    //============================================================
    // d2
    //============================================================

    val tca_tca_latency   = LatencyAnalysis(io.sphere_vld, tca_tca_vld)
    val c0r0_c0r0_latency = LatencyAnalysis(io.sphere_vld, c0r0_c0r0_vld)

    val tca_tca_delayed_vld = Bool
    val tca_tca_delayed     = Fpxx(c.fpxxConfig)
    val c0r0_c0r0_delayed   = Fpxx(c.fpxxConfig)

    if (tca_tca_latency < c0r0_c0r0_latency){
        tca_tca_delayed_vld := c0r0_c0r0_vld
        tca_tca_delayed     := Delay(tca_tca, cycleCount = c0r0_c0r0_latency - tca_tca_latency)
        c0r0_c0r0_delayed   := c0r0_c0r0
    }
    else if (tca_tca_latency > c0r0_c0r0_latency){
        tca_tca_delayed_vld := tca_tca_delayed_vld
        tca_tca_delayed     := tca_tca
        c0r0_c0r0_delayed   := Delay(c0r0_c0r0, cycleCount = tca_tca_latency - c0r0_c0r0_latency)
    }
    else {
        tca_tca_delayed_vld := tca_tca_delayed_vld
        tca_tca_delayed     := tca_tca
        c0r0_c0r0_delayed   := c0r0_c0r0
    }

    val d2_vld = Bool
    val d2     = Fpxx(c.fpxxConfig)

    val u_d2 = new FpxxSub(c.fpxxConfig, pipeStages = 5)
    u_d2.io.op_vld <> tca_tca_delayed_vld
    u_d2.io.op_a   <> tca_tca_delayed
    u_d2.io.op_b   <> c0r0_c0r0_delayed

    u_d2.io.result_vld <> d2_vld
    u_d2.io.result     <> d2

    val intersects_d2 = (d2.toVec().asSInt <= io.sphere.radius2.toVec().asSInt)

    //============================================================
    // radius2_m_d2
    //============================================================

    val radius2_m_d2_vld = Bool
    val radius2_m_d2     = Fpxx(c.fpxxConfig)

    val u_radius2_m_d2 = new FpxxSub(c.fpxxConfig, pipeStages = 5)
    u_radius2_m_d2.io.op_vld <> d2_vld
    u_radius2_m_d2.io.op_a   <> io.sphere.radius2
    u_radius2_m_d2.io.op_b   <> d2

    u_radius2_m_d2.io.result_vld <> radius2_m_d2_vld
    u_radius2_m_d2.io.result     <> radius2_m_d2

    //============================================================
    // thc
    //============================================================

    val thc_vld = Bool
    val thc     = Fpxx(c.fpxxConfig)

    val u_thc = new FpxxSqrt(c.fpxxConfig, FpxxSqrtConfig(pipeStages = 5))
    u_thc.io.op_vld <> radius2_m_d2_vld
    u_thc.io.op     <> radius2_m_d2

    u_thc.io.result_vld <> thc_vld
    u_thc.io.result     <> thc

}
