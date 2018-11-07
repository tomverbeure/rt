

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
        val op_vld          = in(Bool)
        val sphere          = in(Sphere(c))
        val ray             = in(Ray(c))

        val result_vld          = out(Bool)
        val result_intersects   = out(Bool)
        val result_t            = out(Fpxx(c.fpxxConfig))
        val result_intersection = out(Vec3(c))
        val result_normal       = out(Vec3(c))
        val result_reflect_ray  = out(Ray(c))
    }

    //============================================================
    // c0r0
    //============================================================

    val c0r0_vld = Bool
    val c0r0     = Vec3(c)

    val u_c0r0 = new SubVecVec(c)
    u_c0r0.io.op_vld        <> io.op_vld
    u_c0r0.io.op_a          <> io.sphere.center
    u_c0r0.io.op_b          <> io.ray.origin

    u_c0r0.io.result_vld    <> c0r0_vld
    u_c0r0.io.result        <> c0r0

    //============================================================
    // tca
    //============================================================

    val c0r0_latency = LatencyAnalysis(io.op_vld, c0r0_vld)
    val dir_delayed_c0r0 = Delay(io.ray.direction, cycleCount = c0r0_latency)

    val tca_vld = Bool
    val tca     = Fpxx(c.fpxxConfig)

    val u_dot_tca = new DotProduct(c)
    u_dot_tca.io.op_vld     <> c0r0_vld
    u_dot_tca.io.op_a       <> c0r0
    u_dot_tca.io.op_b       <> dir_delayed_c0r0

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

    val (tca_tca_delayed_vld, tca_tca_delayed, c0r0_c0r0_delayed) = MatchLatency(
                                                                        io.op_vld,
                                                                        tca_tca_vld,   tca_tca,
                                                                        c0r0_c0r0_vld, c0r0_c0r0)

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
    u_radius2_m_d2.io.op_a   <> io.sphere.radius2           // Static parameter. No need for latency matching.
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

    //============================================================
    // t0, t1
    //============================================================

    val (tca_delayed_vld, tca_delayed, thc_delayed) = MatchLatency(
                                                        io.op_vld,
                                                        tca_vld, tca,
                                                        thc_vld, thc)

    val t0_vld = Bool
    val t0     = Fpxx(c.fpxxConfig)

    val u_t0 = new FpxxSub(c.fpxxConfig, pipeStages = 5)
    u_t0.io.op_vld <> tca_delayed_vld
    u_t0.io.op_a   <> tca_delayed
    u_t0.io.op_b   <> thc_delayed

    u_t0.io.result_vld <> t0_vld
    u_t0.io.result     <> t0

    val t1_vld = Bool
    val t1     = Fpxx(c.fpxxConfig)

    val u_t1 = new FpxxAdd(c.fpxxConfig, pipeStages = 5)
    u_t1.io.op_vld <> tca_delayed_vld
    u_t1.io.op_a   <> tca_delayed
    u_t1.io.op_b   <> thc_delayed

    u_t1.io.result_vld <> t1_vld
    u_t1.io.result     <> t1

    //============================================================
    // t
    //============================================================

    val t_vld = Bool
    val t = Fpxx(c.fpxxConfig)

    t_vld := RegNext(t0_vld)
    t     := RegNext((t1.toVec().asSInt < t0.toVec().asSInt) ? t1 | t0)

    //============================================================
    // intersection
    //============================================================

    val (dir_delayed_vld_intersect, dir_delayed_intersect, t_delayed_0) = MatchLatency(
                                                        io.op_vld,
                                                        c0r0_vld, dir_delayed_c0r0,
                                                        t_vld,    t)

    val (origin_delayed_vld_intersect, origin_delayed_intersect, t_delayed_1) = MatchLatency(
                                                        io.op_vld,
                                                        io.op_vld, io.ray.origin,
                                                        t_vld,    t)


    val intersection_vld = Bool
    val intersection     = Vec3(c)

    val u_intersection = new Intersection(c)
    u_intersection.io.op_vld     <> t_vld
    u_intersection.io.ray_origin <> origin_delayed_intersect
    u_intersection.io.ray_dir    <> dir_delayed_intersect
    u_intersection.io.t          <> t

    u_intersection.io.result_vld <> intersection_vld
    u_intersection.io.result     <> intersection

}


