

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

        val early_intersects_vld = out(Bool)
        val early_intersects     = out(Bool)

        val result_vld          = out(Bool)
        val result_intersects   = out(Bool)
        val result_t            = out(Fpxx(c.fpxxConfig))
        val result_intersection = out(Vec3(c))
        val result_normal       = out(Vec3(c))
        val result_reflect_ray  = out(Ray(c))
        var result_ray          = out(Ray(c))
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

    val intersects_tca_vld = tca_vld
    val intersects_tca = !tca.sign && !tca.is_nan() && !tca.is_infinite()

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

    val u_tca_tca = new FpxxMul(c.fpxxConfig, Constants.fpxxMulConfig)
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

    val u_d2 = new FpxxSub(c.fpxxConfig, Constants.fpxxAddConfig)
    u_d2.io.op_vld <> tca_tca_delayed_vld
    u_d2.io.op_a   <> c0r0_c0r0_delayed
    u_d2.io.op_b   <> tca_tca_delayed

    u_d2.io.result_vld <> d2_vld
    u_d2.io.result     <> d2

    val (intersects_tca_delayed_early_vld, intersects_tca_delayed_early, d2_delayed) = MatchLatency(
                                                        io.op_vld,
                                                        intersects_tca_vld,    intersects_tca,
                                                        d2_vld,                d2)

    val radius2_ge_d2 = (io.sphere.radius2.toVec().asSInt >= d2.toVec().asSInt)

    io.early_intersects_vld := RegNext(d2_vld)
    io.early_intersects     := RegNext(radius2_ge_d2 && intersects_tca_delayed_early)

    //============================================================
    // radius2_m_d2
    //============================================================

    val radius2_m_d2_vld = Bool
    val radius2_m_d2     = Fpxx(c.fpxxConfig)

    val u_radius2_m_d2 = new FpxxSub(c.fpxxConfig, Constants.fpxxAddConfig)
    u_radius2_m_d2.io.op_vld <> d2_vld
    u_radius2_m_d2.io.op_a   <> io.sphere.radius2           // Static parameter. No need for latency matching.
    u_radius2_m_d2.io.op_b   <> d2

    u_radius2_m_d2.io.result_vld <> radius2_m_d2_vld
    u_radius2_m_d2.io.result     <> radius2_m_d2

    val intersects_d2_vld = radius2_m_d2_vld
    val intersects_d2     = !radius2_m_d2.sign && !radius2_m_d2.is_nan() && !radius2_m_d2.is_infinite()

    //============================================================
    // thc
    //============================================================

    val thc_vld = Bool
    val thc     = Fpxx(c.fpxxConfig)

    val u_thc = new FpxxSqrt(c.fpxxConfig, Constants.fpxxSqrtConfig)
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

    val u_t0 = new FpxxSub(c.fpxxConfig, Constants.fpxxAddConfig)
    u_t0.io.op_vld <> tca_delayed_vld
    u_t0.io.op_a   <> tca_delayed
    u_t0.io.op_b   <> thc_delayed

    u_t0.io.result_vld <> t0_vld
    u_t0.io.result     <> t0

    val t1_vld = Bool
    val t1     = Fpxx(c.fpxxConfig)

    val u_t1 = new FpxxAdd(c.fpxxConfig, Constants.fpxxAddConfig)
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

    //============================================================
    // normal_raw
    //============================================================

    val (center_delayed_vld, center_delayed, intersection_delayed_0) = MatchLatency(
                                                        io.op_vld,
                                                        io.op_vld,        io.sphere.center,
                                                        intersection_vld, intersection)

    val normal_raw_vld = Bool
    val normal_raw     = Vec3(c)

    val u_normal_raw = new SubVecVec(c)
    u_normal_raw.io.op_vld        <> center_delayed_vld
    u_normal_raw.io.op_a          <> center_delayed
    u_normal_raw.io.op_b          <> intersection

    u_normal_raw.io.result_vld    <> normal_raw_vld
    u_normal_raw.io.result        <> normal_raw

    //============================================================
    // normal
    //============================================================

    val normal_vld = Bool
    val normal     = Vec3(c)

    val u_normalize = new Normalize(c)
    u_normalize.io.op_vld      <> normal_raw_vld
    u_normalize.io.op          <> normal_raw

    u_normalize.io.result_vld  <> normal_vld
    u_normalize.io.result      <> normal

    //============================================================
    // dir_dot_normal
    //============================================================

    val (dir_delayed_vld_dot_normal, dir_delayed_dot_normal, normal_delayed_0) = MatchLatency(
                                                        io.op_vld,
                                                        dir_delayed_vld_intersect, dir_delayed_intersect,
                                                        normal_vld,                normal)

    val dir_dot_normal_vld = Bool
    val dir_dot_normal     = Fpxx(c.fpxxConfig)

    val u_dir_dot_normal = new DotProduct(c)
    u_dir_dot_normal.io.op_vld     <> normal_vld
    u_dir_dot_normal.io.op_a       <> normal
    u_dir_dot_normal.io.op_b       <> dir_delayed_dot_normal

    u_dir_dot_normal.io.result_vld <> dir_dot_normal_vld
    u_dir_dot_normal.io.result     <> dir_dot_normal

    //============================================================
    // dir_dot_normal_x2
    //============================================================

    val dir_dot_normal_x2_vld = Bool
    val dir_dot_normal_x2     = Fpxx(c.fpxxConfig)

    dir_dot_normal_x2_vld   := RegNext(dir_dot_normal_vld)
    dir_dot_normal_x2.sign  := RegNext(dir_dot_normal.sign)
    dir_dot_normal_x2.exp   := RegNext(dir_dot_normal.exp) + 1
    dir_dot_normal_x2.mant  := RegNext(dir_dot_normal.mant)

    //============================================================
    // dir_mirror
    //============================================================

    val (normal_delayed_vld_dir_mirror, normal_delayed_dir_mirror, dir_dot_normal_x2_delayed) = MatchLatency(
                                                        io.op_vld,
                                                        normal_vld,            normal,
                                                        dir_dot_normal_x2_vld, dir_dot_normal_x2)

    val dir_mirror_vld = Bool
    val dir_mirror     = Vec3(c)

    val u_dir_mirror = new MulVecScalar(c)
    u_dir_mirror.io.op_vld     <> normal_delayed_vld_dir_mirror
    u_dir_mirror.io.op_vec     <> normal_delayed_dir_mirror
    u_dir_mirror.io.op_scalar  <> dir_dot_normal_x2

    u_dir_mirror.io.result_vld <> dir_mirror_vld
    u_dir_mirror.io.result     <> dir_mirror

    //============================================================
    // reflect_ray_dir
    //============================================================

    val (dir_delayed_vld_reflect_dir, dir_delayed_reflect_dir, dir_mirror_delayed_0) = MatchLatency(
                                                        io.op_vld,
                                                        dir_delayed_vld_dot_normal, dir_delayed_dot_normal,
                                                        dir_mirror_vld,             dir_mirror)


    val reflect_dir_vld = Bool
    val reflect_dir     = Vec3(c)

    val u_reflect_dir = new SubVecVec(c)
    u_reflect_dir.io.op_vld        <> dir_delayed_vld_reflect_dir
    u_reflect_dir.io.op_a          <> dir_delayed_reflect_dir
    u_reflect_dir.io.op_b          <> dir_mirror

    u_reflect_dir.io.result_vld    <> reflect_dir_vld
    u_reflect_dir.io.result        <> reflect_dir

    //============================================================
    // result
    //============================================================

    val (intersects_tca_delayed_vld, intersects_tca_delayed, reflect_dir_delayed_0) = MatchLatency(
                                                        io.op_vld,
                                                        intersects_tca_vld,             intersects_tca,
                                                        reflect_dir_vld,                reflect_dir)

    val (intersects_d2_delayed_vld, intersects_d2_delayed, reflect_dir_delayed_1) = MatchLatency(
                                                        io.op_vld,
                                                        intersects_d2_vld,              intersects_d2,
                                                        reflect_dir_vld,                reflect_dir)

    val (t_delayed_vld, t_delayed, reflect_dir_delayed_2) = MatchLatency(
                                                        io.op_vld,
                                                        t_vld,                          t,
                                                        reflect_dir_vld,                reflect_dir)

    val (intersection_delayed_vld, intersection_delayed, reflect_dir_delayed_3) = MatchLatency(
                                                        io.op_vld,
                                                        intersection_vld,               intersection,
                                                        reflect_dir_vld,                reflect_dir)

    val (normal_delayed_vld_result, normal_delayed_result, reflect_dir_delayed_4) = MatchLatency(
                                                        io.op_vld,
                                                        normal_delayed_vld_dir_mirror,  normal_delayed_dir_mirror,
                                                        reflect_dir_vld,                reflect_dir)

    val (origin_delayed_vld_result, origin_delayed_result, reflect_dir_delayed_5) = MatchLatency(
                                                        io.op_vld,
                                                        origin_delayed_vld_intersect,   origin_delayed_intersect,
                                                        reflect_dir_vld,                reflect_dir)

    val (dir_delayed_vld_result, dir_delayed_result, reflect_dir_delayed_6) = MatchLatency(
                                                        io.op_vld,
                                                        dir_delayed_vld_reflect_dir,    dir_delayed_reflect_dir,
                                                        reflect_dir_vld,                reflect_dir)

    val intersects_delayed = intersects_tca_delayed && intersects_d2_delayed

    val reflect_ray = Ray(c)
    reflect_ray.origin    := intersection_delayed
    reflect_ray.direction := reflect_dir

    val ray_delayed = Ray(c)
    ray_delayed.origin    := origin_delayed_result
    ray_delayed.direction := dir_delayed_result

    io.result_vld          <> reflect_dir_vld
    io.result_intersects   <> intersects_delayed
    io.result_t            <> t_delayed
    io.result_intersection <> intersection_delayed
    io.result_normal       <> normal_delayed_result
    io.result_reflect_ray  <> reflect_ray
    io.result_ray          <> ray_delayed

}


