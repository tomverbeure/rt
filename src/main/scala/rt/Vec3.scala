
package rt

import spinal.core._

import spinal.lib.LatencyAnalysis
import spinal.lib.Delay

import math._

//object Vec3 {
//    def apply(c: RTConfig) : Vec3 = Vec3(c)
//}

case class Vec3(c: RTConfig) extends Bundle {
    val x   = Fpxx(c.fpxxConfig)
    val y   = Fpxx(c.fpxxConfig)
    val z   = Fpxx(c.fpxxConfig)

    def init() : Vec3 = {
        x init()
        y init()
        z init()

        this
    }
}

case class DotProductConfig(
    hwMul           : Boolean = false
    ){
}


class DotProduct(c: RTConfig, dotConfig : DotProductConfig = null) extends Component {

    def hwMul = if (dotConfig == null) false else dotConfig.hwMul

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op_a        = in(Vec3(c))
        val op_b        = in(Vec3(c))

        val result_vld  = out(Bool)
        val result      = out(Fpxx(c.fpxxConfig))
    }

    val xx_vld = Bool
    val yy_vld = Bool
    val zz_vld = Bool

    val xx  = Fpxx(c.fpxxConfig)
    val yy  = Fpxx(c.fpxxConfig)
    val zz  = Fpxx(c.fpxxConfig)

    val u_xx = new FpxxMul(c.fpxxConfig, if (hwMul) Constants.fpxxHwMulConfig else Constants.fpxxMulConfig)
    u_xx.io.op_vld := io.op_vld
    u_xx.io.op_a   := io.op_a.x
    u_xx.io.op_b   := io.op_b.x

    xx_vld := u_xx.io.result_vld
    xx := u_xx.io.result

    val u_yy = new FpxxMul(c.fpxxConfig, if (hwMul) Constants.fpxxHwMulConfig else Constants.fpxxMulConfig)
    u_yy.io.op_vld := io.op_vld
    u_yy.io.op_a   := io.op_a.y
    u_yy.io.op_b   := io.op_b.y

    yy_vld := u_yy.io.result_vld
    yy := u_yy.io.result

    val u_zz = new FpxxMul(c.fpxxConfig, if (hwMul) Constants.fpxxHwMulConfig else Constants.fpxxMulConfig)
    u_zz.io.op_vld := io.op_vld
    u_zz.io.op_a   := io.op_a.z
    u_zz.io.op_b   := io.op_b.z

    zz_vld := u_zz.io.result_vld
    zz := u_zz.io.result

    val xx_yy_vld     = Bool
    val xx_yy_zz_vld  = Bool

    val xx_yy     = Fpxx(c.fpxxConfig)
    val xx_yy_zz  = Fpxx(c.fpxxConfig)

    val u_xx_yy = new FpxxAdd(c.fpxxConfig, Constants.fpxxAddConfig)
    u_xx_yy.io.op_vld := xx_vld
    u_xx_yy.io.op_a   := xx
    u_xx_yy.io.op_b   := yy

    xx_yy_vld := u_xx_yy.io.result_vld
    xx_yy := u_xx_yy.io.result

    val xx_yy_latency = LatencyAnalysis(xx_vld, xx_yy_vld)
    val zz_delayed    = Delay(zz, cycleCount = xx_yy_latency)

    val u_xx_yy_zz = new FpxxAdd(c.fpxxConfig, Constants.fpxxAddConfig)
    u_xx_yy_zz.io.op_vld := xx_yy_vld
    u_xx_yy_zz.io.op_a   := xx_yy
    u_xx_yy_zz.io.op_b   := zz_delayed

    xx_yy_zz_vld := u_xx_yy_zz.io.result_vld
    xx_yy_zz := u_xx_yy_zz.io.result

    io.result_vld := xx_yy_zz_vld
    io.result     := xx_yy_zz
}

class AddVecVec(c: RTConfig) extends Component {

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op_a        = in(Vec3(c))
        val op_b        = in(Vec3(c))

        val result_vld  = out(Bool)
        val result      = out(Vec3(c))
    }

    val u_x = new FpxxAdd(c.fpxxConfig, Constants.fpxxAddConfig)
    u_x.io.op_vld <> io.op_vld
    u_x.io.op_a   <> io.op_a.x
    u_x.io.op_b   <> io.op_b.x

    u_x.io.result_vld <> io.result_vld
    u_x.io.result     <> io.result.x


    val u_y = new FpxxAdd(c.fpxxConfig, Constants.fpxxAddConfig)
    u_y.io.op_vld <> io.op_vld
    u_y.io.op_a   <> io.op_a.y
    u_y.io.op_b   <> io.op_b.y

    u_y.io.result     <> io.result.y

    val u_z = new FpxxAdd(c.fpxxConfig, Constants.fpxxAddConfig)
    u_z.io.op_vld <> io.op_vld
    u_z.io.op_a   <> io.op_a.z
    u_z.io.op_b   <> io.op_b.z

    u_z.io.result     <> io.result.z
}

class SubVecVec(c: RTConfig) extends Component {

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op_a        = in(Vec3(c))
        val op_b        = in(Vec3(c))

        val result_vld  = out(Bool)
        val result      = out(Vec3(c))
    }

    val u_x = new FpxxSub(c.fpxxConfig, Constants.fpxxAddConfig)
    u_x.io.op_vld <> io.op_vld
    u_x.io.op_a   <> io.op_a.x
    u_x.io.op_b   <> io.op_b.x

    u_x.io.result_vld <> io.result_vld
    u_x.io.result     <> io.result.x


    val u_y = new FpxxSub(c.fpxxConfig, Constants.fpxxAddConfig)
    u_y.io.op_vld <> io.op_vld
    u_y.io.op_a   <> io.op_a.y
    u_y.io.op_b   <> io.op_b.y

    u_y.io.result     <> io.result.y

    val u_z = new FpxxSub(c.fpxxConfig, Constants.fpxxAddConfig)
    u_z.io.op_vld <> io.op_vld
    u_z.io.op_a   <> io.op_a.z
    u_z.io.op_b   <> io.op_b.z

    u_z.io.result     <> io.result.z
}


class MulVecScalar(c: RTConfig) extends Component {

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op_vec      = in(Vec3(c))
        val op_scalar   = in(Fpxx(c.fpxxConfig))

        val result_vld  = out(Bool)
        val result      = out(Vec3(c))
    }

    val u_x = new FpxxMul(c.fpxxConfig, Constants.fpxxMulConfig)
    u_x.io.op_vld <> io.op_vld
    u_x.io.op_a   <> io.op_vec.x
    u_x.io.op_b   <> io.op_scalar

    u_x.io.result_vld <> io.result_vld
    u_x.io.result     <> io.result.x


    val u_y = new FpxxMul(c.fpxxConfig, Constants.fpxxMulConfig)
    u_y.io.op_vld <> io.op_vld
    u_y.io.op_a   <> io.op_vec.y
    u_y.io.op_b   <> io.op_scalar

    u_y.io.result     <> io.result.y

    val u_z = new FpxxMul(c.fpxxConfig, Constants.fpxxMulConfig)
    u_z.io.op_vld <> io.op_vld
    u_z.io.op_a   <> io.op_vec.z
    u_z.io.op_b   <> io.op_scalar

    u_z.io.result     <> io.result.z
}

class Normalize(c: RTConfig) extends Component {

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op          = in(Vec3(c))

        val result_vld  = out(Bool)
        val result      = out(Vec3(c))
    }

    val vec_dot_vld = Bool
    val vec_dot     = Fpxx(c.fpxxConfig)

    val u_dot = new DotProduct(c)
    u_dot.io.op_vld <> io.op_vld
    u_dot.io.op_a   <> io.op
    u_dot.io.op_b   <> io.op

    u_dot.io.result_vld <> vec_dot_vld
    u_dot.io.result     <> vec_dot


    val denom_vld = Bool
    val denom     = Fpxx(c.fpxxConfig)

    val u_rsqrt = new FpxxRSqrt(c.fpxxConfig, Constants.fpxxRSqrtConfig)
    u_rsqrt.io.op_vld <> vec_dot_vld
    u_rsqrt.io.op     <> vec_dot

    u_rsqrt.io.result_vld <> denom_vld
    u_rsqrt.io.result     <> denom

    val denom_latency = LatencyAnalysis(io.op_vld, denom_vld)
    val op_delayed    = Delay(io.op,     cycleCount = denom_latency)

    val u_vec_adj = new MulVecScalar(c)
    u_vec_adj.io.op_vld    <> denom_vld
    u_vec_adj.io.op_vec    <> op_delayed
    u_vec_adj.io.op_scalar <> denom

    u_vec_adj.io.result_vld <> io.result_vld
    u_vec_adj.io.result     <> io.result
}

class Intersection(c: RTConfig) extends Component {

    val io = new Bundle {
        val op_vld      = in(Bool)
        val ray_origin  = in(Vec3(c))
        val ray_dir     = in(Vec3(c))
        val t           = in(Fpxx(c.fpxxConfig))

        val result_vld  = out(Bool)
        val result      = out(Vec3(c))
    }

    //============================================================
    // dir_mul_t
    //============================================================

    val dir_mul_t_vld = Bool
    val dir_mul_t = Vec3(c)

    val u_mul_dir_t = new MulVecScalar(c)
    u_mul_dir_t.io.op_vld       <> io.op_vld
    u_mul_dir_t.io.op_vec       <> io.ray_dir
    u_mul_dir_t.io.op_scalar    <> io.t

    u_mul_dir_t.io.result_vld   <> dir_mul_t_vld
    u_mul_dir_t.io.result       <> dir_mul_t

    //============================================================
    // intersection
    //============================================================

    val (origin_delayed_vld, origin_delayed, dir_mul_t_delayed) = MatchLatency(
                                                                    io.op_vld,
                                                                    io.op_vld, io.ray_origin,
                                                                    dir_mul_t_vld, dir_mul_t)

    val intersection_vld = Bool
    val intersection = Vec3(c)

    val u_add_origin_dir_mul_t = new AddVecVec(c)
    u_add_origin_dir_mul_t.io.op_vld        <> dir_mul_t_vld
    u_add_origin_dir_mul_t.io.op_a          <> dir_mul_t
    u_add_origin_dir_mul_t.io.op_b          <> origin_delayed

    u_add_origin_dir_mul_t.io.result_vld    <> intersection_vld
    u_add_origin_dir_mul_t.io.result        <> intersection

    io.result_vld <> intersection_vld
    io.result     <> intersection

}

case class RotateConfig(
    hwMul           : Boolean = false
    ){
}

class RotateX(c: RTConfig, rotateConfig: RotateConfig = null) extends Component {

    def hwMul = if (rotateConfig == null) false else rotateConfig.hwMul

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op          = in(Vec3(c))
        val sin         = in(Fpxx(c.fpxxConfig))
        val cos         = in(Fpxx(c.fpxxConfig))

        val result_vld  = out(Bool)
        val result      = out(Vec3(c))
    }

    // (cos, sin) x (y, z)

    val cy_vld          = Bool
    val cy              = Fpxx(c.fpxxConfig)

    val sy_vld          = Bool
    val sy              = Fpxx(c.fpxxConfig)

    val cz_vld          = Bool
    val cz              = Fpxx(c.fpxxConfig)

    val sz_vld          = Bool
    val sz              = Fpxx(c.fpxxConfig)

    val u_cy = new FpxxMul(c.fpxxConfig, if (hwMul) Constants.fpxxHwMulConfig else Constants.fpxxMulConfig)
    u_cy.io.op_vld      <> io.op_vld
    u_cy.io.op_a        <> io.op.y
    u_cy.io.op_b        <> io.cos

    u_cy.io.result_vld  <> cy_vld
    u_cy.io.result      <> cy

    val u_sy = new FpxxMul(c.fpxxConfig, if (hwMul) Constants.fpxxHwMulConfig else Constants.fpxxMulConfig)
    u_sy.io.op_vld      <> io.op_vld
    u_sy.io.op_a        <> io.op.y
    u_sy.io.op_b        <> io.sin

    u_sy.io.result_vld  <> sy_vld
    u_sy.io.result      <> sy


    val u_cz = new FpxxMul(c.fpxxConfig, if (hwMul) Constants.fpxxHwMulConfig else Constants.fpxxMulConfig)
    u_cz.io.op_vld      <> io.op_vld
    u_cz.io.op_a        <> io.op.z
    u_cz.io.op_b        <> io.cos

    u_cz.io.result_vld  <> cz_vld
    u_cz.io.result      <> cz

    val u_sz = new FpxxMul(c.fpxxConfig, if (hwMul) Constants.fpxxHwMulConfig else Constants.fpxxMulConfig)
    u_sz.io.op_vld      <> io.op_vld
    u_sz.io.op_a        <> io.op.z
    u_sz.io.op_b        <> io.sin

    u_sz.io.result_vld  <> sz_vld
    u_sz.io.result      <> sz

    // Calc y, z

    val cy_sz_vld   = Bool
    val sy_cz_vld   = Bool

    val cy_sz       = Fpxx(c.fpxxConfig)
    val sy_cz       = Fpxx(c.fpxxConfig)

    val u_cy_sz = new FpxxSub(c.fpxxConfig, Constants.fpxxAddConfig)
    u_cy_sz.io.op_vld       <> cy_vld
    u_cy_sz.io.op_a         <> cy
    u_cy_sz.io.op_b         <> sz

    u_cy_sz.io.result_vld   <> cy_sz_vld
    u_cy_sz.io.result       <> cy_sz

    val u_sy_cz = new FpxxAdd(c.fpxxConfig, Constants.fpxxAddConfig)
    u_sy_cz.io.op_vld       <> sy_vld
    u_sy_cz.io.op_a         <> sy
    u_sy_cz.io.op_b         <> cz

    u_sy_cz.io.result_vld   <> sy_cz_vld
    u_sy_cz.io.result       <> sy_cz

    val (x_delayed_vld, x_delayed, cy_delayed) = MatchLatency(
                                                            io.op_vld,
                                                            io.op_vld, io.op.x,
                                                            cy_vld,    cy)


    io.result_vld   <> x_delayed_vld
    io.result.x     <> x_delayed
    io.result.y     <> cy_sz
    io.result.z     <> sy_cz
}

class RotateY(c: RTConfig, rotateConfig: RotateConfig = null) extends Component {

    def hwMul = if (rotateConfig == null) false else rotateConfig.hwMul

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op          = in(Vec3(c))
        val sin         = in(Fpxx(c.fpxxConfig))
        val cos         = in(Fpxx(c.fpxxConfig))

        val result_vld  = out(Bool)
        val result      = out(Vec3(c))
    }

    // (cos, sin) x (x, z)

    val cx_vld          = Bool
    val cx              = Fpxx(c.fpxxConfig)

    val sx_vld          = Bool
    val sx              = Fpxx(c.fpxxConfig)

    val cz_vld          = Bool
    val cz              = Fpxx(c.fpxxConfig)

    val sz_vld          = Bool
    val sz              = Fpxx(c.fpxxConfig)

    val u_cx = new FpxxMul(c.fpxxConfig, if (hwMul) Constants.fpxxHwMulConfig else Constants.fpxxMulConfig)
    u_cx.io.op_vld      <> io.op_vld
    u_cx.io.op_a        <> io.op.x
    u_cx.io.op_b        <> io.cos

    u_cx.io.result_vld  <> cx_vld
    u_cx.io.result      <> cx

    val u_sx = new FpxxMul(c.fpxxConfig, if (hwMul) Constants.fpxxHwMulConfig else Constants.fpxxMulConfig)
    u_sx.io.op_vld      <> io.op_vld
    u_sx.io.op_a        <> io.op.x
    u_sx.io.op_b        <> io.sin

    u_sx.io.result_vld  <> sx_vld
    u_sx.io.result      <> sx


    val u_cz = new FpxxMul(c.fpxxConfig, if (hwMul) Constants.fpxxHwMulConfig else Constants.fpxxMulConfig)
    u_cz.io.op_vld      <> io.op_vld
    u_cz.io.op_a        <> io.op.z
    u_cz.io.op_b        <> io.cos

    u_cz.io.result_vld  <> cz_vld
    u_cz.io.result      <> cz

    val u_sz = new FpxxMul(c.fpxxConfig, if (hwMul) Constants.fpxxHwMulConfig else Constants.fpxxMulConfig)
    u_sz.io.op_vld      <> io.op_vld
    u_sz.io.op_a        <> io.op.z
    u_sz.io.op_b        <> io.sin

    u_sz.io.result_vld  <> sz_vld
    u_sz.io.result      <> sz

    // Calc x, z

    val cx_sz_vld   = Bool
    val cz_sx_vld   = Bool

    val cx_sz       = Fpxx(c.fpxxConfig)
    val cz_sx       = Fpxx(c.fpxxConfig)

    val u_cx_sz = new FpxxAdd(c.fpxxConfig, Constants.fpxxAddConfig)
    u_cx_sz.io.op_vld       <> cx_vld
    u_cx_sz.io.op_a         <> cx
    u_cx_sz.io.op_b         <> sz

    u_cx_sz.io.result_vld   <> cx_sz_vld
    u_cx_sz.io.result       <> cx_sz

    val u_cz_sx = new FpxxSub(c.fpxxConfig, Constants.fpxxAddConfig)
    u_cz_sx.io.op_vld       <> cz_vld
    u_cz_sx.io.op_a         <> cz
    u_cz_sx.io.op_b         <> sx

    u_cz_sx.io.result_vld   <> cz_sx_vld
    u_cz_sx.io.result       <> cz_sx

    val (y_delayed_vld, y_delayed, cx_delayed) = MatchLatency(
                                                            io.op_vld,
                                                            io.op_vld, io.op.y,
                                                            cx_vld,    cx)


    io.result_vld   <> y_delayed_vld
    io.result.x     <> cx_sz
    io.result.y     <> y_delayed
    io.result.z     <> cz_sx
}

