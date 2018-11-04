
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

class DotProduct(c: RTConfig) extends Component {

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

    val u_xx = new FpxxMul(c.fpxxConfig, pipeStages = 5)
    u_xx.io.op_vld := io.op_vld
    u_xx.io.op_a   := io.op_a.x
    u_xx.io.op_b   := io.op_b.x

    xx_vld := u_xx.io.result_vld
    xx := u_xx.io.result

    val u_yy = new FpxxMul(c.fpxxConfig, pipeStages = 5)
    u_yy.io.op_vld := io.op_vld
    u_yy.io.op_a   := io.op_a.y
    u_yy.io.op_b   := io.op_b.y

    yy_vld := u_yy.io.result_vld
    yy := u_yy.io.result

    val u_zz = new FpxxMul(c.fpxxConfig, pipeStages = 5)
    u_zz.io.op_vld := io.op_vld
    u_zz.io.op_a   := io.op_a.z
    u_zz.io.op_b   := io.op_b.z

    zz_vld := u_zz.io.result_vld
    zz := u_zz.io.result

    val xx_yy_vld     = Bool
    val xx_yy_zz_vld  = Bool

    val xx_yy     = Fpxx(c.fpxxConfig)
    val xx_yy_zz  = Fpxx(c.fpxxConfig)

    val u_xx_yy = new FpxxAdd(c.fpxxConfig, pipeStages = 5)
    u_xx_yy.io.op_vld := xx_vld
    u_xx_yy.io.op_a   := xx
    u_xx_yy.io.op_b   := yy

    xx_yy_vld := u_xx_yy.io.result_vld
    xx_yy := u_xx_yy.io.result

    val xx_yy_latency = LatencyAnalysis(xx.sign, xx_yy.sign)
    val zz_delayed = Delay(zz, cycleCount = xx_yy_latency)

    val u_xx_yy_zz = new FpxxAdd(c.fpxxConfig, pipeStages = 5)
    u_xx_yy_zz.io.op_vld := zz_vld
    u_xx_yy_zz.io.op_a   := xx_yy
    u_xx_yy_zz.io.op_b   := zz_delayed

    xx_yy_zz_vld := u_xx_yy_zz.io.result_vld
    xx_yy_zz := u_xx_yy_zz.io.result

    io.result_vld := xx_yy_zz_vld
    io.result     := xx_yy_zz

}
