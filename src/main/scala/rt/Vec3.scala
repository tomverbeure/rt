
package rt

import spinal.core._
import math._

case class Vec3Config(rtConfig: RTConfig) {
    def fpxxConfig = rtConfig.fpxxConfig
}

object Vec3 {
    def apply(c: RTConfig) : Vec3 = Vec3(Vec3Config(c))
}

case class Vec3(c: Vec3Config) extends Bundle {
    val x   = Fpxx(c.fpxxConfig)
    val y   = Fpxx(c.fpxxConfig)
    val z   = Fpxx(c.fpxxConfig)
}

class DotProduct(c: Vec3Config) extends Component {

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op_a        = in(Vec3(c))
        val op_b        = in(Vec3(c))
        val result_vld  = out(Bool)

        val result      = out(Fpxx(c.fpxxConfig))
    }
}
