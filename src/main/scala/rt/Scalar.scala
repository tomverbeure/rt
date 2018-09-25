
package rt

import spinal.core._

case class ScalarConfig(width: Int) {
}

object Scalar {
    def apply(width: Int) : Scalar = Scalar(ScalarConfig(width))
}

case class Scalar(c: ScalarConfig) extends Bundle {
    val value = SInt(c.width bits)

    def init() : Scalar = {
        value init(0)
        this
    }
}
