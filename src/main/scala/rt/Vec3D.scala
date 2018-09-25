
package rt

import spinal.core._

case class Vec3DConfig(width: Int){
}

object Vec3D{
    def apply(width: Int) : Vec3D = Vec3D(Vec3DConfig(width))
}

case class Vec3D(c: Vec3DConfig) extends Bundle {
    val x   = Scalar(c.width)
    val y   = Scalar(c.width)
    val z   = Scalar(c.width)

    def init() : Vec3D = {
        x   init()
        y   init()
        z   init()
        this
    }
}
