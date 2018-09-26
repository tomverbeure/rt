
package rt

import spinal.core._

case class Vec3DConfig(peak: ExpNumber, resolution: ExpNumber){
}

object Vec3D{
    def apply(peak: ExpNumber, resolution: ExpNumber) : Vec3D = Vec3D(Vec3DConfig(peak, resolution))
}

case class Vec3D(c: Vec3DConfig) extends Bundle {
    val x   = SFix(c.peak, c.resolution)
    val y   = SFix(c.peak, c.resolution)
    val z   = SFix(c.peak, c.resolution)
}
