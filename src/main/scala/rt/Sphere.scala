

package rt

import spinal.core._

import spinal.lib.LatencyAnalysis
import spinal.lib.Delay

import math._

case class Sphere(c: RTConfig) extends Bundle {
    val center  = Vec3(c)
    val radius  = Fpxx(c.fpxxConfig)
}

