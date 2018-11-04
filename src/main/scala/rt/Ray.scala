

package rt

import spinal.core._

import spinal.lib.LatencyAnalysis
import spinal.lib.Delay

import math._

case class Ray(c: RTConfig) extends Bundle {
    val origin      = Vec3(c)
    val direction   = Vec3(c)
}

