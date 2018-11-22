
package rt

import spinal.core._
import spinal.lib.LatencyAnalysis
import spinal.lib.Delay
import math._

case class RTConfig() {
    def fpxxConfig = FpxxConfig(6,13)
}

object MatchLatency {

    // Match arrival time of 2 signals with _vld
    def apply[A <: Data, B <: Data](common_vld: Bool, a_vld : Bool, a : A, b_vld : Bool, b : B) : (Bool, A, B) = {

        val a_latency = LatencyAnalysis(common_vld, a_vld)
        val b_latency = LatencyAnalysis(common_vld, b_vld)

        if (a_latency > b_latency) {
            (a_vld, a, Delay(b, cycleCount = a_latency - b_latency) )
        }
        else if (b_latency > a_latency) {
            (b_vld, Delay(a, cycleCount = b_latency - a_latency), b )
        }
        else {
            (a_vld, a, b)
        }
    }

    // Match arrival time of 2 signals with _vld
    def apply[A <: Data](common_vld: Bool, a_vld : Bool, a : A, b_vld : Bool) : (Bool, A) = {

        val a_latency = LatencyAnalysis(common_vld, a_vld)
        val b_latency = LatencyAnalysis(common_vld, b_vld)

        require(b_latency >= a_latency)

        if (b_latency > a_latency) {
            (b_vld, Delay(a, cycleCount = b_latency - a_latency))
        }
        else {
            (a_vld, a)
        }
    }

}

object Constants {
    def fpxxAddConfig   = FpxxAddConfig(pipeStages = 2)
    def fpxxMulConfig   = FpxxMulConfig(pipeStages = 5)
    def fpxxHwMulConfig = FpxxMulConfig(pipeStages = 1, hwMul = true)
    def fpxxDivConfig   = FpxxDivConfig(pipeStages = 5)
    def fpxxSqrtConfig  = FpxxSqrtConfig (pipeStages = 5, tableSizeBits = 12, lutMantBits = 12)
    def fpxxRSqrtConfig = FpxxRSqrtConfig(pipeStages = 5, tableSizeBits = 12, lutMantBits = 12)

    def dotHwMulConfig  = DotProductConfig(hwMul = true)
}


