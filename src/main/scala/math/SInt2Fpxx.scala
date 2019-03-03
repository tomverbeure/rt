
package math

import spinal.core._

class SInt2Fpxx(intNrBits: Int, c: FpxxConfig) extends Component {

    def pipeStages      = 2

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op          = in(SInt(intNrBits bits))

        val result_vld  = out(Bool)
        val result      = out(Fpxx(c))
    }

    val p0_vld  = io.op_vld
    val op_p0   = io.op

    val sign_p0     = op_p0.msb

    val op_abs_p0 = UInt(intNrBits bits)
    op_abs_p0 := (sign_p0 ? -op_p0 | op_p0).asUInt

    //============================================================
    val p1_pipe_ena = pipeStages >= 1
    val p1_vld          = OptPipeInit(p0_vld, False, p1_pipe_ena)
    val sign_p1         = OptPipe(sign_p0, p0_vld, p1_pipe_ena)
    val op_abs_p1       = OptPipe(op_abs_p0, p0_vld, p1_pipe_ena)
    //============================================================

    val lz_p1 = LeadingZeros(op_abs_p1.asBits).resize(log2Up(intNrBits))

    //============================================================
    val p2_pipe_ena = pipeStages >= 2
    val p2_vld          = OptPipeInit(p1_vld, False, p2_pipe_ena)
    val sign_p2         = OptPipe(sign_p1, p1_vld, p2_pipe_ena)
    val op_abs_p2       = OptPipe(op_abs_p1, p1_vld, p2_pipe_ena)
    val lz_p2           = OptPipe(lz_p1, p1_vld, p2_pipe_ena)
    //============================================================

    val op_adj_p2 = op_abs_p2 |<< lz_p2

    val sign_final_p2 = Bool
    val exp_final_p2  = UInt(c.exp_size bits)
    val mant_final_p2 = UInt(c.mant_size bits)

    when(op_abs_p2 === 0){
        sign_final_p2 := False
        exp_final_p2.clearAll
        mant_final_p2.clearAll
    }
    .otherwise{
        sign_final_p2 := sign_p2
        exp_final_p2  := (intNrBits + c.bias - 1) - lz_p2

        if (intNrBits-1 >= c.mant_size) {
            mant_final_p2 := op_adj_p2(intNrBits-c.mant_size-1, c.mant_size bits)
        }
        else{
            mant_final_p2 := op_adj_p2.resize(intNrBits-1) @@ U(0, (c.mant_size - intNrBits + 1) bits)
        }
    }
        
    io.result_vld   := p2_vld
    io.result.sign  := sign_final_p2
    io.result.exp   := exp_final_p2
    io.result.mant  := mant_final_p2
}

