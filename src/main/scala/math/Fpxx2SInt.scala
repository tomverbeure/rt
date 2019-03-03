
package math

import spinal.core._

class Fpxx2SInt(intNrBits: Int, fracNrBits: Int, c: FpxxConfig) extends Component {
    
    assert(intNrBits + fracNrBits + 2 > c.mant_size, "Not enough bits for SInt size")

    def pipeStages = 1

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op          = in(Fpxx(c))

        val result_vld  = out(Bool)
        val result      = out(SInt((intNrBits + fracNrBits) bits))
        val result_ovfl = out(Bool)
    }

    val p0_vld  = io.op_vld
    val op_p0   = io.op

    val sign_p0     = op_p0.sign
    val ge0_p0      = op_p0.exp >= (c.bias-fracNrBits)

    val shift_p0    = SInt(c.exp_size+2 bits)
    shift_p0 := (intNrBits - 2 + c.bias) - (U"00" @@ op_p0.exp).asSInt      // -2: sign bit + leading "1" of mantissa

    val mant_full_p0 = (U"01" @@ op_p0.mant).asSInt
    val mant_2c_p0   = sign_p0 ? -mant_full_p0 | mant_full_p0

    //============================================================
    val p1_pipe_ena = pipeStages >= 1
    val p1_vld          = OptPipeInit(p0_vld, False,   p1_pipe_ena)
    val ge0_p1          = OptPipe(ge0_p0,     p0_vld, p1_pipe_ena)
    val shift_p1        = OptPipe(shift_p0,   p0_vld, p1_pipe_ena)
    val mant_2c_p1      = OptPipe(mant_2c_p0, p0_vld, p1_pipe_ena)
    //============================================================

    val shift_clipped_p1 = UInt(log2Up(intNrBits+fracNrBits) bits)
    shift_clipped_p1 := shift_p1.asUInt.resize(shift_clipped_p1.getWidth)

    val int_p1 = SInt((intNrBits+fracNrBits) bits)
    val ovfl_p1 = Bool

    when(shift_p1 >= (intNrBits+fracNrBits) || !ge0_p1){
        int_p1.clearAll
        ovfl_p1 := False
    }
    .elsewhen(shift_p1 < 0){
        int_p1 := S(int_p1.getWidth bits, (int_p1.getWidth-1) -> mant_2c_p1.msb, default -> !mant_2c_p1.msb)
        ovfl_p1 := True
    }
    .otherwise{
        int_p1 := (mant_2c_p1 @@ S(0, (intNrBits+fracNrBits-mant_2c_p1.getWidth) bits)) |>> shift_clipped_p1
        ovfl_p1 := False
    }

    io.result_vld   := p1_vld
    io.result       := int_p1
    io.result_ovfl  := ovfl_p1
}

