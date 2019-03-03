
package math

import spinal.core._

case class FpxxSqrtConfig(
    pipeStages      : Int = 1,
    tableSizeBits   : Int = -1,
    lutMantBits     : Int = -1
            ){
}

class FpxxSqrt(c: FpxxConfig, sqrtConfig: FpxxSqrtConfig = null) extends Component {

    def pipeStages      = if (sqrtConfig == null) 0 else sqrtConfig.pipeStages
    def lutMantBits     = if (sqrtConfig == null || sqrtConfig.lutMantBits < 0)   c.mant_size   else sqrtConfig.lutMantBits
    def tableSizeBits   = if (sqrtConfig == null || sqrtConfig.tableSizeBits < 0) c.mant_size/2 else sqrtConfig.tableSizeBits
    def tableSize       = (1<<tableSizeBits)-(1<<(tableSizeBits-2))

    def sqrtTableContents = for(i <- 0 until tableSize) yield {

        // Values in range (0.5, 2.0(
        val fin     = ((1L<<(tableSizeBits-2)) + i).toDouble / (1L<<(tableSizeBits-1)).toDouble;
        val fout    = scala.math.sqrt(fin)

        val fin_exp     = Fp64.exp(fin)
        val fout_exp    = Fp64.exp(fout)
        var fout_mant   = Fp64.mant(fout)

        val shift = fin_exp - fout_exp

        val round = (fout_mant >> (Fp64.mant_bits-lutMantBits+1)) & 1
        fout_mant = (fout_mant >> (Fp64.mant_bits-lutMantBits)) + round

        // printf("Sqrt table: %d: %10f -> %10f : %08x\n", i, fin, fout, fout_mant)

        U( (fout_mant << 2) | (shift & 0x3), (lutMantBits+2) bits) 
    }

    val sqrt_table = Mem(UInt(lutMantBits+2 bits), initialContent = sqrtTableContents)

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op          = in(Fpxx(c))

        val result_vld  = out(Bool)
        val result      = out(Fpxx(c))
    }

    val p0_vld  = io.op_vld
    val op_p0   = io.op

    //============================================================

    val op_zero_p0 = op_p0.is_zero()
    val op_nan_p0  = op_p0.is_nan() || op_p0.sign
    val op_inf_p0  = op_p0.is_infinite() && !op_p0.sign

    val exp_p0 = SInt(c.exp_size+1 bits)
    exp_p0 := op_p0.exp.resize(c.exp_size+1).asSInt - c.bias

    val gt_1_p0 = !(exp_p0).lsb

    val sqrt_addr_p0 = UInt(tableSizeBits bits)
    sqrt_addr_p0 := (((U(1,1 bits) @@ op_p0.mant) << gt_1_p0.asUInt) >> (c.mant_size+2-tableSizeBits)) - (1<<(tableSizeBits-2))

    //============================================================
    val p1_pipe_ena = pipeStages >= 0
    val p1_vld      = OptPipeInit(p0_vld, False, p1_pipe_ena)
    val op_zero_p1  = OptPipe(op_zero_p0,   p0_vld, p1_pipe_ena)
    val op_nan_p1   = OptPipe(op_nan_p0,    p0_vld, p1_pipe_ena)
    val op_inf_p1   = OptPipe(op_inf_p0,    p0_vld, p1_pipe_ena)
    val exp_p1      = OptPipe(exp_p0,       p0_vld, p1_pipe_ena)

    val sqrt_val_p1 = sqrt_table.readSync(sqrt_addr_p0, p0_vld)
    //============================================================

    val sqrt_shift_p1   = sqrt_val_p1(0, 2 bits)
    val sqrt_mant_p1    = sqrt_val_p1(2, lutMantBits bits)

    val exp_adj_p1 = SInt(c.exp_size+1 bits)
    exp_adj_p1 := (exp_p1 |>> 1) - sqrt_shift_p1.resize(3).asSInt + c.bias

    val sign_final_p1 = Bool
    val exp_final_p1  = UInt(c.exp_size bits)
    val mant_final_p1 = UInt(c.mant_size bits)

    when(op_nan_p1){
        // Negative -> NaN
        sign_final_p1   := False
        exp_final_p1.setAll
        mant_final_p1   := (c.mant_size-1 -> True, default -> False)
    }
    .elsewhen(op_inf_p1){
        // Infinite -> Infinite
        sign_final_p1   := False
        exp_final_p1.setAll
        mant_final_p1.clearAll
    }
    .elsewhen(exp_adj_p1 <= 0 || op_zero_p1){
        // Underflow
        sign_final_p1   := False
        exp_final_p1.clearAll
        mant_final_p1.clearAll
    }
    .otherwise{
        sign_final_p1   := False
        exp_final_p1    := exp_adj_p1.asUInt.resize(c.exp_size)
        mant_final_p1   := sqrt_mant_p1 << (c.mant_size-lutMantBits) 
    }

    io.result_vld   := p1_vld
    io.result.sign  := sign_final_p1
    io.result.exp   := exp_final_p1
    io.result.mant  := mant_final_p1
}
