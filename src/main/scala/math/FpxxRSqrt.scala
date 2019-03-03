
package math

import spinal.core._

case class FpxxRSqrtConfig(
    pipeStages      : Int = 1,
    tableSizeBits   : Int = -1,
    lutMantBits     : Int = -1
            ){
}

class FpxxRSqrt(c: FpxxConfig, rsqrtConfig: FpxxRSqrtConfig = null) extends Component {

    def pipeStages      = if (rsqrtConfig == null) 0 else rsqrtConfig.pipeStages
    def lutMantBits     = if (rsqrtConfig == null || rsqrtConfig.lutMantBits < 0)   c.mant_size   else rsqrtConfig.lutMantBits
    def tableSizeBits   = if (rsqrtConfig == null || rsqrtConfig.tableSizeBits < 0) c.mant_size/2 else rsqrtConfig.tableSizeBits
    def tableSize       = (1<<tableSizeBits)-(1<<(tableSizeBits-2))

    def rsqrtTableContents = for(i <- 0 until tableSize) yield {

        // Values in range (0.5, 2.0(
        val fin     = ((1L<<(tableSizeBits-2)) + i).toDouble / (1L<<(tableSizeBits-1)).toDouble;
        val fout    = 1.0 / scala.math.sqrt(fin)

        val fin_exp     = Fp64.exp(fin)
        val fout_exp    = Fp64.exp(fout)
        var fout_mant   = Fp64.mant(fout)

        val shift = if (fin_exp - fout_exp > 0) 1 else 0

        val round = (fout_mant >> (Fp64.mant_bits-lutMantBits+1)) & 1
        fout_mant = (fout_mant >> (Fp64.mant_bits-lutMantBits)) + round

        // printf("RSqrt table: %d: %10f -> %10f : %08x, %d, %d\n", i, fin, fout, fout_mant, (fin_exp-fout_exp), shift)

        U( (fout_mant << 2) | (shift & 0x3), (lutMantBits+2) bits) 
    }

    val rsqrt_table = Mem(UInt(lutMantBits+2 bits), initialContent = rsqrtTableContents)

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
    val op_inf_p0  = op_p0.is_infinite()

    val exp_p0 = SInt(c.exp_size+1 bits)
    exp_p0 := op_p0.exp.resize(c.exp_size+1).asSInt - c.bias

    val gt_1_p0 = !(exp_p0).lsb

    val rsqrt_addr_p0 = UInt(tableSizeBits bits)
    rsqrt_addr_p0 := (((U(1,1 bits) @@ op_p0.mant) << gt_1_p0.asUInt) >> (c.mant_size+2-tableSizeBits)) - (1<<(tableSizeBits-2))

    //============================================================
    val p1_pipe_ena = pipeStages >= 0
    val p1_vld      = OptPipeInit(p0_vld, False, p1_pipe_ena)
    val op_zero_p1  = OptPipe(op_zero_p0,   p0_vld, p1_pipe_ena)
    val op_nan_p1   = OptPipe(op_nan_p0,    p0_vld, p1_pipe_ena)
    val op_inf_p1   = OptPipe(op_inf_p0,    p0_vld, p1_pipe_ena)
    val exp_p1      = OptPipe(exp_p0,       p0_vld, p1_pipe_ena)

    val rsqrt_val_p1 = rsqrt_table.readSync(rsqrt_addr_p0, p0_vld)
    //============================================================

    val rsqrt_shift_p1   = rsqrt_val_p1(0, 2 bits).asSInt
    val rsqrt_mant_p1    = rsqrt_val_p1(2, lutMantBits bits)

    val exp_adj_p1 = SInt(c.exp_size+1 bits)
    exp_adj_p1 := -((exp_p1+1) |>> 1) - rsqrt_shift_p1 + c.bias

    val sign_final_p1 = Bool
    val exp_final_p1  = UInt(c.exp_size bits)
    val mant_final_p1 = UInt(c.mant_size bits)

    when(op_nan_p1){
        // Negative -> NaN
        sign_final_p1   := False
        exp_final_p1.setAll
        mant_final_p1   := (c.mant_size-1 -> True, default -> False)
    }
    .elsewhen(op_zero_p1){
        // Infinity
        sign_final_p1   := False
        exp_final_p1.setAll
        mant_final_p1.clearAll
    }
    .elsewhen(op_inf_p1 || exp_adj_p1 <= 0){
        // Underflow
        sign_final_p1   := False
        exp_final_p1.clearAll
        mant_final_p1.clearAll
    }
    .otherwise{
        sign_final_p1   := False
        exp_final_p1    := exp_adj_p1.asUInt.resize(c.exp_size)
        mant_final_p1   := rsqrt_mant_p1 << (c.mant_size-lutMantBits) 
    }

    io.result_vld   := p1_vld
    io.result.sign  := sign_final_p1
    io.result.exp   := exp_final_p1
    io.result.mant  := mant_final_p1
}
