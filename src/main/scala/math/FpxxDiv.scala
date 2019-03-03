
package math

import spinal.core._

case class FpxxDivConfig(
    pipeStages      : Int = 1,
    tableSizeBits   : Int = -1,
    lutMantBits     : Int = -1
            ){
}

class FpxxDiv(c: FpxxConfig, divConfig: FpxxDivConfig = null) extends Component {

    assert((c.mant_size&1)==1, "FpxxDiv: mantissa must be odd")

    def pipeStages      = if (divConfig == null) 0 else divConfig.pipeStages
    def halfBits        = (c.mant_size+1)/2
    def lutMantBits     = if (divConfig == null || divConfig.lutMantBits < 0) 2*halfBits+2 else divConfig.lutMantBits
    def tableSizeBits   = if (divConfig == null || divConfig.tableSizeBits < 0) halfBits else divConfig.tableSizeBits
    def tableSize       = 1<< tableSizeBits

    def divTableContents = for(i <- 0 until tableSize) yield {
        val fin     = 1.0 + i.toDouble / tableSize
        val fout    = 1.0 / (fin * fin)

        val fin_exp     = Fp64.exp(fin)
        val fout_exp    = Fp64.exp(fout)
        var fout_mant   = Fp64.mant(fout)

        val round = (fout_mant >> (Fp64.mant_bits-lutMantBits+1)) & 1
        fout_mant = (fout_mant >> (Fp64.mant_bits-lutMantBits)) + round

        U(fout_mant, lutMantBits bits)
    }

    val div_table = Mem(UInt(lutMantBits bits), initialContent = divTableContents)

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op_a        = in(Fpxx(c))
        val op_b        = in(Fpxx(c))

        val result_vld  = out(Bool)
        val result      = out(Fpxx(c))
    }

    val p0_vld  = io.op_vld
    val op_a_p0 = io.op_a
    val op_b_p0 = io.op_b

    //============================================================

    val yh_p0       = (U(1, 1 bits) @@ op_b_p0.mant)(halfBits-1, halfBits+1 bits) << (halfBits-1)
    val yl_p0       = op_b_p0.mant(0, halfBits-1 bits).resize(2*halfBits)

    val yh_m_yl_p0  = yh_p0 - yl_p0
    val div_addr_p0 = op_b_p0.mant >> (c.mant_size-tableSizeBits)

    val exp_p0      = op_a_p0.exp.resize(c.exp_size+1).asSInt - op_b_p0.exp.resize(c.exp_size+1).asSInt
    val sign_p0     = op_a_p0.sign ^ op_b_p0.sign

    val op_a_zero_p0 = op_a_p0.is_zero()
    val op_b_zero_p0 = op_b_p0.is_zero()
    val op_a_inf_p0  = op_a_p0.is_infinite()
    val op_b_inf_p0  = op_b_p0.is_infinite()
    val op_nan_p0    = op_a_p0.is_nan() || op_b_p0.is_nan() || (op_a_inf_p0 && op_b_inf_p0)

    // Value in table where 1/y^2 requires exp adjustment of 2 instead of 1
    val expBoundary = U( ((scala.math.sqrt(2.0)-1.0) * tableSize + 1).toInt, tableSizeBits bits)

    val recip_exp_p0 = (div_addr_p0 === 0) ? U(0, 2 bits) |
                         ((div_addr_p0 < expBoundary) ? U(1, 2 bits) | U(2, 2 bits))

    //============================================================
    val p1_pipe_ena  = true          // Always true because ROM is pipelined as well.
    val p1_vld       = OptPipeInit(p0_vld, False, p1_pipe_ena)
    val yh_m_yl_p1   = OptPipe(yh_m_yl_p0,   p0_vld, p1_pipe_ena)
    val mant_a_p1    = OptPipe(op_a_p0.mant, p0_vld, p1_pipe_ena)
    val exp_p1       = OptPipe(exp_p0,       p0_vld, p1_pipe_ena)
    val sign_p1      = OptPipe(sign_p0,      p0_vld, p1_pipe_ena)
    val op_a_zero_p1 = OptPipe(op_a_zero_p0, p0_vld, p1_pipe_ena)
    val op_b_zero_p1 = OptPipe(op_b_zero_p0, p0_vld, p1_pipe_ena)
    val op_nan_p1    = OptPipe(op_nan_p0,    p0_vld, p1_pipe_ena)
    val recip_exp_p1 = OptPipe(recip_exp_p0, p0_vld, p1_pipe_ena)

    val div_val_p1  = div_table.readSync(div_addr_p0, p0_vld)
    //============================================================

    val recip_yh2_p1    = U(1, 1 bits) @@ div_val_p1

    val exp_full_p1  = exp_p1.resize(c.exp_size+2) - recip_exp_p1.resize(c.exp_size+2).asSInt + S(c.bias+1, c.exp_size+2 bits)

    //============================================================
    val p2_pipe_ena     = pipeStages >= 1
    val p2_vld          = OptPipeInit(p1_vld, False, p2_pipe_ena)
    val yh_m_yl_p2      = OptPipe(yh_m_yl_p1,   p1_vld, p2_pipe_ena)
    val mant_a_p2       = OptPipe(mant_a_p1,    p1_vld, p2_pipe_ena)
    val sign_p2         = OptPipe(sign_p1,      p1_vld, p2_pipe_ena)
    val recip_yh2_p2    = OptPipe(recip_yh2_p1, p1_vld, p2_pipe_ena)
    val exp_full_p2     = OptPipe(exp_full_p1,  p1_vld, p2_pipe_ena)
    val op_a_zero_p2    = OptPipe(op_a_zero_p1, p1_vld, p2_pipe_ena)
    val op_b_zero_p2    = OptPipe(op_b_zero_p1, p1_vld, p2_pipe_ena)
    val op_nan_p2       = OptPipe(op_nan_p1,    p1_vld, p2_pipe_ena)
    //============================================================

    val mant_a_full_p2  = U(1, 1 bits) @@ mant_a_p2
    val x_mul_yhyl_full_p2 = mant_a_full_p2 * yh_m_yl_p2
    val xMulYhYlShift = x_mul_yhyl_full_p2.getWidth - (2*halfBits+3)
    val x_mul_yhyl_p2 = x_mul_yhyl_full_p2(xMulYhYlShift, (2*halfBits+3) bits)

    //============================================================
    val p3_pipe_ena     = pipeStages >= 1
    val p3_vld          = OptPipeInit(p2_vld, False, p3_pipe_ena)
    val sign_p3         = OptPipe(sign_p2,       p2_vld, p3_pipe_ena)
    val x_mul_yhyl_p3   = OptPipe(x_mul_yhyl_p2, p2_vld, p3_pipe_ena)
    val recip_yh2_p3    = OptPipe(recip_yh2_p2,  p2_vld, p3_pipe_ena)
    val exp_full_p3     = OptPipe(exp_full_p2,   p2_vld, p3_pipe_ena)
    val op_a_zero_p3    = OptPipe(op_a_zero_p2,  p2_vld, p3_pipe_ena)
    val op_b_zero_p3    = OptPipe(op_b_zero_p2,  p2_vld, p3_pipe_ena)
    val op_nan_p3       = OptPipe(op_nan_p2,     p2_vld, p3_pipe_ena)
    //============================================================

    // Empty stage: useful for routing from output of one multiplier to input of next multiplier

    //============================================================
    val p4_pipe_ena     = pipeStages >= 2
    val p4_vld          = OptPipeInit(p3_vld, False, p4_pipe_ena)
    val sign_p4         = OptPipe(sign_p3,       p3_vld, p4_pipe_ena)
    val x_mul_yhyl_p4   = OptPipe(x_mul_yhyl_p3, p3_vld, p4_pipe_ena)
    val recip_yh2_p4    = OptPipe(recip_yh2_p3,  p3_vld, p4_pipe_ena)
    val exp_full_p4     = OptPipe(exp_full_p3,   p3_vld, p4_pipe_ena)
    val op_a_zero_p4    = OptPipe(op_a_zero_p3,  p3_vld, p4_pipe_ena)
    val op_b_zero_p4    = OptPipe(op_b_zero_p3,  p3_vld, p4_pipe_ena)
    val op_nan_p4       = OptPipe(op_nan_p3,     p3_vld, p4_pipe_ena)
    //============================================================

    // In the stage after this, we need to shift-adjust div based on the result
    // of the multiplication by 0 to 3. To make sure that we don't drop
    // any of the final lower bits, we need drag along 3 extra bits.

    val div_full_p4 = x_mul_yhyl_p4 * recip_yh2_p4
    val divShift = div_full_p4.getWidth-(2*halfBits+3)
    val div_p4      = div_full_p4(divShift, 2*halfBits+3 bits)

    //============================================================
    val p5_pipe_ena     = pipeStages >= 2
    val p5_vld          = OptPipeInit(p4_vld, False, p5_pipe_ena)
    val sign_p5         = OptPipe(sign_p4,       p4_vld, p5_pipe_ena)
    val div_p5          = OptPipe(div_p4,        p4_vld, p5_pipe_ena)
    val exp_full_p5     = OptPipe(exp_full_p4,   p4_vld, p5_pipe_ena)
    val op_a_zero_p5    = OptPipe(op_a_zero_p4,  p4_vld, p5_pipe_ena)
    val op_b_zero_p5    = OptPipe(op_b_zero_p4,  p4_vld, p5_pipe_ena)
    val op_nan_p5       = OptPipe(op_nan_p4,     p4_vld, p5_pipe_ena)
    //============================================================

    val div_adj_p5 = UInt(c.mant_size bits)
    val exp_adj_p5 = SInt(c.exp_size+2 bits)

    val shift_adj_p5 = UInt(2 bits)
    val exp_delta_p5 = SInt(3 bits)

    when(div_p5(div_p5.getWidth-1, 1 bits) === U"1"){
        shift_adj_p5    := 3
        exp_delta_p5    := 1
    }
    .elsewhen(div_p5(div_p5.getWidth-2, 2 bits) === U"01"){
        shift_adj_p5    := 2
        exp_delta_p5    := 0
    }
    .elsewhen(div_p5(div_p5.getWidth-3, 3 bits) === U"001"){
        shift_adj_p5    := 1
        exp_delta_p5    := -1
    }
    .otherwise {
        shift_adj_p5    := 0
        exp_delta_p5    := -2
    }

    div_adj_p5   := (div_p5 >> shift_adj_p5).resize(2*halfBits-1)
    exp_adj_p5   := exp_full_p5 + exp_delta_p5

    //============================================================
    val p6_pipe_ena     = pipeStages >= 3
    val p6_vld          = OptPipeInit(p5_vld, False, p6_pipe_ena)
    val sign_p6         = OptPipe(sign_p5,       p5_vld, p6_pipe_ena)
    val div_adj_p6      = OptPipe(div_adj_p5,    p5_vld, p6_pipe_ena)
    val exp_adj_p6      = OptPipe(exp_adj_p5,    p5_vld, p6_pipe_ena)
    val op_a_zero_p6    = OptPipe(op_a_zero_p5,  p5_vld, p6_pipe_ena)
    val op_b_zero_p6    = OptPipe(op_b_zero_p5,  p5_vld, p6_pipe_ena)
    val op_nan_p6       = OptPipe(op_nan_p5,     p5_vld, p6_pipe_ena)
    //============================================================

    val sign_final_p6 = Bool
    val exp_final_p6  = UInt(c.exp_size bits)
    val div_final_p6  = UInt(c.mant_size bits)

    when((op_a_zero_p6 && op_b_zero_p6) || op_nan_p6){
        // 0/0 -> Nan
        sign_final_p6 := sign_p6
        exp_final_p6.setAll
        div_final_p6  := (c.mant_size-1 -> True, default -> False)
    }
    .elsewhen(exp_adj_p6 >= ((1<<c.exp_size)-1) || op_b_zero_p6){
        // Infinity
        sign_final_p6 := sign_p6
        exp_final_p6.setAll
        div_final_p6.clearAll
    }
    .elsewhen(exp_adj_p6 <= 0){
        // Underflow -> zero
        sign_final_p6 := sign_p6
        exp_final_p6.clearAll
        div_final_p6.clearAll
    }
    .otherwise{
        sign_final_p6 := sign_p6
        exp_final_p6  := exp_adj_p6(0, c.exp_size bits).asUInt
        div_final_p6  := div_adj_p6
    }

    io.result_vld   := p6_vld
    io.result.sign  := sign_final_p6
    io.result.exp   := exp_final_p6
    io.result.mant  := div_final_p6

}
