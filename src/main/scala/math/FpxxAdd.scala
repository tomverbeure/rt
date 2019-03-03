
package math

import spinal.core._

// Doesn't support: denormals, rounding, and correct signed zeros

case class FpxxAddConfig(
    pipeStages      : Int = 1
    ){
}

class FpxxAdd(c: FpxxConfig, addConfig: FpxxAddConfig = null) extends Component {

    def pipeStages      = if (addConfig == null) 1 else addConfig.pipeStages

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

    val op_a_is_zero_p0 = op_a_p0.is_zero()
    val op_b_is_zero_p0 = op_b_p0.is_zero()

    val op_a_is_inf_p0 = op_a_p0.is_infinite()
    val op_b_is_inf_p0 = op_b_p0.is_infinite()

    val op_is_zero_p0       = op_a_is_zero_p0 || op_b_is_zero_p0
    val op_is_nan_p0        = op_a_p0.is_nan() || op_b_p0.is_nan() || (op_a_is_inf_p0 && op_b_is_inf_p0 && op_a_p0.sign =/= op_b_p0.sign)
    val op_is_inf_p0        = (op_a_is_inf_p0 || op_b_is_inf_p0)

    val mant_a_p0 = op_a_p0.full_mant()
    val mant_b_p0 = op_b_p0.full_mant()

    when(op_a_is_zero_p0){
        mant_a_p0.clearAll
    }

    when(op_b_is_zero_p0){
        mant_b_p0.clearAll
    }

    val exp_diff_a_b_p0 = SInt(c.exp_size+1 bits)
    val exp_diff_b_a_p0 = UInt(c.exp_size   bits)

    exp_diff_a_b_p0 := op_a_p0.exp.resize(c.exp_size+1).asSInt - op_b_p0.exp.resize(c.exp_size+1).asSInt
    exp_diff_b_a_p0 := op_b_p0.exp - op_a_p0.exp

    val sign_a_swap_p0   = Bool
    val sign_b_swap_p0   = Bool
    val exp_diff_ovfl_p0 = Bool
    val exp_diff_p0      = UInt(log2Up(c.mant_size) bits)
    val exp_add_p0       = UInt(c.exp_size bits)
    val mant_a_swap_p0   = UInt(c.mant_size+1 bits)
    val mant_b_swap_p0   = UInt(c.mant_size+1 bits)


    when(exp_diff_a_b_p0 >= 0){
        sign_a_swap_p0   := op_a_p0.sign
        sign_b_swap_p0   := op_b_p0.sign
        exp_add_p0       := op_a_p0.exp
        exp_diff_ovfl_p0 := exp_diff_a_b_p0 > c.mant_size
        exp_diff_p0      := exp_diff_a_b_p0.resize(log2Up(c.mant_size)).asUInt
        mant_a_swap_p0   := mant_a_p0
        mant_b_swap_p0   := mant_b_p0
    }
    .otherwise{
        sign_a_swap_p0   := op_b_p0.sign
        sign_b_swap_p0   := op_a_p0.sign
        exp_add_p0       := op_b_p0.exp
        exp_diff_ovfl_p0 := exp_diff_b_a_p0 > c.mant_size
        exp_diff_p0      := exp_diff_b_a_p0.resize(log2Up(c.mant_size))
        mant_a_swap_p0   := mant_b_p0
        mant_b_swap_p0   := mant_a_p0
    }

    //============================================================

    val p1_pipe_ena = pipeStages >= 3

    val p1_vld          = OptPipeInit(p0_vld, False,       p1_pipe_ena)
    val op_is_zero_p1   = OptPipe(op_is_zero_p0,   p0_vld, p1_pipe_ena)
    val op_is_nan_p1    = OptPipe(op_is_nan_p0,    p0_vld, p1_pipe_ena)
    val op_is_inf_p1    = OptPipe(op_is_inf_p0,    p0_vld, p1_pipe_ena)
    val sign_a_p1       = OptPipe(sign_a_swap_p0,  p0_vld, p1_pipe_ena)
    val sign_b_p1       = OptPipe(sign_b_swap_p0,  p0_vld, p1_pipe_ena)
    val exp_add_p1      = OptPipe(exp_add_p0,      p0_vld, p1_pipe_ena)
    val exp_diff_ovfl_p1= OptPipe(exp_diff_ovfl_p0,p0_vld, p1_pipe_ena)
    val exp_diff_p1     = OptPipe(exp_diff_p0,     p0_vld, p1_pipe_ena)
    val mant_a_p1       = OptPipe(mant_a_swap_p0,  p0_vld, p1_pipe_ena)
    val mant_b_p1       = OptPipe(mant_b_swap_p0,  p0_vld, p1_pipe_ena)

    //============================================================

    val mant_a_adj_p1 = UInt(c.mant_size+2 bits)
    val mant_b_adj_p1 = UInt(c.mant_size+2 bits)

    mant_a_adj_p1  := mant_a_p1.resize(c.mant_size+2);
    mant_b_adj_p1  := exp_diff_ovfl_p1 ? U(0, c.mant_size+2 bits) | (mant_b_p1 |>> exp_diff_p1).resize(c.mant_size+2)

    //============================================================

    val p2_pipe_ena = pipeStages >= 1

    val p2_vld        = OptPipeInit(p1_vld, False,     p2_pipe_ena)
    val op_is_zero_p2 = OptPipe(op_is_zero_p1, p1_vld, p2_pipe_ena)
    val op_is_nan_p2  = OptPipe(op_is_nan_p1,  p1_vld, p2_pipe_ena)
    val op_is_inf_p2  = OptPipe(op_is_inf_p1,  p1_vld, p2_pipe_ena)
    val sign_a_p2     = OptPipe(sign_a_p1,     p1_vld, p2_pipe_ena)
    val sign_b_p2     = OptPipe(sign_b_p1,     p1_vld, p2_pipe_ena)
    val exp_add_p2    = OptPipe(exp_add_p1,    p1_vld, p2_pipe_ena)
    val mant_a_adj_p2 = OptPipe(mant_a_adj_p1, p1_vld, p2_pipe_ena)
    val mant_b_adj_p2 = OptPipe(mant_b_adj_p1, p1_vld, p2_pipe_ena)

    //============================================================

    val sign_add_p2 = Bool

    val mant_a_opt_inv_p2 = UInt(c.mant_size+3 bits)
    val mant_b_opt_inv_p2 = UInt(c.mant_size+3 bits)

    when(sign_a_p2 === sign_b_p2){
        sign_add_p2       := sign_a_p2
        mant_a_opt_inv_p2 := mant_a_adj_p2 @@ False
        mant_b_opt_inv_p2 := mant_b_adj_p2 @@ False
    }
    .elsewhen(mant_a_adj_p2 >= mant_b_adj_p2){
        sign_add_p2       := sign_a_p2
        mant_a_opt_inv_p2 :=  mant_a_adj_p2 @@ True
        mant_b_opt_inv_p2 := ~mant_b_adj_p2 @@ True
    }
    .otherwise{
        sign_add_p2       := sign_b_p2
        mant_a_opt_inv_p2 := ~mant_a_adj_p2 @@ True
        mant_b_opt_inv_p2 :=  mant_b_adj_p2 @@ True
    }

    //============================================================

    val p3_pipe_ena = pipeStages >= 4

    val p3_vld            = OptPipeInit(p2_vld, False,         p3_pipe_ena)
    val op_is_zero_p3     = OptPipe(op_is_zero_p2,     p2_vld, p3_pipe_ena)
    val op_is_nan_p3      = OptPipe(op_is_nan_p2,      p2_vld, p3_pipe_ena)
    val op_is_inf_p3      = OptPipe(op_is_inf_p2,      p2_vld, p3_pipe_ena)
    val sign_add_p3       = OptPipe(sign_add_p2,       p2_vld, p3_pipe_ena)
    val exp_add_p3        = OptPipe(exp_add_p2,        p2_vld, p3_pipe_ena)
    val mant_a_opt_inv_p3 = OptPipe(mant_a_opt_inv_p2, p2_vld, p3_pipe_ena)
    val mant_b_opt_inv_p3 = OptPipe(mant_b_opt_inv_p2, p2_vld, p3_pipe_ena)

    //============================================================

    val mant_add_p3 = (mant_a_opt_inv_p3 + mant_b_opt_inv_p3)(1, c.mant_size+2 bits)

    //============================================================

    val p4_pipe_ena = pipeStages >= 2

    val p4_vld        = OptPipeInit(p3_vld, False,     p4_pipe_ena)
    val op_is_zero_p4 = OptPipe(op_is_zero_p3, p3_vld, p4_pipe_ena)
    val op_is_nan_p4  = OptPipe(op_is_nan_p3,  p3_vld, p4_pipe_ena)
    val op_is_inf_p4  = OptPipe(op_is_inf_p3,  p3_vld, p4_pipe_ena)
    val sign_add_p4   = OptPipe(sign_add_p3,   p3_vld, p4_pipe_ena)
    val exp_add_p4    = OptPipe(exp_add_p3,    p3_vld, p4_pipe_ena)
    val mant_add_p4   = OptPipe(mant_add_p3,   p3_vld, p4_pipe_ena)

    //============================================================

    // Doing leading zeros detection on the output of the adder adds directly to the critical path, or
    // requires an additional pipeline stage. An alternative is to do leading zeros anticipation (LZA)
    // and do it in parallel with the addition, but that's not done here.

    val lz_p4 = LeadingZeros(mant_add_p4.resize(c.mant_size+1).asBits)
    when(op_is_zero_p4){
        lz_p4.clearAll
    }

    val exp_add_adj_p4  = UInt(c.exp_size bits)
    val mant_add_adj_p4 = UInt(c.mant_size+1 bits)

    when(mant_add_p4(c.mant_size+1)){
        mant_add_adj_p4  := mant_add_p4 >> 1
        exp_add_adj_p4   := exp_add_p4 + 1
        lz_p4.clearAll
    }
    .otherwise{
        mant_add_adj_p4  := mant_add_p4.resize(c.mant_size+1)
        exp_add_adj_p4   := exp_add_p4
    }

    //============================================================

    val p5_pipe_ena = pipeStages >= 5

    val p5_vld        = OptPipeInit(p4_vld, False,       p5_pipe_ena)
    val op_is_nan_p5  = OptPipe(op_is_nan_p4,    p4_vld, p5_pipe_ena)
    val op_is_inf_p5  = OptPipe(op_is_inf_p4,    p4_vld, p5_pipe_ena)
    val lz_p5         = OptPipe(lz_p4,           p4_vld, p5_pipe_ena)
    val sign_add_p5   = OptPipe(sign_add_p4,     p4_vld, p5_pipe_ena)
    val exp_add_p5    = OptPipe(exp_add_adj_p4,  p4_vld, p5_pipe_ena)
    val mant_add_p5   = OptPipe(mant_add_adj_p4, p4_vld, p5_pipe_ena)

    //============================================================

    val sign_final_p5  = Bool
    val exp_final_p5   = UInt(c.exp_size bits)
    val mant_final_p5  = UInt(c.mant_size+1 bits)

    val exp_add_m_lz = SInt(c.exp_size+1 bits)
    exp_add_m_lz    := exp_add_p5.resize(c.exp_size+1).asSInt - lz_p5.resize(c.exp_size+1).asSInt
    val exp_eq_lz   = exp_add_p5 === lz_p5

    when(op_is_nan_p5){
        sign_final_p5   := False
        exp_final_p5.setAll
        mant_final_p5   := (c.mant_size-1 -> True, default -> False)
    }
    .elsewhen(op_is_inf_p5 || exp_add_p5.andR){
        sign_final_p5   := sign_add_p5
        exp_final_p5.setAll
        mant_final_p5.clearAll
    }
    .otherwise{
        sign_final_p5   := sign_add_p5
        exp_final_p5    := ((lz_p5 < c.mant_size+1) && !exp_add_m_lz.msb) ? exp_add_m_lz.asUInt.resize(c.exp_size) | 0
        mant_final_p5   := (!exp_add_m_lz.msb && !exp_eq_lz) ? (mant_add_p5 |<< lz_p5) | 0
    }

    io.result_vld   := p5_vld
    io.result.sign  := sign_final_p5
    io.result.exp   := exp_final_p5
    io.result.mant  := mant_final_p5.resize(c.mant_size)
}


class FpxxSub(c: FpxxConfig, addConfig: FpxxAddConfig) extends Component {

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op_a        = in(Fpxx(c))
        val op_b        = in(Fpxx(c))

        val result_vld  = out(Bool)
        val result      = out(Fpxx(c))
    }

    val op_b = Fpxx(c)
    op_b.sign   := !io.op_b.sign
    op_b.exp    := io.op_b.exp
    op_b.mant   := io.op_b.mant

    val u_add = new FpxxAdd(c, addConfig)
    u_add.io.op_vld     <> io.op_vld
    u_add.io.op_a       <> io.op_a
    u_add.io.op_b       <> op_b

    u_add.io.result_vld <> io.result_vld
    u_add.io.result     <> io.result
}
