
package mr1

import java.nio.file.{Files, Paths}
import spinal.core._

import rt._
import math._

class MR1Top(config: MR1Config, rtConfig: RTConfig) extends Component {

    val io = new Bundle {
        val led1    = out(Bool)
        val led2    = out(Bool)
        val led3    = out(Bool)

        val switch_ = in(Bool)

        val camera_pos_x = out(Fpxx(rtConfig.fpxxConfig))
        val camera_pos_y = out(Fpxx(rtConfig.fpxxConfig))
        val camera_pos_z = out(Fpxx(rtConfig.fpxxConfig))

        val rot_x_sin  = out(Fpxx(rtConfig.fpxxConfig))
        val rot_x_cos  = out(Fpxx(rtConfig.fpxxConfig))
        val rot_y_sin  = out(Fpxx(rtConfig.fpxxConfig))
        val rot_y_cos  = out(Fpxx(rtConfig.fpxxConfig))

        val sphere_pos_x = out(Fpxx(rtConfig.fpxxConfig))
        val sphere_pos_y = out(Fpxx(rtConfig.fpxxConfig))
        val sphere_pos_z = out(Fpxx(rtConfig.fpxxConfig))

        val eof         = in(Bool)
    }

    val mr1 = new MR1(config)

    val wmask = mr1.io.data_req.size.mux(

                    B"00"   -> B"0001",
                    B"01"   -> B"0011",
                    default -> B"1111") |<< mr1.io.data_req.addr(1 downto 0)

    mr1.io.instr_req.ready := True
    mr1.io.instr_rsp.valid := RegNext(mr1.io.instr_req.valid) init(False)

    val cpu_ram_rd_data = Bits(32 bits)
    val reg_rd_data     = Bits(32 bits)

    mr1.io.data_req.ready := True
    mr1.io.data_rsp.valid := RegNext(mr1.io.data_req.valid && !mr1.io.data_req.wr) init(False)
    mr1.io.data_rsp.data  := RegNext(mr1.io.data_req.addr(19)) ? reg_rd_data | cpu_ram_rd_data


    val ramSize = 8192

    val ram = if (true) new Area{

        val byteArray = Files.readAllBytes(Paths.get("sw/progmem8k.bin"))
        val cpuRamContent = for(i <- 0 until ramSize/4) yield {
                B( (byteArray(4*i).toLong & 0xff) + ((byteArray(4*i+1).toLong & 0xff)<<8) + ((byteArray(4*i+2).toLong & 0xff)<<16) + ((byteArray(4*i+3).toLong & 0xff)<<24), 32 bits)
        }

        val cpu_ram = Mem(Bits(32 bits), initialContent = cpuRamContent)

        mr1.io.instr_rsp.data := cpu_ram.readSync(
                enable  = mr1.io.instr_req.valid,
                address = (mr1.io.instr_req.addr >> 2).resized
            )

        cpu_ram_rd_data := cpu_ram.readWriteSync(
                enable  = mr1.io.data_req.valid && !mr1.io.data_req.addr(19),
                address = (mr1.io.data_req.addr >> 2).resized,
                write   = mr1.io.data_req.wr,
                data    = mr1.io.data_req.data,
                mask    = wmask
            )
    }
    else new Area{
        val cpu_ram = new cpu_ram()

        cpu_ram.io.address_a     := (mr1.io.instr_req.addr >> 2).resized
        cpu_ram.io.wren_a        := False
        cpu_ram.io.data_a        := 0
        mr1.io.instr_rsp.data    := cpu_ram.io.q_a


        cpu_ram.io.address_b     := (mr1.io.data_req.addr >> 2).resized
        cpu_ram.io.wren_b        := mr1.io.data_req.valid && mr1.io.data_req.wr && !mr1.io.data_req.addr(19)
        cpu_ram.io.byteena_b     := wmask
        cpu_ram.io.data_b        := mr1.io.data_req.data
        mr1.io.data_rsp.data     := cpu_ram.io.q_b
    }

    val update_leds = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080000")

    io.led1 := RegNextWhen(mr1.io.data_req.data(0), update_leds) init(False)
    io.led2 := RegNextWhen(mr1.io.data_req.data(1), update_leds) init(False)
    io.led3 := RegNextWhen(mr1.io.data_req.data(2), update_leds) init(False)

    //============================================================
    // Camera Pos
    //============================================================

    val update_camera_pos_x = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080010")
    val update_camera_pos_y = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080014")
    val update_camera_pos_z = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080018")

    io.camera_pos_x.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.camera_pos_x.toVec().getWidth bits), update_camera_pos_x))
    io.camera_pos_y.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.camera_pos_y.toVec().getWidth bits), update_camera_pos_y))
    io.camera_pos_z.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.camera_pos_z.toVec().getWidth bits), update_camera_pos_z))

    //============================================================
    // Rotate X
    //============================================================

    val update_rot_x_sin = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080020")
    val update_rot_x_cos = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080024")

    io.rot_x_sin.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.rot_x_sin.toVec().getWidth bits), update_rot_x_sin))
    io.rot_x_cos.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.rot_x_cos.toVec().getWidth bits), update_rot_x_cos))

    //============================================================
    // Rotate Y
    //============================================================

    val update_rot_y_sin = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080030")
    val update_rot_y_cos = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080034")

    io.rot_y_sin.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.rot_y_sin.toVec().getWidth bits), update_rot_y_sin))
    io.rot_y_cos.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.rot_y_cos.toVec().getWidth bits), update_rot_y_cos))

    //============================================================
    // EOF
    //============================================================

    val eof_addr  = (mr1.io.data_req.addr === U"32'h00080040")
    val update_eof_sticky = mr1.io.data_req.valid && mr1.io.data_req.wr && eof_addr

    val eof_sticky = Reg(Bool) init(False)
    eof_sticky := io.eof ? True | (eof_sticky && !update_eof_sticky)

    //============================================================
    // Fpxx add, multiply, int2fpxx, fpxx2int
    //============================================================

    val fpxx_op_a_addr  = (mr1.io.data_req.addr === U"32'h00080050")
    val fpxx_op_b_addr  = (mr1.io.data_req.addr === U"32'h00080054")
    val fpxx_mul_addr   = (mr1.io.data_req.addr === U"32'h00080058")
    val fpxx_add_addr   = (mr1.io.data_req.addr === U"32'h0008005c")
    val int2fpxx_addr   = (mr1.io.data_req.addr === U"32'h00080060")
    val fpxx2int_addr   = (mr1.io.data_req.addr === U"32'h00080064")

    val update_fpxx_op_a = mr1.io.data_req.valid && mr1.io.data_req.wr && fpxx_op_a_addr
    val update_fpxx_op_b = mr1.io.data_req.valid && mr1.io.data_req.wr && fpxx_op_b_addr

    val fpxx_op_a = Fpxx(rtConfig.fpxxConfig)
    val fpxx_op_b = Fpxx(rtConfig.fpxxConfig)
    val fpxx_add  = Fpxx(rtConfig.fpxxConfig)
    val fpxx_mul  = Fpxx(rtConfig.fpxxConfig)
    val int2fpxx  = Fpxx(rtConfig.fpxxConfig)
    val fpxx2int  = SInt((8+12) bits)

    fpxx_op_a.fromVec(RegNextWhen(mr1.io.data_req.data(0, fpxx_op_a.toVec().getWidth bits), update_fpxx_op_a))
    fpxx_op_b.fromVec(RegNextWhen(mr1.io.data_req.data(0, fpxx_op_a.toVec().getWidth bits), update_fpxx_op_b))

    val u_fpxx_add = new FpxxAdd(rtConfig.fpxxConfig, Constants.fpxxAddConfig)
    u_fpxx_add.io.op_vld <> True
    u_fpxx_add.io.op_a   <> fpxx_op_a
    u_fpxx_add.io.op_b   <> fpxx_op_b
    u_fpxx_add.io.result <> fpxx_add

    val u_fpxx_mul = new FpxxMul(rtConfig.fpxxConfig, Constants.fpxxHwMulConfig)
    u_fpxx_mul.io.op_vld <> True
    u_fpxx_mul.io.op_a   <> fpxx_op_a
    u_fpxx_mul.io.op_b   <> fpxx_op_b
    u_fpxx_mul.io.result <> fpxx_mul

    val u_int2fpxx = new SInt2Fpxx(fpxx_op_a.toVec().getWidth, rtConfig.fpxxConfig)
    u_int2fpxx.io.op_vld <> True
    u_int2fpxx.io.op     <> fpxx_op_a.toVec().asSInt
    u_int2fpxx.io.result <> int2fpxx

    val u_fpxx2int = new Fpxx2SInt(8,12, rtConfig.fpxxConfig)
    u_fpxx2int.io.op_vld <> True
    u_fpxx2int.io.op     <> fpxx_op_a
    u_fpxx2int.io.result <> fpxx2int

    reg_rd_data :=   (RegNext(eof_addr)      ? (B(0, 31 bits) ## eof_sticky) |
                     (RegNext(fpxx_mul_addr) ? fpxx_mul.toVec().resize(32)   |
                     (RegNext(fpxx_add_addr) ? fpxx_add.toVec().resize(32)   |
                     (RegNext(int2fpxx_addr) ? int2fpxx.toVec().resize(32)   |
                     (RegNext(fpxx2int_addr) ? fpxx2int.resize(32).asBits    |
                                               B(0, 32 bits))))))

    //============================================================
    // Sphere Pos
    //============================================================

    val update_sphere_pos_x = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080070")
    val update_sphere_pos_y = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080074")
    val update_sphere_pos_z = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080078")

    io.sphere_pos_x.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.sphere_pos_x.toVec().getWidth bits), update_sphere_pos_x))
    io.sphere_pos_y.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.sphere_pos_y.toVec().getWidth bits), update_sphere_pos_y))
    io.sphere_pos_z.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.sphere_pos_z.toVec().getWidth bits), update_sphere_pos_z))


}

