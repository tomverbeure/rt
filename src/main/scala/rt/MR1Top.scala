
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
    mr1.io.data_rsp.data  := mr1.io.data_req.addr(19) ? reg_rd_data | cpu_ram_rd_data


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

    val update_camera_pos_x = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080010")
    val update_camera_pos_y = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080014")
    val update_camera_pos_z = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080018")

    io.camera_pos_x.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.camera_pos_x.toVec().getWidth bits), update_camera_pos_x))
    io.camera_pos_y.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.camera_pos_y.toVec().getWidth bits), update_camera_pos_y))
    io.camera_pos_z.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.camera_pos_z.toVec().getWidth bits), update_camera_pos_z))
    
    val update_rot_x_sin = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080020")
    val update_rot_x_cos = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080024")

    io.rot_x_sin.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.rot_x_sin.toVec().getWidth bits), update_rot_x_sin))
    io.rot_x_cos.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.rot_x_cos.toVec().getWidth bits), update_rot_x_cos))

    val update_rot_y_sin = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080030")
    val update_rot_y_cos = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080034")

    io.rot_y_sin.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.rot_y_sin.toVec().getWidth bits), update_rot_y_sin))
    io.rot_y_cos.fromVec(RegNextWhen(mr1.io.data_req.data(0, io.rot_y_cos.toVec().getWidth bits), update_rot_y_cos))

    val update_eof_sticky = mr1.io.data_req.valid && mr1.io.data_req.wr && (mr1.io.data_req.addr === U"32'h00080040")

    val eof_sticky = Reg(Bool) init(False)
    eof_sticky := io.eof ? True | (eof_sticky && !update_eof_sticky)

    reg_rd_data := B(0, 31 bits) ## eof_sticky

}

