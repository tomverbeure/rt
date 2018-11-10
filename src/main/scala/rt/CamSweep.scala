
package rt

import spinal.core._
import math._

class CamSweep(c: RTConfig) extends Component {

    val io = new Bundle {
        val pixel_in    = in(PixelStream())

        val pixel_out   = out(PixelStream())
        val ray         = out(Ray(c))
    }

    val width  = 640
    val height = 480

    val stepX = 1.0/height      // Assume height < width
    val stepY = 1.0/height

    val incrX = Fpxx(c.fpxxConfig)
    val incrY = Fpxx(c.fpxxConfig)

    incrX.fromDouble(1.0/width)
    incrY.fromDouble(1.0/height)

    //------------------------------------------------------------
    // x,y of camera in integer...
    //------------------------------------------------------------

    val pix_x = Reg(SInt(12 bits))
    val pix_y = Reg(SInt(11 bits))

    when(io.pixel_in.vsync || (io.pixel_in.req && io.pixel_in.eof)){
        pix_x   := S(-width/2, 12 bits)
        pix_y   := S((height/2 - 0.4 * height).toInt, 11 bits)
    }
    .elsewhen(io.pixel_in.req){
        when(io.pixel_in.eol){
            pix_x   := S(-width/2, 12 bits)
            pix_y   := pix_y - 1
        }
        .otherwise{
            pix_x   := pix_x + 1
        }
    }

    val pix_vld = RegNext(io.pixel_in.req) init(False)

    //------------------------------------------------------------
    // Convert to Float
    //------------------------------------------------------------

    val pix_fp_vld = Bool
    val pix_x_fp = Fpxx(c.fpxxConfig)
    val pix_y_fp = Fpxx(c.fpxxConfig)

    val u_pix_x_fp = new SInt2Fpxx(pix_x.getWidth, c.fpxxConfig)
    u_pix_x_fp.io.op_vld        <> pix_vld
    u_pix_x_fp.io.op            <> pix_x
    u_pix_x_fp.io.result_vld    <> pix_fp_vld
    u_pix_x_fp.io.result        <> pix_x_fp

    val u_pix_y_fp = new SInt2Fpxx(pix_y.getWidth, c.fpxxConfig)
    u_pix_y_fp.io.op_vld        <> pix_vld
    u_pix_y_fp.io.op            <> pix_y
    u_pix_y_fp.io.result        <> pix_y_fp

    //------------------------------------------------------------
    // Multiply to dir_x and dir_y
    //------------------------------------------------------------

    val step = Fpxx(c.fpxxConfig)
    step.fromDouble(stepY)

    val dir_x_vld = Bool
    val dir_y_vld = Bool

    val dir_x = Fpxx(c.fpxxConfig)
    val dir_y = Fpxx(c.fpxxConfig)

    val u_dir_x = new FpxxMul(c.fpxxConfig, Constants.fpxxMulConfig)
    u_dir_x.io.op_vld       <> pix_fp_vld
    u_dir_x.io.op_a         <> pix_x_fp
    u_dir_x.io.op_b         <> step

    u_dir_x.io.result_vld   <> dir_x_vld
    u_dir_x.io.result       <> dir_x

    val u_dir_y = new FpxxMul(c.fpxxConfig, Constants.fpxxMulConfig)
    u_dir_y.io.op_vld       <> pix_fp_vld
    u_dir_y.io.op_a         <> pix_y_fp
    u_dir_y.io.op_b         <> step

    u_dir_y.io.result_vld   <> dir_y_vld
    u_dir_y.io.result       <> dir_y

    //------------------------------------------------------------
    // Output
    //------------------------------------------------------------

    io.ray.origin.x.fromDouble(  0.0)
    io.ray.origin.y.fromDouble( 10.0)
    io.ray.origin.z.fromDouble(-10.0)

    io.ray.direction.x <> dir_x
    io.ray.direction.y <> dir_y
    io.ray.direction.z.fromDouble(1.0)

    val (pixel_in_delayed_vld, pixel_in_delayed, dir_x_delayed) = MatchLatency(
                                                                io.pixel_in.req,
                                                                io.pixel_in.req,   io.pixel_in,
                                                                dir_x_vld,         dir_x)

    io.pixel_out <> pixel_in_delayed

}





