
package rt

import spinal.core._

class CamSweep extends Component {

    val io = new Bundle {
        val sof         = in(Bool)

        val valid       = out(Bool)
        val ready       = in(Bool)
        val direction   = out(Vec3D(3 exp, -14 exp))
    }

    val dir = Reg(Vec3D(3 exp, -14 exp))


    io.valid        := True 
    io.direction    := dir

    val width  = 400
    val height = 400
//    val stepX = 1.0/width
//    val stepY = 1.0/height

    val xLeft   = -0.5
    val yTop    = 0.5-0.4
    val z       = 1.0
    
    val incrX = SFix(3 exp, -14 exp)
    val incrY = SFix(3 exp, -14 exp)

    incrX := 1.0/width
    incrY := 1.0/height

    val h_cntr = Reg(UInt(12 bits))
    val v_cntr = Reg(UInt(11 bits))

    when(io.sof){
        dir.x   := xLeft
        dir.y   := yTop
        dir.z   := z

        h_cntr  := 0
        v_cntr  := 0
    }
    .elsewhen(io.ready){
        when(h_cntr === U(width)){
            dir.x   := xLeft
            dir.y   := dir.y + incrY

            h_cntr  := 0
            v_cntr  := v_cntr + 1
        }
        .otherwise{
            dir.x   := dir.x + incrX
        }
    }
}





