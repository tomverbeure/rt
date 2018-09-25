
package rt

import spinal.core._

class CamSweep extends Component {

    val io = new Bundle {
        val sof         = in(Bool)

        val valid       = out(Bool)
        val ready       = in(Bool)
        val direction   = out(Vec3D(24))
    }

    val dir = Reg(Vec3D(24)) init


    io.valid        := True 
    io.direction    := dir

    val width  = 400
    val height = 400
    val stepX = (1.0/width.toFloat)*65536*(height.toFloat/width.toFloat)
    val stepY = (1.0/height.toFloat)*65536

    val xLeft   = (-0.5*65536).toInt
    val yTop    = ((-0.5-0.4)*65536).toInt
    val z       = (1.0*65536).toInt
    
    val incrX = UInt(24 bits)
    val incrY = UInt(24 bits)

    incrX := U(stepX.toInt)
    incrY := U(stepY.toInt)

    val h_cntr = UInt(12 bits)
    val v_cntr = UInt(11 bits)

    when(io.sof){
        dir.x   := S(xLeft)
        dir.y   := S(yTop)
        dir.z   := S(z)

        h_cntr  := 0
        v_cntr  := 0
    }
    .elsewhen(io.ready){
        when(h_cntr === U(width)){
            dir.x   := S(xLeft)
            dir.y   := dir.y + S(incrY)

            h_cntr  := 0
            v_cntr  := v_cntr + 1
        }
        .otherwise{
            dir.x   := dir.x + S(incrX)
        }
    }
}





