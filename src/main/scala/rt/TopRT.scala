package rt

import spinal.core._
//import vexriscv.demo._

object TopRT{
    def main(args: Array[String]) {

        val config = SpinalConfig()
        config.generateVerilog({
            val toplevel = new Pano()
            toplevel
        })
        println("DONE")
    }
}
