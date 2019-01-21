package rt

import spinal.core._
import spinal.lib.io._

object TopRT{
    def main(args: Array[String]) {

        val config = SpinalConfig()
        config.generateVerilog({
            val toplevel = new Pano()
            InOutWrapper(toplevel)
        })
        println("DONE")
    }
}
