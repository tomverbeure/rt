
package rt

import org.scalatest.FunSuite

import spinal.sim._
import spinal.core._
import spinal.core.sim._

object PanoTester {

    class PanoCoreDut extends Component {
        val io = new Bundle {
        }

        val u_pano_core = new PanoCore
    }
}

class PanoTester extends FunSuite {

    test("Pano") {

        var compiled = SimConfig
            .withWave
//            .allOptimisation
            .compile(new PanoTester.PanoCoreDut())

        compiled.doSim { dut =>

            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.forkSimSpeedPrinter(0.2)

            var i = 0;
            while(i<500){
                dut.clockDomain.waitSampling(1000)
                printf("*")
                if (i%10==9){
                    printf("%d\n", i)
                }
                i = i +1
            }
        }
    }

}
