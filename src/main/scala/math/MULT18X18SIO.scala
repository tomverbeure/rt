
package math

import spinal.core._

class MULT18X18SIO(inputFF: Boolean, outputFF: Boolean) extends BlackBox {

    val generic = new Generic {
        val AREG        = if (inputFF)  True else False
        val BREG        = if (inputFF)  True else False
        val PREG        = if (outputFF) True else False 
        val B_INPUT     = "DIRECT"
    }

    val io = new Bundle {
        val P       = out(SInt(36 bits))
        val A       = in(SInt(18 bits))
        val B       = in(SInt(18 bits))
        val BCIN    = in(SInt(18 bits))
        val CEA     = in(Bool)
        val CEB     = in(Bool)
        val CEP     = in(Bool)
        val CLK     = in(Bool)
        val RSTA    = in(Bool)
        val RSTB    = in(Bool)
        val RSTP    = in(Bool)
    }

    mapCurrentClockDomain(io.CLK)
    noIoPrefix()
}

/*

   MULT18X18SIO #(
      .AREG(1), // Enable the input registers on the A port (1=on, 0=off)
      .BREG(1), // Enable the input registers on the B port (1=on, 0=off)
      .B_INPUT("DIRECT"), // B cascade input "DIRECT" or "CASCADE" 
      .PREG(1)  // Enable the input registers on the P port (1=on, 0=off)
   ) MULT18X18SIO_inst (
      .BCOUT(BCOUT), // 18-bit cascade output
      .P(P),    // 36-bit multiplier output
      .A(A),    // 18-bit multiplier input
      .B(B),    // 18-bit multiplier input
      .BCIN(BCIN), // 18-bit cascade input
      .CEA(CEA), // Clock enable input for the A port
      .CEB(CEB), // Clock enable input for the B port
      .CEP(CEP), // Clock enable input for the P port
      .CLK(CLK), // Clock input
      .RSTA(RSTA), // Synchronous reset input for the A port
      .RSTB(RSTB), // Synchronous reset input for the B port
      .RSTP(RSTP)  // Synchronous reset input for the P port
   );

*/
