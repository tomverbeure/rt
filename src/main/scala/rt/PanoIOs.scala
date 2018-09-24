
package rt

import spinal.core._

case class VgaIo() extends Bundle {
    val clk      = out(Bool)
    val vsync    = out(Bool)
    val hsync    = out(Bool)
    val blank_   = out(Bool)
    val r        = out(UInt(8 bits))
    val g        = out(UInt(8 bits))
    val b        = out(UInt(8 bits))
}


/*

module pano_pins(
    input wire osc_clk,

    output wire idt_iclk,
    input  wire idt_clk1,

    output wire idt_sclk,
    output wire idt_strobe,
    output wire idt_data,

    output wire led_green,
    output wire led_blue,

    output wire spi_cs_,
    output wire spi_clk,
    output wire spi_dq0,
    output wire spi_dq1,

    output wire audio_mclk,
    output wire audio_bclk,
    output wire audio_dacdat,
    output wire audio_daclrc,
    input  wire audio_adcdat,
    output wire audio_adclrc,

    output wire audio_sclk,
    inout  wire audio_sdin,

    input wire [11:0] sdram_a,
    output wire sdram_ck,
    output wire sdram_ck_,
    input wire sdram_cke,
    input wire sdram_we_,
    input wire sdram_cas_,
    input wire sdram_ras_,
    input wire [3:0] sdram_dm,
    input wire [1:0] sdram_ba,
    input wire [31:0] sdram_dq,
    input wire [3:0] sdram_dqs,

    output wire vo_clk,
    output wire vo_vsync,
    output wire vo_hsync,
    output wire vo_blank_,
    inout  wire vo_scl,
    inout  wire vo_sda,
    output wire [7:0] vo_r,
    output wire [7:0] vo_g,
    output wire [7:0] vo_b,

    output wire usb_clkin,
    output wire usb_reset_n,
    output wire usb_cs_,
    output wire usb_rd_,
    output wire usb_wr_,
    input  wire usb_irq,
    output wire [17:1] usb_a,
    inout  wire [15:0] usb_d
);

*/
