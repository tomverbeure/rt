
XILINX_DIR = /opt/Xilinx/14.7/ISE_DS/
XILINX_BIN = $(XILINX_DIR)/ISE/bin/lin64

ise: progmem.hex c64_font.hex vga8x12_font.hex screen_buffer.hex
	$(XILINX_BIN)/ise

impact:
	sudo LD_PRELOAD=$(XILINX_DIR)/usb-driver/libusb-driver.so $(XILINX_BIN)/impact

update_ram: progmem.mem
	$(XILINX_BIN)/data2mem -bm progmem.bmm -bt pano_pins.bit -bd progmem.mem -o b pano_pins.new.bit
	$(XILINX_BIN)/data2mem -bm progmem.bmm -bt pano_pins.new.bit -d > pano_pins.new.bit.dump
	mv pano_pins.bit pano_pins.bit.orig
	mv pano_pins.new.bit pano_pins.bit

dump:
	$(XILINX_BIN)/data2mem -bm progmem.bmm -bt pano_pins.bit -d > pano_pins.bit.dump

.PHONY: progmem.mem
progmem.mem: 
	cd ../sw/ && make
	cp ../sw/progmem.mem .

.PHONY: progmem.hex
progmem.hex:
	cd ../sw/ && make
	cp ../sw/progmem*.hex .

c64_font.hex:
	cd ../misc/ && make
	cp ../misc/$@ $@

vga8x12_font.hex:
	cd ../misc/ && make
	cp ../misc/$@ $@

screen_buffer.hex:
	cd ../misc/ && make
	cp ../misc/$@ $@
