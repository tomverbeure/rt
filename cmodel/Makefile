

all: rt.ppm rt_rs.ppm
	feh rt.ppm rt_rs.ppm

rt_rs.ppm: rt.ppm
	convert $< -filter box -resize 320x200 $@

rt.ppm: rt
	./rt 640 400 10 > rt.ppm

display_string := -Ddisplay_string

source_dir := src
header_dir := src
build_dir  := build
bin_dir    := ./

sources := $(wildcard $(source_dir)/*.cpp)
objects := $(sources:$(source_dir)/%.cpp=$(build_dir)/%.o)

CC       := g++
CPPFLAGS := $(display_string) -I$(header_dir) -MMD -MP -O0 -g
LDLIBS   := -lm

$(bin_dir)/rt: $(objects) | $(bin_dir)
	$(LINK.o) $^ $(LDLIBS) -o $@

$(build_dir)/%.o: $(source_dir)/%.cpp | $(build_dir)
	$(COMPILE.c) $(OUTPUT_OPTION) $<

$(build_dir) $(bin_dir): ; mkdir $@

clean: ; $(RM) -fr  rt $(objects) $(deps) $(build_dir)

-include $(deps)

$(build_dir)/main.o: Makefile src/fpxx.h src/misc.h

