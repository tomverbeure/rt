

all: rt.ppm
	feh rt.ppm

test: rt
	./rt 200 200

rt.ppm: rt
	./rt 200 200 > rt.ppm

display_string := -Ddisplay_string

source_dir := src
header_dir := src
build_dir  := build
bin_dir    := ./

sources := $(wildcard $(source_dir)/*.c)
objects := $(sources:$(source_dir)/%.c=$(build_dir)/%.o)

CPPFLAGS := $(display_string) -I$(header_dir) -MMD -MP -O0 -g
LDLIBS   := -lm

$(bin_dir)/rt: $(objects) | $(bin_dir)
	$(LINK.o) $^ $(LDLIBS) -o $@

$(build_dir)/%.o: $(source_dir)/%.c | $(build_dir)
	$(COMPILE.c) $(OUTPUT_OPTION) $<

$(build_dir) $(bin_dir): ; mkdir $@

clean: ; $(RM) -fr  rt $(objects) $(deps) $(build_dir)

-include $(deps)
