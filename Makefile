HOST_MACHINE_FLAGS ?=
COMPILER_DEFINES ?= -DENABLE_HDUMP=1

# sforth engine stack depth, in words
STACK_DEPTH ?= 32
# sforth engine core size, in words
CORE_CELLS_COUNT ?= 128 * 1024

CC = gcc
CFLAGS += -Wall -Os -g -fomit-frame-pointer -fdata-sections -ffunction-sections \
	$(HOST_MACHINE_FLAGS) $(COMPILER_DEFINES) \
	 -DSTACK_DEPTH="$(STACK_DEPTH)" \
	 -DCORE_CELLS_COUNT="$(CORE_CELLS_COUNT)"
SFCORE_LIBRARY_OBJECTS = engine.o sf-arch-default.o sf-opt-file.o sf-opt-prog-tools.o sf-opt-string.o

samples: sf sf-string sf-dict

sf-dict: sample-dictionary.o sf-arch.o $(SFCORE_LIBRARY_OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^ -Wl,--gc-sections

sf-string: sample-string.o sf-arch.o $(SFCORE_LIBRARY_OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^ -Wl,--gc-sections

sf: sample-console.o sf-arch.o $(SFCORE_LIBRARY_OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^ -Wl,--gc-sections

clean:
	-rm $(SFCORE_LIBRARY_OBJECTS) sf sf-string sf-dict sf-arch.o sample-dictionary.o sample-string.o sample-console.o

