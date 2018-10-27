CC = icc

# To get a serialized version of the cilk code, 
# add -cilk-serialized to CFLAGS and remove 
# -lcilkrts from LDFLAGS
CFLAGS = -O3
LDFLAGS = -lfftw3 -lm -lrt -lcilkrts

INCLUDE = -B/home/ica/k339970/opt
LIB = -B/home/ica/k339970/opt/lib

BUILD_CC = $(CC) $(CFLAGS) $(INCLUDE) -DN=$(N)
LINK_CC  = $(CC) $(CFLAGS) $(LIB)

OBJS = gettime.o dft.o main.o
BIN = dft-2d

default: all
all: $(BIN)

%.o: %.c
	$(BUILD_CC) -c $< -o $@

$(BIN): $(OBJS)
	$(LINK_CC) $^ -o $@ $(LDFLAGS)

.PHONY: clean
clean:
	rm -rf $(BIN) $(OBJS)
