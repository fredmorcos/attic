CC = icc
CFLAGS = -O3
LDFLAGS = -lrt

BUILD_CC = $(CC) $(CFLAGS) $(LDFLAGS) -DN=$(N)

OBJS = gettime.o cmplx.o dft.o main.o
BIN = dft-2d

default: all
all: $(BIN)

%.o: %.c
	$(BUILD_CC) -c $< -o $@

$(BIN): $(OBJS)
	$(BUILD_CC) $^ -o $@

.PHONY: clean
clean:
	rm -rf $(BIN) $(OBJS)
