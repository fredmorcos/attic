CC = icc
BIN = cilk-test
OBJS = bench.o cilk-test.o

%.o: %.c
	$(CC) -o $@ -c $^ -O0

$(BIN): $(OBJS)
	$(CC) -o $@ $^ -lcilkrts -O0

.PHONY: clean
clean:
	rm -f $(BIN) $(OBJS)