UPCC = upcc 
UPCR = upcrun
CFLAGS = -O 
CC = icc
NETWORK = ibv

BIN = test
OBJS = test-cilk.o test.o

all: $(BIN)

%.o: %.c
	$(CC) -fPIC -c -o $@ $<

%.o: %.upc
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -T=$(THREADS) -c -o $@ $<

$(BIN): $(OBJS)
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -link-with=icc -T=$(THREADS) -o $@ $^ -lcilkrts

.PHONY: clean
clean:
	rm -f $(BIN) $(OBJS)
