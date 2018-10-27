UPCC     = upcc
CC       = icc
LDFLAGS  = -lrt
CFLAGS   = -O3 -funroll-all-loops
UPCFLAGS = -network=$(NETWORK) -T=$(UPC_THREADS) -O -DVECLEN=$(VECLEN) -Wc,"-O3 -funroll-all-loops"
OBJS     = vector-addition.o gettime.o

%.o: %.c
	$(CC) -c -o $@ $^ $(CFLAGS)

%.o: %.upc
# $(UPCC) -o $@ $^ $(UPCFLAGS) -trans
	$(UPCC) -c -o $@ $^ $(UPCFLAGS)

%: %.o $(OBJS)
	$(UPCC) -o $@ $^ -link-with=$(CC) $(UPCFLAGS) $(LDFLAGS)
