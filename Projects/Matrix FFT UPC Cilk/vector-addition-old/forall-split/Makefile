# some defaults just for .trans.c
NETWORK     ?= ibv
UPC_THREADS ?= 4
VECLEN      ?= 20

UPCC     = upcc
CC       = icc
LDFLAGS  = -lrt
CFLAGS   = -O3 -funroll-all-loops
UPCFLAGS = -network=$(NETWORK) -T=$(UPC_THREADS) -DVECLEN=$(VECLEN) -O -Wc,"-O3 -funroll-all-loops"
OBJS     = vector-addition.o gettime.o

%.o: %.c
	$(CC) -c -o $@ $^ $(CFLAGS)

%.trans.c: %.upc
	$(UPCC) -o $@ $^ $(UPCFLAGS) -trans

%.o: %.upc
	$(UPCC) -c -o $@ $^ $(UPCFLAGS)

%: %.o $(OBJS)
	$(UPCC) -o $@ $^ -link-with=$(CC) $(UPCFLAGS) $(LDFLAGS)
