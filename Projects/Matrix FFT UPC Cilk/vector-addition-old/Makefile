# some defaults just for .trans.c
NETWORK     ?= ibv
UPC_THREADS ?= 4
VECLEN      ?= 20

UPCC     = upcc
CC       = icc
CFLAGS   = -O -lrt -DVECLEN=$(VECLEN)
UPCFLAGS = -network=$(NETWORK) -T=$(UPC_THREADS) $(CFLAGS) -Wc,"-O3 -funroll-all-loops"
OBJS     = gettime.o

%.o: %.c
	$(CC) -c -o $@ $^ $(CFLAGS)

%.trans.c: %.upc
	$(UPCC) -o $@ $^ $(UPCFLAGS) -trans

%.o: %.upc
	$(UPCC) -o $@ $^ $(UPCFLAGS) -c

%: %.o $(OBJS)
	$(UPCC) -o $@ $^ -link-with=$(CC) $(UPCFLAGS)
