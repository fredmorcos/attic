UPCC = upcc
CC = icc
CFLAGS = -O

UPCFLAGS = -network=$(NETWORK) -T=$(UPC_THREADS)

%.o: %.upc
	$(UPCC) -c -o $@ $^ $(CFLAGS) $(UPCFLAGS)

%: %.o
	$(UPCC) -o $@ $^ -link-with=$(CC) $(CFLAGS) $(UPCFLAGS)

