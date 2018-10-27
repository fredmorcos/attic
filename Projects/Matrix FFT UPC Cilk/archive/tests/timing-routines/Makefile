OBJS = gettime.o
CFLAGS = -lrt
BIN = gettime

all: CFLAGS += -shared -fPIC
all: $(BIN)
	mv $(BIN) $(BIN).so
test: CFLAGS += -DTEST
test: $(BIN)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $^

%: %.o
	$(CC) -o $@ $^

.PHONY: clean
clean:
	rm -f $(BIN) $(BIN).so *.o