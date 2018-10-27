TESTS = test-gettime

CFLAGS += -I../lib
LDFLAGS += -L../lib -lica

.PHONY: clean
all: $(TESTS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

test-%: test-%.o
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@

clean:
	rm -f $(TESTS)