CC = gcc
CFLAGS = `pkg-config --cflags vte-2.90` -pedantic -Wall -Werror -std=c99
LDFLAGS = `pkg-config --libs vte-2.90`
OBJS = main.o
BIN = tilete

.c.o:
	$(CC) $(CFLAGS) -c -o $@ $^

$(BIN): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^

.PHONY: clean
clean:
	rm -f $(OBJS) $(BIN)
