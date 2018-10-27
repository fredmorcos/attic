CC       = gcc
CFLAGS   = -lrt -O3 -funroll-all-loops
OBJS     = gettime.o main.o

%.o: %.c
	$(CC) -c -o $@ $^ $(CFLAGS)

main: $(OBJS)
	$(CC) -o $@ $^ $(CFLAGS)

clean:
	rm -f main $(OBJS)