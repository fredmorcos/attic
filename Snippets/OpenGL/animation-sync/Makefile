CC = gcc
CFLAGS = -Wall -g
LIBS =  -lGL -lGLU -lXt -lX11
LDFLAGS =

all: animsync

animsync: main.o
	$(CC) $(CFLAGS) $^ $(LDFLAGS) $(LIBS) -o $@


.PHONY: clean

clean:
	rm -rf *.o animsync

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@
