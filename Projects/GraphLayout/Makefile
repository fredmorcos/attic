CC         ?= gcc
_CFLAGS     = -pedantic -Wall -Werror -std=gnu99
CFLAGS      = -O3 -DNDEBUG $(_CFLAGS)
TARGET      = graphlayout-test
OBJS        = list.o graph.o graph-render.o graph-gen.o main.o
# HDRS        = $(shell find . -iname "*.h")

GTK_CFLAGS  = $(shell pkg-config --cflags gtk+-2.0)
GTK_LIBS    = $(shell pkg-config --libs gtk+-2.0)

all: $(TARGET)

debug: CFLAGS = -g -DDEBUG $(_CFLAGS)
debug: all

.c.o:
	$(CC) $< -c -o $@ $(CFLAGS) $(GTK_CFLAGS)

$(TARGET): $(OBJS)
	$(CC) -o $@ $^ $(GTK_LIBS)

clean:
	rm -rf $(OBJS) $(TARGET) *~
