BIN = mediacenter
SCANDIR = scan_clang

SRCS = $(shell echo src/*.c)
HDRS = $(shell echo src/*.h)
OBJS = $(SRCS:.c=.o)

LIB_CFLAGS =                             \
	`sdl2-config --cflags`           \
	`pkg-config --cflags dbus-1 libcec glib-2.0 talloc`
LIB_LDFLAGS =                            \
	`sdl2-config --libs`             \
	-lSDL2_ttf -lSDL2_image          \
	`pkg-config --libs dbus-1 libcec glib-2.0 talloc`

CFLAGS += -Wall -Wextra -std=c11 $(LIB_CFLAGS)
LDFLAGS += -g $(LIB_LDFLAGS)

SFLAGS = -fsanitize=address   \
	 -fsanitize=leak      \
	 -fsanitize=undefined \
	 -fno-omit-frame-pointer

ifeq ($(SAN),yes)
	CFLAGS += $(SFLAGS)
	LDFLAGS += $(SFLAGS)
endif

ifeq ($(OPT),yes)
	CFLAGS += -O3
else
	CFLAGS += -g
endif

.PHONY: clean run

%.o: %.c $(HDRS)
	$(CC) -I/opt/vc/include $(CFLAGS) -c $< -o $@

$(BIN): $(OBJS)
	$(CC) -L/opt/vc/lib $(LDFLAGS) $^ -o $@

run: $(BIN)
	SDL_VIDEO_GL_DRIVER=/opt/vc/lib/libGLESv2.so ./$<

$(SCANDIR):
	mkdir -p $(SCANDIR)

scan: $(SCANDIR)
	scan-build -o $(SCANDIR) make

lint:
	splint -posix-lib $(LIB_CFLAGS) $(SRCS)

clean:
	rm -rf $(BIN) $(OBJS) gmon.out $(SCANDIR)
