BIN = allegro-test
SCANDIR = scan_clang

SRCS = $(shell echo *.c)
HDRS = $(shell echo *.h)
OBJS = $(SRCS:.c=.o)

LIB_CFLAGS  = `pkg-config --cflags talloc allegro_main-5 allegro_image-5`
LIB_LDFLAGS = `pkg-config --libs talloc allegro_main-5 allegro_image-5`

CFLAGS += -Wall -Wextra -std=c11 -pedantic $(LIB_CFLAGS)
LDFLAGS += -g -lm $(LIB_LDFLAGS)

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

.PHONY: clean

%.o: %.c $(HDRS)
	$(CC) $(CFLAGS) -c $< -o $@

$(BIN): $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $@

$(SCANDIR):
	mkdir -p $(SCANDIR)

scan: $(SCANDIR)
	scan-build -o $(SCANDIR) make

lint:
	splint -posix-lib $(LIB_CFLAGS) $(SRCS)

clean:
	rm -rf $(BIN) $(OBJS) gmon.out $(SCANDIR)
