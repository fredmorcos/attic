BIN = contacts
SCANDIR = scan_clang

SRCS = $(shell echo *.c)
HDRS = $(shell echo *.h)
OBJS = $(SRCS:.c=.o)

LIB_CFLAGS  = `pkg-config --cflags talloc`
LIB_LDFLAGS = `pkg-config --libs talloc`

CFLAGS += -std=c11 -pedantic -Wall -W -Wextra                 \
	  -Wwrite-strings -Wpointer-arith -Wbad-function-cast \
	  -Wsign-conversion -Wconversion                      \
	  -Wincompatible-pointer-types                        \
	  -Wuninitialized                                     \
	  $(LIB_CFLAGS)
LDFLAGS += -g -lm $(LIB_LDFLAGS)

SFLAGS = -fsanitize=address              \
	 -fsanitize=leak                 \
	 -fsanitize=undefined            \
	 -fsanitize=float-divide-by-zero \
	 -fsanitize=float-cast-overflow  \
	 -fsanitize=object-size          \
	 -fno-omit-frame-pointer

ifeq ($(SAN),yes)
	CFLAGS += $(SFLAGS)
	LDFLAGS += $(SFLAGS)
endif

ifeq ($(PROF),yes)
	CFLAGS += -pg
	LDFLAGS += -pg
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
