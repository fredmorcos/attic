BIN = lisp
CFLAGS += `pkg-config --cflags talloc`
LIBS = `pkg-config --libs talloc`

CFLAGS += -std=c11 -pedantic -Wall -W -Wextra \
	  -Wwrite-strings -Wpointer-arith -Wbad-function-cast \
	  -Wsign-conversion -Wconversion

CFLAGS += -fsanitize=address              \
	  -fsanitize=leak                 \
	  -fsanitize=undefined            \
	  -fsanitize=float-divide-by-zero \
	  -fsanitize=float-cast-overflow  \
	  -fsanitize=object-size          \
	  -fno-omit-frame-pointer

all: $(BIN)

$(BIN): main.c file.c lex.c
	clang $(CFLAGS) $(LIBS) -g $^ -o $@

clean:
	rm -rf $(BIN)
