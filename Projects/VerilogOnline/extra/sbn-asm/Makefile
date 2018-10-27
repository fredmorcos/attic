OBJS = parser.h symtab.o ast.o eval.o parser.o lexer.o main.o
LIBS = -lfl
CFLAGS = -Wall -g -pedantic -std=gnu99

all: sbn-asm

.c.o:
	gcc -c -o $@ $^ $(CFLAGS)

.l.c:
	flex -o $@ $^

.NOTPARALLEL: .y.c
.y.c:
	bison -o $@ $^

.NOTPARALLEL: .y.h
.y.h:
	bison -o /dev/null --defines=$@ $^

sbn-asm: $(OBJS)
	gcc -o $@ $^ $(CFLAGS) $(LIBS)
