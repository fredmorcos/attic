OBJS = parser.h symtab.o ast.o eval.o parser.o lexer.o main.o
LIBS = -lfl
CFLAGS = -Wall -g -pedantic -std=gnu99

.c.o:
	@echo "CC $@"; \
	gcc -c -o $@ $^ $(CFLAGS)

.l.c:
	@echo "FL $@"; \
	flex -o $@ $^

.y.c:
	@echo "BS $@"; \
	bison -o $@ $^

.y.h:
	@echo "BS $@"; \
	bison -o /dev/null --defines=$@ $^

sbnasm: $(OBJS)
	@echo "LD $@"; \
	gcc -o $@ $^ $(CFLAGS) $(LIBS)
