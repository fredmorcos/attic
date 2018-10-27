CC      = gcc
VM_BIN  = stackvm
VM_OBJS = main.o

default: all

all: $(VM_BIN)

$(VM_BIN): $(VM_OBJS)
	gcc $^ -o $@

%.o: %.c
	$(CC) -c $^

.PHONY: clean

clean:
	rm -f $(VM_BIN) $(VM_OBJS)
