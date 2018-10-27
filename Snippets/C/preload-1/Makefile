BINS = test
LIBS = libbar.so libbaz.so

all: $(LIBS) $(BINS)

default: all

%.o: %.c
	gcc -c $< -o $@

test: main.o $(LIBS)
	gcc $< -o $@ -L. -lbar

bar.o: bar.c
	gcc -c $< -o $@ -fpic

baz.o: baz.c
	gcc -c $< -o $@ -fpic

lib%.so: %.o
	gcc -shared -fpic $< -o $@

.PHONY: clean

clean:
	rm -rf *.o $(LIBS) $(BINS)
