all: fib fac

fib: fib.o bench.o
	gcc -o $@ $^

fac: fac.o bench.o
	gcc -o $@ $^

.c.o:
	gcc -O3 -pedantic -Wall -Werror -std=c99 -c -o $@ $^

clean:
	rm -rf fib fac *.o *~