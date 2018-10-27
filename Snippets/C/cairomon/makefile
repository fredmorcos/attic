all:
	gcc -o cairomon src/*.c -ggdb -Wall `pkg-config --libs --cflags gtk+-2.0 libgtop-2.0`;

run:
	./cairomon;

clean:
	rm -rf cairomon;

count:
	wc -l src/*;
