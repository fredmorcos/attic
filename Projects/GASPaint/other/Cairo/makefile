all:
	gcc -o bin/gas src/*.c `pkg-config --libs --cflags gtk+-2.0` -g;

run:
	cd bin && ./gas;

clean:
	rm -rf bin/*;

count:
	wc -l src/*;
