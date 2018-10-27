all:
	gcc -o bin/gas src/*.c `pkg-config --libs --cflags gtk+-2.0 gtkglext-1.0 gtkglext-x11-1.0` -lglut -ggdb -lm;

run:
	cd bin && ./gas;

clean:
	rm -rf bin/*;

count:
	wc -l src/*;
