BIN=gaspaint
SRC=callbacks.c drawarea.c drawing.c event.c main.c ui.c
HDR=callbacks.h config.h drawarea.h drawing.h event.h main.h public.h ui.h

all:
	make cairo

cairo:
	./cairo.sh;
	gcc ${SRC} -o ${BIN} `pkg-config --libs --cflags gtk+-2.0 cairo`

opengl:
	./opengl.sh;
	gcc ${SRC} -o ${BIN} `pkg-config --libs --cflags gtk+-2.0 gtkglext-1.0 gtkglext-x11-1.0` -lGL -lglut

debug:
	cp config.temp config.h;
	echo "#define __DEBUG" >> config.h;
	echo "Debug enabled. Type 'make clean' to disable debug.";
	echo "Now type 'make cairo' or 'make opengl' to build gaspaint."

run:
	./${BIN}

clean:
	rm -rf ${BIN};
	rm -rf config.h

count:
	wc -l ${SRC} ${HDR}
