BIN=gltetris
SRC=cube.c grid.c main.c piece.c well.c
HDR=global.h grid.h cube.h piece.h well.h

all:
	gcc ${SRC} -o ${BIN} -lglut

debug:
	gcc ${SRC} -o ${BIN} -lglut -g

run:
	./${BIN}

clean:
	rm -rf ${BIN};

count:
	wc -l ${SRC} ${HDR};
