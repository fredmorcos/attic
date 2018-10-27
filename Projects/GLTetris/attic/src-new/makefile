BIN=gltetris
SRC=cube.c grid.c main.c piece.c well.c
HDR=global.h grid.h cube.h piece.h well.h
FLG=-lglut -Wall -Werror -lm -lGL -lGLU

all:
	gcc ${SRC} -o ${BIN} $(FLG)

debug:
	gcc ${SRC} -o ${BIN} $(FLG) -g

run:
	./${BIN}

clean:
	rm -rf ${BIN};

count:
	wc -l ${SRC} ${HDR};
