OUTPUT = pacman
FLAGS = -Wall -Werror -lglut -lm -lGL -lGLU -std=c99
SRC = src/wall.c src/bille.c src/game.c src/glut-extra.c src/ground.c src/man.c src/map.c src/opengl-render.c src/text.c src/texture.c src/main.c

all:
	gcc -o $(OUTPUT) ${SRC} -g3 $(FLAGS)

optimize:
	gcc -o $(OUTPUT) ${SRC} $(FLAGS) -ffast-math -fomit-frame-pointer -O3

clean:
	rm -rf $(OUTPUT)

count:
	wc -l src/*

run:
	./$(OUTPUT)
