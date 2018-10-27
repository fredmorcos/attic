OUTPUT = pacman

all:
	gcc -I src -o $(OUTPUT) src/*.c -g3 -Wall -Werror -lglut -lm -lGL -lGLU

clean:
	rm -rf $(OUTPUT)

count:
	wc -l src/*

run:
	./$(OUTPUT)
