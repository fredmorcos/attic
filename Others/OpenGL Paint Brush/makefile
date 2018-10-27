all:
	gcc bricks.cc -o bricks -lglut `pkg-config --libs --cflags glu`
	gcc draw-line.cc -o draw-line -lglut `pkg-config --libs --cflags glu`
	gcc paint-brush.cc -o paint-brush -lglut `pkg-config --libs --cflags glu`

clean:
	rm -rf bricks draw-line paint-brush
