OBJS = main.o

CFLAGS += -Wall -Wextra `pkg-config --cflags ecore ecore-evas eina evas`
LDFLAGS += -g `pkg-config --libs ecore ecore-evas eina evas`

SFLAGS = -fsanitize=address -fsanitize=leak -fsanitize=undefined \
	-fno-omit-frame-pointer

ifeq ($(SAN),yes)
	CFLAGS += $(SFLAGS)
	LDFLAGS += $(SFLAGS)
endif

ifeq ($(OPT),yes)
	CFLAGS += -O3
else
	CFLAGS += -g
endif

.PHONY: clean run

%.o: %.c
	$(CC) -I/opt/vc/include $(CFLAGS) -c $< -o $@

emc: $(OBJS)
	$(CC) -L/opt/vc/lib $(LDFLAGS) $^ -o $@

run: emc
	SDL_VIDEO_GL_DRIVER=/opt/vc/lib/libGLESv2.so ./$<

clean:
	rm -f emc *.o
