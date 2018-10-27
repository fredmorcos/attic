LIBICA = libica.so
LIBICA_OBJS = gettime.o utility.o common.o

.PHONY: clean
all: $(LIBICA)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@ -fPIC

$(LIBICA): $(LIBICA_OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@ -shared -Wl,-soname,$@

clean:
	rm -f $(LIBICA) $(LIBICA_OBJS)
