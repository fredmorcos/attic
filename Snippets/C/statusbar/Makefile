%.o: %.c
	gcc -c $< -o $@

rendertest: rendertest.o
	gcc $^ -o $@ -lxcb -lxcb-render -lxcb-aux

statusbar: main.o
	gcc $^ -o $@ -lxcb
