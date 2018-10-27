#define WHITE_ON_BLACK 0x07

void k_clear_screen ();
unsigned int k_printf (
		char *message,
		unsigned int line);
void update_cursor (
		int row, 
		int col);

k_main () {
	k_clear_screen ();
	k_printf ("Welcome,\nthis is my kernel!", 0);
};

void k_clear_screen () {
	char *vidmem = (char *) 0xb8000;
	unsigned int i = 0;

	while (i < (80 * 25 * 2)) {
		vidmem [i++] = ' ';
		vidmem [i++] = WHITE_ON_BLACK;
	};
};

unsigned int k_printf (char *message, unsigned int line) {
	char *vidmem = (char *) 0xb8000;
	unsigned int i = 0;

	i = (line * 80 * 2);

	while (*message != 0) {
		if (*message == '\n') {
			i = (++line * 80 * 2);
			*message++;
		} else {
			vidmem [i++] = *message++;
			vidmem [i++] = WHITE_ON_BLACK;
		};
	};

	return (1);
};
