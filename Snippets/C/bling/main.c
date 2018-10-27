#include "main.h"
#include "bling.h"
#include "prefs.h"

int main(int argc, char **argv)
{
	gtk_init(&argc, &argv);
	start_bling();
	start_prefs();
	gtk_main();
	return 0;
}
