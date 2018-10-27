#include "tests.h"
#include "list.h"
#include "point.h"
#include <glib.h>

void list_test()
{
	g_print("** STARTING LINKED LIST TEST **\n");
	List *testlist = list_new();
	Point *testbox;
	int i = 0;
	while (i < 100000)
	{
		list_append(testlist, point_new(20, 10));
		i++;
	}
	g_print("list size = %d\n", testlist->size);
	testbox = testlist->last->data;
	g_print("box width = %d\n", testbox->iX);
	g_print("** FINISHED LINKED LIST TEST **\n");
}
