#include <gio/gio.h>
#include <glib.h>

int main (int argc, char *argv[])
{
	g_type_init();
	GFile *file = g_file_new_for_uri("hello.cpp");
	GFileInfo *info = g_file_query_info(file, G_FILE_ATTRIBUTE_STANDARD_SIZE, G_FILE_QUERY_INFO_NONE, NULL, NULL);
	GFileType type = g_file_info_get_file_type(info);

	g_print("%d\n", type);

	return 0;
}
