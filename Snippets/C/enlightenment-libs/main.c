#include <Ecore.h>
#include <Ecore_Evas.h>

Eina_Bool print_dir_stuff (const void *container, void *data, void *fdata) {
  Eina_File_Direct_Info *info = data;

  EINA_LOG_INFO("%s", info->path);
  return EINA_TRUE;
}

int main (int argc, char **argv) {
  Ecore_Evas *ee;
  Evas *evas;
  Eina_List *engines;
  Eina_Iterator *it;
  void *l, *data;
  int w, h;

  eina_init();
  ecore_evas_init();

  engines = ecore_evas_engines_get();

  eina_log_level_set(EINA_LOG_LEVEL_INFO);
  EINA_LOG_INFO("Engines:");
  EINA_LIST_FOREACH(engines, l, data) EINA_LOG_INFO("  %s", data);

  ecore_evas_engines_free(engines);

  it = eina_file_stat_ls("/home/fnm/Workspace/projects");
  eina_iterator_foreach(it, print_dir_stuff, NULL);

  ee = ecore_evas_new(NULL, 10, 10, 100, 100, NULL);

  if (!ee) { printf("nothing\n"); }

  ecore_evas_fullscreen_set(ee, EINA_TRUE);

  ecore_evas_show(ee);

  evas = ecore_evas_get(ee);

  evas_output_size_get(evas, &w, &h);
  EINA_LOG_INFO("%dx%d", w, h);

  ecore_main_loop_begin();

  ecore_evas_free(ee);
  ecore_evas_shutdown();

  return 0;
}
