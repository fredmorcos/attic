#include <clutter/clutter.h>

gboolean
stage_clicked (ClutterActor *actor,
               ClutterEvent *event,
               gpointer      user_data) {
  clutter_main_quit();
}

int main (int argc, char **argv) {
  ClutterActor *stage;
  ClutterColor color = {255, 0, 0, 255};

  if (clutter_init(&argc, &argv) != CLUTTER_INIT_SUCCESS)
    fprintf(stderr, "Error initializing clutter\n");

  stage = clutter_stage_new();
  clutter_actor_set_background_color(stage, &color);
  g_signal_connect(G_OBJECT(stage), "button-release-event", G_CALLBACK(stage_clicked), NULL);
  clutter_actor_show(stage);
  clutter_main();

  return 0;
}
