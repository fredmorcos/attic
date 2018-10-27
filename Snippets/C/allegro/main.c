#include <allegro5/allegro.h>
#include <allegro5/allegro_image.h>

int main (int argc, char **argv) {
  al_init();
  /* al_init_image_addon(); */
  al_install_keyboard();

  ALLEGRO_DISPLAY *display = al_create_display(800, 600);
  ALLEGRO_COLOR color = al_map_rgba(255, 0, 0, 255);
  al_clear_to_color(color);

  /* ALLEGRO_BITMAP *bitmap = al_load_bitmap */
  /*   ("~/Workspace/projects/archive/jov/images/WallpaperNebulaSmall.jpg"); */

  ALLEGRO_EVENT_QUEUE *ev_queue = al_create_event_queue();
  al_register_event_source(ev_queue, al_get_keyboard_event_source());

  ALLEGRO_EVENT ev;

  while (1) {
    if (al_get_next_event(ev_queue, &ev)) {
      if (ev.type == ALLEGRO_EVENT_KEY_DOWN &&
          ev.keyboard.keycode == ALLEGRO_KEY_ESCAPE) {
        break;
      }
    }

    /* al_clear_to_color(color); */
    /* al_draw_bitmap(bitmap, 0, 0, 0); */
    al_flip_display();
    al_wait_for_vsync();
  }

  al_destroy_display(display);

  return 0;
}
