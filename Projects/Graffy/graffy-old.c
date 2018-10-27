#include <assert.h>
#include <err.h>
#include <errno.h>
#include <sysexits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <limits.h>
#include <unistd.h>
#include <math.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <bsd/bsd.h>

#include <SDL.h>
#include <SDL2_framerate.h>
#include <SDL2_gfxPrimitives.h>

struct point {
  long double x;
  long double y;
};

struct vertex {
  struct point pos;
  struct point net_force;
  uint32_t rad;
};

struct edge {
  struct vertex *v1;
  struct vertex *v2;
};

static const long double electric_constant = 300;
static const long double spring_constant = 0.3;

static const uint32_t win_w = 1600;
static const uint32_t win_h = 1000;
static const uint32_t min_rad = 5;
static const uint32_t max_rad = 15;
static const size_t n_verts = 200;
static const size_t n_edges = n_verts / 1.5;
static const long double timestep = 0.9;
static const long double damping = 0.3;
static const long double td_factor = timestep * damping;

struct point update_vert_positions(struct vertex *verts) {
  struct point energy = { 0, 0 };
  struct vertex *v = verts;

  for (size_t i = 0; i < n_verts; i++, v++) {
    const struct point velocity = { td_factor * v->net_force.x,
                                    td_factor * v->net_force.y };

    v->pos.x += velocity.x * timestep;
    v->pos.y += velocity.y * timestep;

    if (v->pos.x < v->rad)
      v->pos.x = v->rad;

    if (v->pos.x > win_w - v->rad)
      v->pos.x = win_w - v->rad;

    if (v->pos.y < v->rad)
      v->pos.y = v->rad;

    if (v->pos.y > win_h - v->rad)
      v->pos.y = win_h - v->rad;

    energy.x += v->rad * velocity.x * velocity.x / 2.0;
    energy.y += v->rad * velocity.y * velocity.y / 2.0;

    v->net_force = (struct point) { 0, 0 };
  }

  return energy;
}

void repulsion(struct vertex *restrict v1,
               const struct vertex *restrict const v2,
               const long double electric_const) {
  const struct point distance = { v1->pos.x - v2->pos.x,
                                  v1->pos.y - v2->pos.y };
  const long double numerator = v1->rad * v2->rad * electric_const;
  const long double denominator = pow(distance.x * distance.x +
                                      distance.y * distance.y, 1.5);
  const long double factor = numerator / denominator;

  v1->net_force.x += factor * distance.x;
  v1->net_force.y += factor * distance.y;
}

void attraction(struct edge *restrict e,
                const struct vertex *restrict const v,
                const long double spring_const) {
  struct vertex *a;
  const struct vertex *b;

  if (e->v1 == v) {
    a = e->v1;
    b = e->v2;
  } else {
    a = e->v2;
    b = e->v1;
  }

  const struct point dist = { a->pos.x - b->pos.x, a->pos.y - b->pos.y };

  a->net_force.x += -spring_const * dist.x;
  a->net_force.y += -spring_const * dist.y;
}

void brute_force_layout(struct vertex *const verts, struct edge *const edges) {
  struct vertex *v1 = verts;

  for (size_t i = 0; i < n_verts; i++, v1++) {
    struct vertex *v2 = verts;

    for (size_t j = 0; j < n_verts; j++, v2++) {
      if (i != j) {
        repulsion(v1, v2, electric_constant);
      }
    }

    struct edge *e = edges;

    for (size_t i = 0; i < n_edges; i++, e++) {
      if (v1 == e->v1 || v1 == e->v2)
        attraction(e, v1, spring_constant);
    }
  }

  (void) update_vert_positions(verts);
}

void vertex_render(SDL_Renderer *const rend, const struct vertex *const v) {
  (void) filledCircleRGBA(rend, v->pos.x, v->pos.y, v->rad, 255, 255, 255, 255);
}

void edge_render(SDL_Renderer *const rend, const struct edge *const e) {
  (void) lineRGBA(rend, e->v1->pos.x, e->v1->pos.y, e->v2->pos.x, e->v2->pos.y,
                  255, 255, 255, 100);
}

int main (void) {
  struct vertex *const verts = malloc(sizeof(struct vertex) * n_verts);

  if (verts == NULL)
    err(EX_OSERR, "Unable to allocate enough space for %zu verteces", n_verts);

  {
    struct vertex *v = verts;

    for (size_t i = 0; i < n_verts; i++, v++) {
      v->rad = min_rad + arc4random_uniform(max_rad - min_rad * 2);
      v->pos.x = v->rad + arc4random_uniform(win_w - v->rad * 2);
      v->pos.y = v->rad + arc4random_uniform(win_h - v->rad * 2);
      v->net_force = (struct point) { 0, 0 };
    }
  }

  struct edge *const edges = malloc(sizeof(struct edge) * n_edges);

  if (edges == NULL)
    err(EX_OSERR, "Unable to allocate enough space for %zu edges", n_edges);

  {
    struct edge *e = edges;

    for (size_t i = 0; i < n_edges; i++, e++) {
      e->v1 = &verts[arc4random_uniform(n_verts)];
      e->v2 = &verts[arc4random_uniform(n_verts)];
    }
  }

  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) != 0) {
    free(verts);
    free(edges);
    errx(EX_SOFTWARE, "Unable to initialize SDL: %s", SDL_GetError());
  }

  SDL_Window *win;
  SDL_Renderer *rend;

  if (SDL_CreateWindowAndRenderer(win_w, win_h, 0, &win, &rend) != 0) {
    warnx("Unable to create window and renderer: %s", SDL_GetError());
    SDL_Quit();
    free(verts);
    free(edges);
    return EX_SOFTWARE;
  }

  SDL_SetWindowTitle(win, "Graffy");

  SDL_Event ev;

  while (1) {
    if (SDL_PollEvent(&ev) != 0) {
      /* handle exiting */
      if ((ev.type == SDL_KEYUP &&
           (ev.key.keysym.sym == SDLK_ESCAPE ||
            ev.key.keysym.sym == SDLK_q)) ||
          ev.type == SDL_QUIT) {
        break;
      }
    }

    SDL_SetRenderDrawColor(rend, 0, 0, 0, 0);
    SDL_RenderClear(rend);

    brute_force_layout(verts, edges);

    {
      const struct vertex *v = verts;

      for (size_t i = 0; i < n_verts; i++, v++)
        vertex_render(rend, v);

      const struct edge *e = edges;

      for (size_t i = 0; i < n_edges; i++, e++)
        edge_render(rend, e);
    }

    SDL_RenderPresent(rend);
  }

  SDL_DestroyRenderer(rend);
  SDL_DestroyWindow(win);
  SDL_Quit();

  free(verts);
  free(edges);

  return EX_OK;
}
