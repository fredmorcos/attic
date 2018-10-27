#include <assert.h>
#include <err.h>
#include <errno.h>
#include <float.h>
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

struct vector {
  float x;
  float y;
};

typedef struct vector Vector;

struct vertex {
  Vector pos;
  Vector force;
  int rad;
};

typedef struct vertex Vertex;

struct edge {
  Vertex *v1;
  Vertex *v2;
};

typedef struct edge Edge;

inline Vector
vector_add(const Vector *restrict const v1,
           const Vector *restrict const v2)
{
  return (Vector) { v1->x + v2->x, v1->y + v2->y };
}

inline Vector
vector_sub(const Vector *restrict const v1,
           const Vector *restrict const v2)
{
  const Vector v2_neg = { -v2->x, -v2->y };
  return vector_add(v1, &v2_neg);
}

inline Vector
vector_mul(const Vector *const v,
           const float r)
{
  return (Vector) { v->x * r, v->y * r };
}

inline Vector
vector_div(const Vector *const v,
           const float r)
{
  assert(r > 0.01 || r < -0.01);
  return vector_mul(v, 1.0 / r);
}

inline float
vector_mag(const Vector *const v)
{
  return sqrt(v->x * v->x + v->y * v->y);
}

struct qtree {
  Vertex center;
  size_t ncenter;
  const Vertex *vertex;
  struct qtree *quads[4];
};

typedef struct qtree QTree;

QTree *
qtree_new(const Vertex *const vertex) {
  assert(vertex);

  QTree *const qtree = malloc(sizeof(QTree));

  if (!qtree)
    return NULL;

  *qtree = (QTree) {
    .center = *vertex,
    .ncenter = 1,
    .vertex = vertex,
    .quads = { NULL, NULL, NULL, NULL }
  };

  return qtree;
}

void
qtree_free(QTree *const qtree) {
  assert(qtree);

  QTree *c = qtree->quads[0];

  for (size_t i = 0; i < 4; i++, c++) {
    if (c) {
      qtree_free(c);
    }
  }

  free(qtree);
}

/*
 * QTree *
 * qtree_insert(QTree *const qtree,
 *              const Vertex *const vertex) {
 *   assert(qtree);
 *   assert(vertex);
 *
 *   if (qtree->vertex) {
 *   } else {
 *   }
 * }
 */

inline void
vertex_update(const int win_w,
              const int win_h,
              Vertex *const v)
{
  v->pos = vector_add((Vector *) v, &v->force);
  v->force = (Vector) {0, 0};

  if (v->pos.x < v->rad)
    v->pos.x = v->rad;

  if (v->pos.x > win_w - v->rad)
    v->pos.x = win_w - v->rad;

  if (v->pos.y < v->rad)
    v->pos.y = v->rad;

  if (v->pos.y > win_h - v->rad)
    v->pos.y = win_h - v->rad;
}

inline Vector
repulsion(const Vertex *restrict const v1,
          const Vertex *restrict const v2)
{
  const Vector dist = vector_sub((Vector *) v1, (Vector *) v2);
  const float dist_mag = vector_mag(&dist);

  const Vector div = dist_mag == 0.0 ?
    vector_div(&dist, powf(0.1, 3)) :
    vector_div(&dist, powf(dist_mag, 3));

  return vector_mul(&div, v1->rad * v2->rad * 10);
}

inline Vector
attraction(const Edge *const e,
           const Vertex *const v,
           const Vector *const len) {
  const Vertex *const a = e->v1 == v ? e->v1 : e->v2;
  const Vertex *const b = e->v1 == v ? e->v2 : e->v1;

  /*
   * const Vector dist = vector_sub((Vector *) a, (Vector *) b);
   * const float dist_mag = vector_mag(&dist);
   * const float len_mag = vector_mag(len);
   */

  const Vector delta = { a->pos.x - b->pos.x,
                         a->pos.y - b->pos.y };
  const float dist = vector_mag(&delta);
  const float fact = -100 * (dist - 16) / dist;

  warnx("Attr %p -> delta = { %f, %f } | dist = %f | fact = %f",
        (void *) v, delta.x, delta.y, dist, fact);

  return vector_mul(&delta, fact);

  /*
   * warnx("Attr %p -> len = %f | dist = %f | disp = %f",
   *       (void *) e->v2,
   *       vector_mag(len),
   *       vector_mag(&dist),
   *       vector_mag(&disp));
   */

  /* return (Vector) {0, 0}; */
}

void
layout_bf(const int win_w,
          const int win_h,
          const size_t nverts,
          Vertex *const verts,
          const size_t nedges,
          Edge *const edges,
          const Vector *const len)
{
  Vertex *v1 = verts;

  for (size_t i = 0; i < nverts; i++, v1++) {
    {
      Vertex *v2 = verts;

      for (size_t j = 0; j < nverts; j++, v2++) {
        if (i != j) {
          const Vector rforce = repulsion(v1, v2);
          v1->force = vector_add(&v1->force, &rforce);
        }
      }
    }

    {
      Edge *e = edges;

      for (size_t j = 0; j < nedges; j++, e++) {
        const Vector aforce = attraction(e, v1, len);
        v1->force = vector_add(&v1->force, &aforce);
      }
    }

    vertex_update(win_w, win_h, v1);
  }
}

int
main (void)
{
  const int win_w = 1400;
  const int win_h = 800;

  const int min_rad = 5;
  const int max_rad = 15;

  const size_t n_verts = 100;
  const size_t n_edges = 99;

  const Vector len = { win_w / n_verts,
                       win_h / n_verts };

  Vertex *const verts = malloc(sizeof(Vertex) * n_verts);

  if (verts == NULL)
    err(EX_OSERR, "Unable to allocate enough space for %zu verteces", n_verts);

  {
    Vertex *v = verts;

    for (size_t i = 0; i < n_verts; i++, v++) {
      v->rad = min_rad + arc4random_uniform(max_rad - min_rad * 2);
      v->pos.x = v->rad + arc4random_uniform(win_w - v->rad * 2);
      v->pos.y = v->rad + arc4random_uniform(win_h - v->rad * 2);
    }
  }

  Edge *const edges = malloc(sizeof(Edge) * n_edges);

  if (edges == NULL)
    err(EX_OSERR, "Unable to allocate enough space for %zu edges", n_edges);

  {
    Edge *e = edges;

    /*
     * for (size_t i = 0; i < n_edges; i++, e++) {
     *   e->v1 = &verts[arc4random_uniform(n_verts)];
     *   e->v2 = &verts[arc4random_uniform(n_verts)];
     * }
     */

    for (size_t i = 1; i < n_verts; i++, e++) {
      e->v1 = &verts[0];
      e->v2 = &verts[i];
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

  if (SDL_SetRenderDrawBlendMode(rend, SDL_BLENDMODE_BLEND) < 0)
    warnx("Cannot set blending mode: %s", SDL_GetError());

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

    if (SDL_SetRenderDrawColor(rend, 0, 0, 0, 0) < 0) {
      warnx("Cannot set drawing color: %s", SDL_GetError());
      goto finish;
    }

    if (SDL_RenderClear(rend) < 0) {
      warnx("Cannot clear screen: %s", SDL_GetError());
      goto finish;
    }

    layout_bf(win_w, win_h, n_verts, verts, n_edges, edges, &len);

    {
      const Vertex *v = verts;

      for (size_t i = 0; i < n_verts; i++, v++) {
        const int size = v->rad % min_rad + 2;
        const SDL_Rect rect = {
          .x = v->pos.x,
          .y = v->pos.y,
          .w = size,
          .h = size
        };

        if (SDL_SetRenderDrawColor(rend, 255, 255, 255, 255) < 0) {
          warnx("Cannot set vertex drawing color: %s", SDL_GetError());
          goto finish;
        }

        if (SDL_RenderFillRect(rend, &rect) < 0) {
          warnx("Cannot render vertex: %s", SDL_GetError());
          goto finish;
        }
      }

      const Edge *e = edges;

      for (size_t i = 0; i < n_edges; i++, e++) {
        if (SDL_SetRenderDrawColor(rend, 255, 255, 255, 50) < 0) {
          warnx("Cannot set edge drawing color: %s", SDL_GetError());
          goto finish;
        }

        if (SDL_RenderDrawLine
            (rend,
             e->v1->pos.x, e->v1->pos.y,
             e->v2->pos.x, e->v2->pos.y) < 0) {
          warnx("Cannot render edge: %s", SDL_GetError());
          goto finish;
        }
      }
    }

    SDL_RenderPresent(rend);
  }

 finish:
  SDL_DestroyRenderer(rend);
  SDL_DestroyWindow(win);
  SDL_Quit();

  free(verts);
  free(edges);

  return EX_OK;
}
