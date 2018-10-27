#include "obj.h"

#include <SDL.h>
#include <SDL_ttf.h>

static SDL_bool _create_texture (SDL_Renderer *rend,
                                 SDL_Texture **t,
                                 SDL_Rect *r);
static SDL_bool _reset_render_target(SDL_Renderer *rend);

static SDL_bool _create_texture (SDL_Renderer *rend,
                                 SDL_Texture **t,
                                 SDL_Rect *r) {
  SDL_DestroyTexture(*t);
  if (!(*t = SDL_CreateTexture(rend,
                               SDL_PIXELFORMAT_RGBA4444,
                               SDL_TEXTUREACCESS_TARGET,
                               r->w, r->h))) {
    SDL_LogCritical(SDL_LOG_CATEGORY_RENDER,
                    "Cannot create texture: %s\n",
                    SDL_GetError());
    *t = NULL;
    return SDL_FALSE;
  }

  return SDL_TRUE;
}

static SDL_bool _reset_render_target(SDL_Renderer *rend) {
  if (SDL_SetRenderTarget(rend, NULL) < 0) {
    SDL_LogCritical(SDL_LOG_CATEGORY_RENDER,
                    "Cannot reset render target back to default: %s\n",
                    SDL_GetError());
    return SDL_FALSE;
  }

  return SDL_TRUE;
}

void obj_destroy_textures (Obj *o) {
  SDL_DestroyTexture(o->content_texture);
  o->content_texture = NULL;

  SDL_DestroyTexture(o->box_texture);
  o->box_texture = NULL;

  SDL_DestroyTexture(o->texture);
  o->texture = NULL;
}

SDL_bool obj_set_text (Obj *o,
                       SDL_Renderer *rend,
                       SDL_Color *color,
                       TTF_Font *font,
                       const char *t,
                       SDL_bool *size_changed) {
  SDL_bool rc = SDL_TRUE;
  SDL_Surface *surf = NULL;
  int new_w = -1, new_h = -1;

  if (TTF_SizeText(font, t, &new_w, &new_h)) {
    SDL_LogCritical(SDL_LOG_CATEGORY_APPLICATION,
                    "Cannot size text (%s) with font: %s\n",
                    t, TTF_GetError());
    return SDL_FALSE;
  }

  if (new_w != o->rect.w) {
    *size_changed = SDL_TRUE;
    o->rect.w = new_w;
  }

  if (new_h != o->rect.h) {
    *size_changed = SDL_TRUE;
    o->rect.h = new_h;
  }

  if (!(surf = TTF_RenderText_Blended(font, t, *color))) {
    SDL_LogCritical(SDL_LOG_CATEGORY_RENDER,
                    "Cannot create text surface: %s\n", TTF_GetError());
    return SDL_FALSE;
  }

  SDL_DestroyTexture(o->content_texture);

  if (!(o->content_texture = SDL_CreateTextureFromSurface(rend, surf))) {
    SDL_LogCritical(SDL_LOG_CATEGORY_RENDER,
                    "Cannot create texture from surface: %s\n",
                    TTF_GetError());
    o->content_texture = NULL;
    goto quit_fail;
  }

 quit:
  SDL_FreeSurface(surf);
  return rc;

 quit_fail:
  rc = SDL_FALSE;
  goto quit;
}

SDL_bool obj_set_bg (Obj *o, SDL_Renderer *rend, SDL_Color *c) {
  if (!c) {
    SDL_DestroyTexture(o->box_texture);
    o->box_texture = NULL;
    return SDL_TRUE;
  }

  if (_create_texture(rend, &o->box_texture, &o->rect) == SDL_FALSE) {
    return SDL_FALSE;
  }

  if (SDL_SetRenderTarget(rend, o->box_texture) < 0) {
    SDL_LogCritical(SDL_LOG_CATEGORY_RENDER,
                    "Cannot set render target to texture: %s\n",
                    SDL_GetError());
    return SDL_FALSE;
  }

  if (SDL_SetRenderDrawColor(rend, c->r, c->g, c->b, c->a) < 0) {
    SDL_LogError(SDL_LOG_CATEGORY_RENDER,
                 "Cannot set render draw color: %s\n", SDL_GetError());
  }

  if (SDL_RenderFillRect(rend, NULL) < 0) {
    SDL_LogError(SDL_LOG_CATEGORY_RENDER,
                 "Cannot fill rectangle: %s\n", SDL_GetError());
  }

  return _reset_render_target(rend);
}

SDL_bool obj_blit_textures (Obj *o, SDL_Renderer *rend) {
  if (_create_texture(rend, &o->texture, &o->rect) == SDL_FALSE) {
    return SDL_FALSE;
  }

  if (SDL_SetTextureBlendMode(o->texture, SDL_BLENDMODE_BLEND) < 0) {
    SDL_LogError(SDL_LOG_CATEGORY_RENDER,
                 "Cannot set texture blend mode: %s\n", SDL_GetError());
  }

  if (SDL_SetRenderTarget(rend, o->texture) < 0) {
    SDL_LogCritical(SDL_LOG_CATEGORY_RENDER,
                    "Cannot set render target to texture: %s\n",
                    SDL_GetError());
    return SDL_FALSE;
  }

  if (o->box_texture) SDL_RenderCopy(rend, o->box_texture, NULL, NULL);
  if (o->content_texture) SDL_RenderCopy(rend, o->content_texture, NULL, NULL);

  return _reset_render_target(rend);
}
