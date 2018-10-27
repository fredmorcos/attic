#pragma once

#include <stdio.h>

enum termattr {
  TermAttrEnd = -1,
  TermAttrReset = 0,

  TermAttrBold = 1,
  TermAttrDim,
  TermAttrItalic,
  TermAttrUnderlined,
  TermAttrBlink,
  TermAttrUnknown,
  TermAttrReverse,
  TermAttrHidden,

  TermAttrResetBold = 21,
  TermAttrResetDim,
  TermAttrResetItalic,
  TermAttrResetUnderlined,
  TermAttrResetBlink,
  TermAttrResetUnknown,
  TermAttrResetReverse,
  TermAttrResetHidden,

  TermAttrFGDefault = 39,
  TermAttrFGBlack = 30,
  TermAttrFGRed,
  TermAttrFGGreen,
  TermAttrFGYellow,
  TermAttrFGBlue,
  TermAttrFGMagenta,
  TermAttrFGCyan,
  TermAttrFGLightGray,

  TermAttrFGDarkGray = 90,
  TermAttrFGLightRed,
  TermAttrFGLightGreen,
  TermAttrFGLightYellow,
  TermAttrFGLightBlue,
  TermAttrFGLightMagenta,
  TermAttrFGLightCyan,
  TermAttrFGWhite,

  TermAttrBGDefault = 49,
  TermAttrBGBlack = 40,
  TermAttrBGRed,
  TermAttrBGGreen,
  TermAttrBGYellow,
  TermAttrBGBlue,
  TermAttrBGMagenta,
  TermAttrBGCyan,
  TermAttrBGLightGray,

  TermAttrBGDarkGray = 100,
  TermAttrBGLightRed,
  TermAttrBGLightGreen,
  TermAttrBGLightYellow,
  TermAttrBGLightBlue,
  TermAttrBGLightMagenta,
  TermAttrBGLightCyan,
  TermAttrBGWhite,
};

void termsize(const int fd, int *const w, int *const h);
void termset(FILE *const f, const enum termattr *attrs);
void fclearline(FILE *const f);
