#ifndef QICONCOMPAT_H
#define QICONCOMPAT_H

#include <QIcon>

#ifndef Q_WS_X11
#define QICON_FROM_THEME(x) QIcon(":/icons/winmac/24x24/" x ".png")
#else
#define QICON_FROM_THEME(x) QIcon::fromTheme(x)
#endif // !Q_WS_X11

#endif // QICONCOMPAT_H
