#ifndef DOWNLOADWIDGET_H
#define DOWNLOADWIDGET_H

#include "ui_downloadwidget.h"

class DownloadWidget : public QWidget, private Ui::DownloadWidget {
    Q_OBJECT
public:
    DownloadWidget(QWidget *parent = 0);

protected:
    void changeEvent(QEvent *e);
};

#endif // DOWNLOADWIDGET_H
