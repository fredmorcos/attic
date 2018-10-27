#ifndef UPLOADWIDGET_H
#define UPLOADWIDGET_H

#include "ui_uploadwidget.h"

class UploadWidget : public QWidget, private Ui::UploadWidget {
    Q_OBJECT
public:
    UploadWidget(QWidget *parent = 0);

protected:
    void changeEvent(QEvent *e);
};

#endif // UPLOADWIDGET_H
