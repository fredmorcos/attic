#ifndef TOOLBOX_H
#define TOOLBOX_H

#include "ui_toolbox.h"

class ToolBox : public QWidget, private Ui::ToolBox
{
    Q_OBJECT

public:
    ToolBox(QWidget *parent = 0);
};

#endif // TOOLBOX_H
