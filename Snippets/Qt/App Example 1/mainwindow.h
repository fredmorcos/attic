/****************************************************************************
 **
 ** Copyright (C) 2009 Nokia Corporation and/or its subsidiary(-ies).
 ** Contact: Nokia Corporation (qt-info@nokia.com)
 **
 ** This file is part of the examples of the Qt Toolkit.
 **
 ** $QT_BEGIN_LICENSE:LGPL$
 ** Commercial Usage
 ** Licensees holding valid Qt Commercial licenses may use this file in
 ** accordance with the Qt Commercial License Agreement provided with the
 ** Software or, alternatively, in accordance with the terms contained in
 ** a written agreement between you and Nokia.
 **
 ** GNU Lesser General Public License Usage
 ** Alternatively, this file may be used under the terms of the GNU Lesser
 ** General Public License version 2.1 as published by the Free Software
 ** Foundation and appearing in the file LICENSE.LGPL included in the
 ** packaging of this file.  Please review the following information to
 ** ensure the GNU Lesser General Public License version 2.1 requirements
 ** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
 **
 ** In addition, as a special exception, Nokia gives you certain
 ** additional rights. These rights are described in the Nokia Qt LGPL
 ** Exception version 1.0, included in the file LGPL_EXCEPTION.txt in this
 ** package.
 **
 ** GNU General Public License Usage
 ** Alternatively, this file may be used under the terms of the GNU
 ** General Public License version 3.0 as published by the Free Software
 ** Foundation and appearing in the file LICENSE.GPL included in the
 ** packaging of this file.  Please review the following information to
 ** ensure the GNU General Public License version 3.0 requirements will be
 ** met: http://www.gnu.org/copyleft/gpl.html.
 **
 ** If you are unsure which license is appropriate for your use, please
 ** contact the sales department at http://www.qtsoftware.com/contact.
 ** $QT_END_LICENSE$
 **
 ****************************************************************************/

 #ifndef MAINWINDOW_H
 #define MAINWINDOW_H

 #include <QMainWindow>

 #include "diagramitem.h"

 class DiagramScene;

 class QAction;
 class QToolBox;
 class QSpinBox;
 class QComboBox;
 class QFontComboBox;
 class QButtonGroup;
 class QLineEdit;
 class QGraphicsTextItem;
 class QFont;
 class QToolButton;
 class QAbstractButton;
 class QGraphicsView;

 class MainWindow : public QMainWindow
 {
     Q_OBJECT

 public:
    MainWindow();

 private slots:
     void backgroundButtonGroupClicked(QAbstractButton *button);
     void buttonGroupClicked(int id);
     void deleteItem();
     void pointerGroupClicked(int id);
     void bringToFront();
     void sendToBack();
     void itemInserted(DiagramItem *item);
     void textInserted(QGraphicsTextItem *item);
     void currentFontChanged(const QFont &font);
     void fontSizeChanged(const QString &size);
     void sceneScaleChanged(const QString &scale);
     void textColorChanged();
     void itemColorChanged();
     void lineColorChanged();
     void textButtonTriggered();
     void fillButtonTriggered();
     void lineButtonTriggered();
     void handleFontChange();
     void itemSelected(QGraphicsItem *item);
     void about();

 private:
     void createToolBox();
     void createActions();
     void createMenus();
     void createToolbars();
     QWidget *createBackgroundCellWidget(const QString &text,
                                         const QString &image);
     QWidget *createCellWidget(const QString &text,
                               DiagramItem::DiagramType type);
     QMenu *createColorMenu(const char *slot, QColor defaultColor);
     QIcon createColorToolButtonIcon(const QString &image, QColor color);
     QIcon createColorIcon(QColor color);

     DiagramScene *scene;
     QGraphicsView *view;

     QAction *exitAction;
     QAction *addAction;
     QAction *deleteAction;

     QAction *toFrontAction;
     QAction *sendBackAction;
     QAction *aboutAction;

     QMenu *fileMenu;
     QMenu *itemMenu;
     QMenu *aboutMenu;

     QToolBar *textToolBar;
     QToolBar *editToolBar;
     QToolBar *colorToolBar;
     QToolBar *pointerToolbar;

     QComboBox *sceneScaleCombo;
     QComboBox *itemColorCombo;
     QComboBox *textColorCombo;
     QComboBox *fontSizeCombo;
     QFontComboBox *fontCombo;

     QToolBox *toolBox;
     QButtonGroup *buttonGroup;
     QButtonGroup *pointerTypeGroup;
     QButtonGroup *backgroundButtonGroup;
     QToolButton *fontColorToolButton;
     QToolButton *fillColorToolButton;
     QToolButton *lineColorToolButton;
     QAction *boldAction;
     QAction *underlineAction;
     QAction *italicAction;
     QAction *textAction;
     QAction *fillAction;
     QAction *lineAction;
 };

 #endif
