/*
 *	This file is part of OpenGrafik.
 *
 *	OpenGrafik is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	OpenGrafik is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with OpenGrafik.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "mainwindow.h"
#include "preferencesdialog.h"
#include "widgets/maintoolbox.h"
#include "widgets/propertyview.h"
#include "canvas/diagramview.h"

#include <QMdiArea>
#include <QSplitter>
#include <QSettings>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QCloseEvent>
#include <QDebug>
#include <QFileDialog>
#include <QDockWidget>

MainWindow::MainWindow(QWidget *parent):
    QMainWindow(parent),
    mdiArea(new QMdiArea(this)),
    mainToolBox(new MainToolBox(this)),
    mainSplitter(new QSplitter(this)),
    preferencesDialog(new PreferencesDialog(this)),
    propertyView(new PropertyView(this))
{
    setupUi(this);
    createCentralWidgets();
    updateShortcuts();
    updateMenusAndToolBox();

    connect(mdiArea, SIGNAL(subWindowActivated(QMdiSubWindow*)),
            this, SLOT(currentDiagramChanged(QMdiSubWindow*)));
    connect(preferencesDialog->useTabsCheckBox(), SIGNAL(toggled(bool)),
            this, SLOT(useTabsOptionSwitched(bool)));
#ifdef Q_WS_MAC
    connect(preferencesDialog->useUnifiedToolBarCheckBox(),
            SIGNAL(toggled(bool)),
            this, SLOT(useUnifiedToolBarOptionSwitched(bool)));
#endif

    readSettings();
    show();
}

void MainWindow::changeEvent(QEvent *e)
{
    QMainWindow::changeEvent(e);
    switch (e->type())
    {
    case QEvent::LanguageChange:
        retranslateUi(this);
        break;
    default:
        break;
    }
}

void MainWindow::createCentralWidgets()
{
    mdiArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    mdiArea->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);

    createMainToolBox();

    QDockWidget *dock = new QDockWidget(tr("Toolbox"), this);
    dock->setWidget(mainToolBox);
    addDockWidget(Qt::LeftDockWidgetArea, dock, Qt::Vertical);

    // mainSplitter->addWidget(mainToolBox);
    mainSplitter->addWidget(mdiArea);

    mainLayout->addWidget(mainSplitter);
}

void MainWindow::createMainToolBox()
{
    mainToolBox->getMainToolBox()->addItem(propertyView,
                                           QIcon(":icons/properties.png"),
                                           tr("Properties"));
}

void MainWindow::readSettings()
{
    QSettings settings;

    settings.beginGroup("MDIArea");
    preferencesDialog->useTabsCheckBox()->setChecked(
            settings.value("tabsMode", false).toBool());
    settings.endGroup();

    settings.beginGroup("ToolBox");
    mainSplitter->restoreState(settings.value("sizes").toByteArray());
    settings.endGroup();

    settings.beginGroup("MainWindow");
#ifdef Q_WS_MAC
    preferencesDialog->useUnifiedToolBarCheckBox().setChecked(
            settings.value("unifiedToolbar", false).toBool());
#endif
    setGeometry(settings.value("geometry", QRect(10, 10, 800, 600)).toRect());
    if (settings.value("maximized", false).toBool() == true)
        showMaximized();
    settings.endGroup();
}

void MainWindow::writeSettings()
{
    QSettings settings;

    settings.beginGroup("MainWindow");
#ifdef Q_WS_MAC
    settings.setValue("unifiedToolbar", unifiedTitleAndToolBarOnMac());
#endif
    settings.setValue("geometry", geometry());
    settings.setValue("maximized", isMaximized());
    settings.endGroup();

    settings.beginGroup("ToolBox");
    settings.setValue("sizes", mainSplitter->saveState());
    settings.endGroup();

    settings.beginGroup("MDIArea");
    settings.setValue("tabsMode", mdiArea->viewMode() == QMdiArea::TabbedView);
    settings.endGroup();

    settings.sync();
}

void MainWindow::updateShortcuts()
{
    actionNew->setShortcut(QKeySequence(QKeySequence::New));
    actionOpen->setShortcut(QKeySequence(QKeySequence::Open));
    actionSave->setShortcut(QKeySequence(QKeySequence::Save));
    actionSaveAs->setShortcut(QKeySequence(QKeySequence::SaveAs));
    actionQuit->setShortcut(QKeySequence("Ctrl+Q"));

    actionZoomIn->setShortcut(QKeySequence(QKeySequence::ZoomIn));
    actionZoomOut->setShortcut(QKeySequence(QKeySequence::ZoomOut));
}

DiagramView *MainWindow::currentDiagram() const
{
    return diagramFromSubWindow(mdiArea->activeSubWindow());
}

DiagramView *MainWindow::diagramFromSubWindow(QMdiSubWindow *window) const
{
    if (window != 0)
        return static_cast<DiagramView *>(window->widget());
    else
        return 0;
}

void MainWindow::currentDiagramChanged(QMdiSubWindow *window)
{
    updateMenusAndToolBox(diagramFromSubWindow(window));
}

void MainWindow::useTabsOptionSwitched(bool val)
{
    if (val == true)
        mdiArea->setViewMode(QMdiArea::TabbedView);
    else
        mdiArea->setViewMode(QMdiArea::SubWindowView);
}

#ifdef Q_WS_MAC
void MainWindow::useUnifiedToolBarOptionSwitched(bool val)
{
    this->setUnifiedTitleAndToolBarOnMac(val);
}
#endif

void MainWindow::updateMenusAndToolBox(DiagramView *diagram)
{
    int windowListSize = mdiArea->subWindowList().size();

    if (windowListSize <= 1)
    {
        if (windowListSize < 1)
            propertyView->clear();

        actionTile->setEnabled(false);
        actionCascade->setEnabled(false);
    }
    else
    {
        actionTile->setEnabled(true);
        actionCascade->setEnabled(true);
    }

    if (windowListSize == 0 || diagram == 0)
    {
        actionActualSize->setEnabled(false);
        actionBestFit->setEnabled(false);
        actionSave->setEnabled(false);
        actionSaveAs->setEnabled(false);
        actionZoomIn->setEnabled(false);
        actionZoomOut->setEnabled(false);
    }
    else
    {
        actionActualSize->setEnabled(true);
        actionBestFit->setEnabled(true);
        actionSave->setEnabled(diagram->isWindowModified() == true);
        actionSaveAs->setEnabled(true);
        actionZoomIn->setEnabled(true);
        actionZoomOut->setEnabled(true);
    }
}

void MainWindow::addNewDiagram(DiagramView *view)
{
    connect(view, SIGNAL(aboutToClose(DiagramView*,QCloseEvent*)),
            this, SLOT(diagramAboutToClose(DiagramView*,QCloseEvent*)));
    connect(view, SIGNAL(hasBeenModified(DiagramView*)),
            this, SLOT(updateMenusAndToolBox(DiagramView*)));

    QMdiSubWindow *subWindow = mdiArea->addSubWindow(view);
    QSize rect = subWindow->size();
    subWindow->resize(rect.width() / 2, rect.height() / 2);
    subWindow->setWindowIcon(QIcon(":icons/document.png"));
    view->show();

    propertyView->setItem(view->scene());
}

void MainWindow::diagramAboutToClose(DiagramView *diagram, QCloseEvent *event)
{
    if (diagram->isWindowModified() == true)
    {
        QMessageBox::StandardButton response =
        QMessageBox::StandardButton(
        QMessageBox::question(this, tr("Document Modified"),
        tr("The document %1 you are trying to close has some modifications. ")
        .arg(diagram->windowTitle().remove("[*]")) +
        tr("Would you like to save it first?"),
        QMessageBox::Yes, QMessageBox::No, QMessageBox::Cancel));

        if (response == QMessageBox::Yes)
        {
            if (saveDiagram(diagram) == true)
                event->accept();
            else
                event->ignore();
        }
        else if (response == QMessageBox::No)
            event->accept();
        else if (response == QMessageBox::Cancel)
            event->ignore();
        else
        {
            qDebug() << "MainWindow::diagramAboutToClose";
            qDebug() << "Should never be reached, please report a bug.";
        }
    }
}

bool MainWindow::diagramIsOpen(QString fileName)
{
    QList<QMdiSubWindow *> windowList = mdiArea->subWindowList();
    DiagramView *diagram;
    QMdiSubWindow *window;

    for (int i = 0; i < windowList.size(); i++)
    {
        window = windowList[i];
        diagram = dynamic_cast<DiagramView *>(window->widget());
        if (diagram->fileName() == fileName)
        {
            mdiArea->setActiveSubWindow(window);
            return true;
        }
    }

    return false;
}

void MainWindow::on_actionNew_triggered()
{
    addNewDiagram(new DiagramView(this));
}

void MainWindow::on_actionOpen_triggered()
{
    QStringList filenames = QFileDialog::getOpenFileNames(
            this, tr("Open Diagram"), "",
            tr("All Diagram Files (*.ogf);;Open Diagram Format (*.ogf)"));

    DiagramView *diagram;
    QString filename;

    for (int i = 0; i < filenames.size(); i++)
    {
        filename = filenames[i];

        if (diagramIsOpen(filename) != true)
        {
            diagram = new DiagramView(this);
            diagram->setFileName(filename);
            addNewDiagram(diagram);
        }
    }
}

bool MainWindow::saveDiagram(DiagramView *view)
{
    if (view->isUntitled() == true)
        return saveDiagramAs(view);
    else
        saveDiagramData(view);
    return true;
}

bool MainWindow::saveDiagramAs(DiagramView *view)
{
    QString name = QFileDialog::getSaveFileName(
            this, tr("Save Diagram As"), "",
            tr("All Diagram Files (*.ogf);;Open Diagram Format (*.ogf)"));

    if (name.isEmpty() == true)
        return false;

    view->setFileName(name);
    saveDiagramData(view);
    return true;
}

void MainWindow::saveDiagramData(DiagramView *view)
{
    // TODO get diagram XML data and save to filename
    // QString filename = view->fileName();

    view->setWindowModified(false);
}

void MainWindow::on_actionSave_triggered()
{
    saveDiagram(currentDiagram());
}

void MainWindow::on_actionSaveAs_triggered()
{
    saveDiagramAs(currentDiagram());
}

void MainWindow::on_actionQuit_triggered()
{
    close();
}

void MainWindow::closeEvent(QCloseEvent *event)
{
    quit();
    event->accept();
}

void MainWindow::quit()
{
    QList<QMdiSubWindow *> list = mdiArea->subWindowList();
    DiagramView *diagram;

    for (int i = 0; i < list.size(); i++)
    {
        diagram = diagramFromSubWindow(list[i]);
        diagram->close();
    }

    writeSettings();
}

void MainWindow::on_actionZoomIn_triggered()
{
    currentDiagram()->scale(2, 2);
}

void MainWindow::on_actionZoomOut_triggered()
{
    currentDiagram()->scale(0.5, 0.5);
}

void MainWindow::on_actionActualSize_triggered()
{
    currentDiagram()->setMatrix(QMatrix());
}

void MainWindow::on_actionBestFit_triggered()
{
    DiagramView *tmp = currentDiagram();
    tmp->fitInView(tmp->scene()->itemsBoundingRect(), Qt::KeepAspectRatio);
}

void MainWindow::on_actionPreferences_triggered()
{
    preferencesDialog->show();
}

void MainWindow::on_actionAbout_triggered()
{
    QMessageBox::about(this, tr("About Grafeo"),
    QString("Grafeo version %1\n\n").arg(VERSION) +
    tr("Released under the GPLv3.\n") +
    tr("Copyright (c) 2009 Grafeo Developers Team.\n\n") +
    "Frederic-Gerald Morcos\t<fred.morcos@gmail.com>\n" +
    "Michael Morckos\t<mikey.morckos@gmail.com>\n\n" +
    "http://gitorious.org/grafeo");
}

void MainWindow::on_actionTile_triggered()
{
    mdiArea->tileSubWindows();
}

void MainWindow::on_actionCascade_triggered()
{
    mdiArea->cascadeSubWindows();
}
