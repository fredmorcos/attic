#include "mainwindow.h"

#include "engine/node.h"
#include "engine/edge.h"
#include "engine/graph.h"

MainWindow::MainWindow(QWidget *parent):
	QMainWindow(parent)
{
    setupUi(this);

	scene = new Graph(graphicsView);
	graphicsView->setScene(scene);

	showMaximized();
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

void MainWindow::on_selectButton_toggled(bool checked)
{
	if (checked == true)
		scene->setState(Graph::None);
}

void MainWindow::on_nodesButton_toggled(bool checked)
{
	if (checked == true)
		scene->setState(Graph::AddingNodes);
}

void MainWindow::on_edgesButton_toggled(bool checked)
{
	if (checked == true)
		scene->setState(Graph::AddingEdges);
}
