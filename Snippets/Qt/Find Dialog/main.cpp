#include <QApplication>
#include <QHBoxLayout>
#include <QSlider>
#include <QSpinBox>

int main(int argc, char *argv[])
{
	QApplication app(argc, argv);
	QWidget *window = new QWidget;
	QSpinBox *spinbox = new QSpinBox;
	QSlider *slider = new QSlider(Qt::Horizontal);
	QHBoxLayout *layout = new QHBoxLayout;

	window->setWindowTitle("Age");
	spinbox->setRange(0, 100);
	slider->setRange(0, 100);

	QObject::connect(spinbox, SIGNAL(valueChanged(int)),
			slider, SLOT(setValue(int)));
	QObject::connect(slider, SIGNAL(valueChanged(int)),
			spinbox, SLOT(setValue(int)));

	spinbox->setValue(35);
	layout->addWidget(spinbox);
	layout->addWidget(slider);

	window->setLayout(layout);
	window->show();

	return app.exec();
}
