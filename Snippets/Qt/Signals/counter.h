#include <QObject>

class counter : public QObject
{
	Q_OBJECT
	
	public:
		counter ();
		int getValue ();
		
	public slots:
		void setValue (int newValue);
		
	signals:
		void valueChanged (int newValue);
		
	private:
		int value;
};
