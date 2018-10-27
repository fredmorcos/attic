#include <QCoreApplication>
#include <QtSql>
#include <QDebug>
#include <QStringList>
#include <QVariant>

int main(int argc, char *argv[])
{
	QFile file("/home/fred/workspace/dbtest/test.jpg");
	bool f = file.open(QFile::ReadOnly);
	if (f == false)
		qDebug() << "cannot open file";
	QByteArray arr = file.readAll();
	file.close();
	QString ss(arr);

	qDebug() << ss;

	QSqlDatabase db = QSqlDatabase::addDatabase("QSQLITE");
	db.setDatabaseName("testdb");
	bool ok = db.open();

	QSqlQuery q("select first_name,last_name from person where first_name like 'hild_'");
	while(q.next())
		qDebug() << q.value(0).toString() << q.value(1).toString();

	q.exec("insert into person values (6,'my','picture')");
	// QString s = QString("insert into person values (6,%1,'picture')").arg(ss);
	// QString s = QString("insert into person values (6,'foo','picture')");

	// qDebug() << s;
	// q.exec(s);

	q.exec("select first_name from person where last_name='picture'");
	while(q.next())
		qDebug() << q.value(0).toByteArray();

	db.close();
}
