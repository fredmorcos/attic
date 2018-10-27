#include "testtask.h"
#include "testschedule.h"
#include "testoptimizer.h"

int main(int argc, char *argv[])
{
	TestTask testTask;
	QTest::qExec(&testTask, argc, argv);

	TestSchedule testSchedule;
	QTest::qExec(&testSchedule, argc, argv);

	TestOptimizer testOptimizer;
	QTest::qExec(&testOptimizer, argc, argv);
}
