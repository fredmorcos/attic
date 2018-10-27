#include "enginetest.h"

void EngineTest::testSingletons()
{
	QCOMPARE(PreferencesManager::instance(),
			 PreferencesManager::instance());
	PreferencesManager *pm1 = PreferencesManager::instance(),
					   *pm2 = PreferencesManager::instance();
	QCOMPARE(pm1, pm2);

	QCOMPARE(DocumentManager::instance(),
			 DocumentManager::instance());
	DocumentManager *dm1 = DocumentManager::instance(),
					*dm2 = DocumentManager::instance();
	QCOMPARE(dm1, dm2);
}

QTEST_MAIN(EngineTest)

