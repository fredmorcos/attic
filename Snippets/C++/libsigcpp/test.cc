#include "test.h"

test::test()
{
}

test::type_test_signal test::signal_proxy()
{
	return test_signal;
}

void test::changeMe()
{
	test_signal.emit("I was changed!!!");
}

