#ifndef ddd
#define ddd

#include <sigc++/sigc++.h>
#include <string>

class test
{
	public:
		test();
		void changeMe();
		typedef sigc::signal<void, std::string> type_test_signal;	/* type_test_signal is our signal type, typedef'd for easiness */
		type_test_signal signal_proxy();				/* this is our signal proxy (for other classes to get the signal and connect to it */

	protected:
		type_test_signal test_signal;					/* the signal itself that returns the type_test_signal */
};

#endif

