#include <cilk/cilk.h>

void cilk_test (int n)
{
	if (n < 0) 
		return;
	else {
		cilk_spawn cilk_test(n - 1);
		cilk_spawn cilk_test(n - 2);
		cilk_sync;
	}
}
