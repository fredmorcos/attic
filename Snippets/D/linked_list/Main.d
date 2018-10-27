module test.Main;

import List;
import tango.io.Stdout;

/**
  Entrance function
*/
int main (char[][] args) {
	List.List!(int) *x = new List.List!(int);

	for (int i = 0; i < 10; i++) {
		x.append(i);
	}

	x.goToFirst();

	for (int i = 0; i < 5; i++) {
		x.next();
	}

	List.List!(int) *y = x.getSubList();

	do {
		Stdout.formatln("{}", y.getCurrent());
	} while (y.next())

	return 0;
}

