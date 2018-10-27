#!/usr/bin/env python

import sys
import time

for i in xrange(10):
	sys.stdout.write("\r[{0}>{1}]".format("="*i, " "*(9 - i)))
	sys.stdout.flush()
	time.sleep(0.5)
