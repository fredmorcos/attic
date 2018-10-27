#!/usr/bin/python

import os, sys, stat, shutil

if len(sys.argv) < 2:
	print "Usage:"
	print "\tpython defrag.py <directory>"
	print "\t./defrag.py <directory>"
	sys.exit(1)

print "Counting files..."

fragdir = os.path.abspath(sys.argv[1])
fragfiles = []
tmpfile = os.path.join(fragdir, "defrag_py_tmp")

for root, dirs, files in os.walk(fragdir, topdown = False):
	for f in files:
		fn = os.path.join(root, f)
		st = os.stat(fn)
		if st[stat.ST_CTIME] <= st[stat.ST_MTIME]:
			fragfiles.append((fn, st[stat.ST_SIZE]))

if len(fragfiles) == 0:
	print "No need for defragmentation."
	sys.exit(0)

fragfiles.sort(lambda a, b: cmp(b[1], a[1]))

for f in fragfiles:
	print "Defragmenting file", f[0], "of size", int(f[1])
	shutil.copy(f[0], tmpfile)
	os.remove(f[0])
	shutil.copy(tmpfile, f[0])
	os.remove(tmpfile)
