#!/usr/bin/python

# Frederic-Gerald Morcos E11

keymat = []

def keymat_get(row, col):
	return keymat[((5 * row) + col)]

def keymat_pos(c):
	pos = keymat.index(c)
	row = (int) (pos / 5)
	col = pos - (row * 5)
	return (row, col)

def keymat_circrow(r, c):
	if c == 5:
		c = 0
	return keymat_get(r, c)

def keymat_circcol(r, c):
	if r == 5:
		r = 0
	return keymat_get(r, c)

import sys

if len(sys.argv) < 3:
	print "Usage: ./playfair-encrypt.py <key> <plaintext>"
	sys.exit(0)

alps = map(chr, range(97, 123));
# alpc = map(chr, range(65, 91));

key = sys.argv[1]
msg = ""

for i in range(2, len(sys.argv)):
	msg += str(sys.argv[i])

print "Key:", key
print "Plaintext:", msg

keymat = list(key)[:]

for obj in alps:
	if obj is 'i':
		continue
	if obj not in keymat:
		keymat.append(obj)

msgp = []
i = 0
while True:
	if i == len(msg) - 1:
		msgp.append((msg[i], 'x'))
		break
	else:
		if i == len(msg) - 2:
			if msg[i] == msg[i + 1]:
				msgp.append((msg[i], 'x'))
				i += 1
				continue
			else:
				msgp.append((msg[i], msg[i + 1]))
				break

	if msg[i] == msg[i + 1]:
		msgp.append((msg[i], 'x'))
		i += 1
	else:
		msgp.append((msg[i], msg[i + 1]))
		i += 2

cipher = ""
# print "Char\tRow\tCol"
for i in msgp:
	if i[0] is "i":
		i = ("y", i[1])
	if i[1] is "i":
		i = (i[0], "y")

	row0, col0 = keymat_pos(i[0])
	row1, col1 = keymat_pos(i[1])

#	print i[0], "\t", row0, "\t", col0
#	print i[1], "\t", row1, "\t", col1

	if (row0 == row1):
		x = keymat_circrow(row0, col0 + 1)
		y = keymat_circrow(row1, col1 + 1)
		cipher += x
		cipher += y
#		print "In same row, replaced by:", x, y
	else:
		if (col0 == col1):
			x = keymat_circcol(row0 + 1, col0)
			y = keymat_circcol(row1 + 1, col1)
			cipher += x
			cipher += y
#			print "In same col, replaced by:", x, y
		else:
			x = keymat_get(row0, col1)
			y = keymat_get(row1, col0)
			cipher += x
			cipher += y
#			print "Neither, replaced by:", x, y

print "Ciphertext:", cipher
