#!/usr/bin/python

# Frederic-Gerald Morcos 4-1805 E11

import sys

if len(sys.argv) < 3:
	print "Usage: ./des.py <enc|dec> <key> <plaintext>"
	sys.exit(0)

enc = (sys.argv[1] == "enc")
key = sys.argv[2]
msg = ""

for i in range(3, len(sys.argv)):
	msg += str(sys.argv[i])

while len(key) % 8:
	key += "x"

while len(msg) % 8:
	msg += "x"

print "Key:", key
print "Plaintext:", msg

# Tables taken from pyDES v2.0 by Todd Whiteman

# Permutation and translation tables for DES
__pc1 = [56, 48, 40, 32, 24, 16,  8,
	  0, 57, 49, 41, 33, 25, 17,
	  9,  1, 58, 50, 42, 34, 26,
	 18, 10,  2, 59, 51, 43, 35,
	 62, 54, 46, 38, 30, 22, 14,
	  6, 61, 53, 45, 37, 29, 21,
	 13,  5, 60, 52, 44, 36, 28,
	 20, 12,  4, 27, 19, 11,  3
]

# number left rotations of pc1
__left_rotations = [
	1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1
]

# permuted choice key (table 2)
__pc2 = [
	13, 16, 10, 23,  0,  4,
	 2, 27, 14,  5, 20,  9,
	22, 18, 11,  3, 25,  7,
	15,  6, 26, 19, 12,  1,
	40, 51, 30, 36, 46, 54,
	29, 39, 50, 44, 32, 47,
	43, 48, 38, 55, 33, 52,
	45, 41, 49, 35, 28, 31
]

# initial permutation IP
__ip = [57, 49, 41, 33, 25, 17, 9,  1,
	59, 51, 43, 35, 27, 19, 11, 3,
	61, 53, 45, 37, 29, 21, 13, 5,
	63, 55, 47, 39, 31, 23, 15, 7,
	56, 48, 40, 32, 24, 16, 8,  0,
	58, 50, 42, 34, 26, 18, 10, 2,
	60, 52, 44, 36, 28, 20, 12, 4,
	62, 54, 46, 38, 30, 22, 14, 6
]

# Expansion table for turning 32 bit blocks into 48 bits
__expansion_table = [
	31,  0,  1,  2,  3,  4,
	 3,  4,  5,  6,  7,  8,
	 7,  8,  9, 10, 11, 12,
	11, 12, 13, 14, 15, 16,
	15, 16, 17, 18, 19, 20,
	19, 20, 21, 22, 23, 24,
	23, 24, 25, 26, 27, 28,
	27, 28, 29, 30, 31,  0
]

# The (in)famous S-boxes
__sbox = [
	# S1
	[14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7,
	 0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8,
	 4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0,
	 15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13],

	# S2
	[15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10,
	 3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5,
	 0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15,
	 13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9],

	# S3
	[10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8,
	 13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1,
	 13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7,
	 1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12],

	# S4
	[7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15,
	 13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9,
	 10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4,
	 3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14],

	# S5
	[2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9,
	 14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6,
	 4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14,
	 11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3],

	# S6
	[12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11,
	 10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8,
	 9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6,
	 4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13],

	# S7
	[4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1,
	 13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6,
	 1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2,
	 6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12],

	# S8
	[13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7,
	 1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2,
	 7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8,
	 2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11],
]

# 32-bit permutation function P used on the output of the S-boxes
__p = [
	15, 6, 19, 20, 28, 11,
	27, 16, 0, 14, 22, 25,
	4, 17, 30, 9, 1, 7,
	23,13, 31, 26, 2, 8,
	18, 12, 29, 5, 21, 10,
	3, 24
]

# final permutation IP^-1
__fp = [
	39,  7, 47, 15, 55, 23, 63, 31,
	38,  6, 46, 14, 54, 22, 62, 30,
	37,  5, 45, 13, 53, 21, 61, 29,
	36,  4, 44, 12, 52, 20, 60, 28,
	35,  3, 43, 11, 51, 19, 59, 27,
	34,  2, 42, 10, 50, 18, 58, 26,
	33,  1, 41,  9, 49, 17, 57, 25,
	32,  0, 40,  8, 48, 16, 56, 24
]

keys = []
left = []
right = []

def chartobin(c):
	res = []
	i = ord(c)
#	print "chartobin:\n\tchar:", c, "\n\tint:", i
	while i / 2:
		res.append(i % 2)
		i /= 2
	res.append(i % 2)
	res.reverse()
	while len(res) < 8:
		res.insert(0, 0)
#	print "\tbin:", res
	return res

def strtobin(s):
	res = []
	for c in s:
		res.extend(chartobin(c))
#	print "strtobin:\n\tstr:", s, "\n\tbin:", res, "\n\tlen:", len(res)
	return res

def bintochar(b):
	b = b[::-1]
	res = 0
	for j, i in enumerate(b):
		if i == 1:
			res += 2**j
	return res

def bintostr(l):
	i = 0
	res = ""
	while i < len(l):
		res += chr(bintochar(l[i:i+8]))
		i += 8
	return res

def permute(table, block):
	return list(map(lambda x: block[x], table))

def createkeys():
#	l = left
#	r = right

	permkey = permute(__pc1, strtobin(key))
	l = permkey[:28]
	r = permkey[28:]
	i = 0
	while i < 16:
		j = 0
		# circular left shifts
		while j < __left_rotations[i]:
			l.append(l[0])
			del l[0]
			r.append(r[0])
			del r[0]
			j += 1

		keys.append(permute(__pc2, l + r))
		# keys[i] = permute(__pc2, l + r)
		i += 1

def blockcrypt(block):
#	l = left
#	r = right

	permblock = permute(__ip, block)
	l = permblock[:32]
	r = permblock[32:]

	# encrypt or decrypt
	if enc == True:
		start = 0
		inc = 1
	else:
		start = 15
		inc = -1

	i = 0
	while i < 16:
		# we will need r later to become l
		tr = r[:]

		# permute r[i - 1] to become r[i]
		r = permute(__expansion_table, r)

		# xor
		r = list(map(lambda x, y: x ^ y, r, keys[start]))
		b = [r[:6], r[6:12], r[12:18], r[18:24], r[24:30], r[30:36], r[36:42], r[42:]]

		# permute using s-boxes
		j = 0
		bn = [0] * 32
		pos = 0
		while j < 8:
			m = (b[j][0] << 1) + b[j][5]
			n = (b[j][1] << 3) + (b[j][2] << 2) + (b[j][3] << 1) + b[j][4]

			# permutation value
			v = __sbox[j][(m << 4) + n]
			bn[pos] = (v & 8) >> 3
			bn[pos + 1] = (v & 4) >> 2
			bn[pos + 2] = (v & 2) >> 1
			bn[pos + 3] = v & 1

			pos += 4
			j += 1

		r = permute(__p, bn)
		r = list(map(lambda x, y: x ^ y, r, l))
		l = tr

		i += 1
		start += inc

	final = permute(__fp, r + l)
	return final

def crypt():
	i = 0
	result = []
	res = ""
	while i < len(msg):
		binblock = strtobin(msg[i:i + 8])
		cryptedblock = blockcrypt(binblock)
		result.append(bintostr(cryptedblock))
		i += 8

	for i in result:
		res += i

	return res

createkeys()
print "Ciphertext:", crypt()
