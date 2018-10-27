"""
This module provides some extra, out-of-project-scope helper functions.
"""
import unittest

def strToBool(s):
	"""
	Converts string s to a boolean
	"""
	assert type(s) == str or type(s) == unicode

	b_dict = {'true': True, 'false': False, 'yes': True, 'no': False}
	return b_dict[s.lower()]

def valFromPercent(percentage, total):
	"""
	Returns the value that percentage represents in terms of total.
	"""
	return total * percentage / 100.0

class TestExtra(unittest.TestCase):
	def test_strToBool(self):
		self.assertEqual(strToBool("True"), True)
		self.assertEqual(strToBool("false"), False)
		self.assertEqual(strToBool("yes"), True)
		self.assertEqual(strToBool("No"), False)

	def test_valFromPercent(self):
		for i in range(1000):
			self.assertEqual(valFromPercent(i, 100), i)
		self.assertEqual(valFromPercent(5, 2700), 135)

if __name__ == "__main__":
	unittest.main()
