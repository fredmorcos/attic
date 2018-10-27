"""
This module provides some extra, out-of-project-scope helper functions.
"""
def strToBool(s):
	"""
	Converts string s to a boolean
	"""
	assert type(s) == str or type(s) == unicode

	b_dict = {'true': True, 'false': False, 'yes': True, 'no': False}
	return b_dict[s.lower()]
