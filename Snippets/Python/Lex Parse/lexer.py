import sys
from token import Token

c = ''
t = None

def lexer_get_next_token():
	c = ''
	t = Token()

	while True:
		c = sys.stdin.read(1)

		if len(c) <= 0:
			t.type = 'EOF'
		elif c.isspace():
			sys.stdin.
