#include <stdio.h>

int main() {
	short signed int x = how_deep("welcome");
	printf("result: %d\n", x);
	return 0;
}

int how_deep (char* nested) {
	char stack[50];
	unsigned int sc = 0;
	
	while (nested[0] != '\0') {
		if (nested[0] == '[' ||
		nested[0] == '(' ||
		nested[0] == '<' ||
		nested[0] == '{') {
			stack[++sc] = ++nested[0];
		}
		else if (nested[0] == '/') {
			if (nested[1] == '*') {
				stack[++sc] = nested[0];
				nested += 2;
			}
			else {
				break;
			}
		}
		else if (nested[0] == ']') {
			if (nested[sc - 1] == '[') {
				--sc;
				++nested;
			}
			else {
				return -1;
			}
		}
	}
	printf ("%c\n", nested[0]);
	if (sc == 0) {
		return 1;
	}
	else {
		return -1;
	}
}
