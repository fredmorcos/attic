all:
	cd src; flex --header-file=lex.yy.h lexer.lex; cd ..; gcc src/*.c -o florence -lfl

clean:
	rm -f florence src/lex.yy.*

