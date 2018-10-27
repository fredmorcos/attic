all:
	as base64enc.s -o base64enc.o
	ld base64enc.o -o base64enc

debug:
	as -gstabs base64enc.s -o base64enc.o
	ld base64enc.o -o base64enc

clean:
	rm base64enc.o base64enc

