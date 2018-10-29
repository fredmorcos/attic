CSC  ?= csc
SRCS  = file.scm
OBJS  = $(SRCS:.scm=.o)

.PHONY: all clean

all: backyd

%.o: %.scm
	$(CSC) -c $<

%: $(OBJS) %.o
	$(CSC) -o $@ $^

clean:
	rm -rf $(OBJS) backyd.o backyc.o backyd backyc
