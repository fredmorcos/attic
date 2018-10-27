#!/bin/sh

TARGET=hllc
SRCS=`find . -iname "*.c" | grep -i -v "\.yy\.c\|\.tab\.c" | sed -e 's/[0-9]*\://' | sed -e 's/^\.\///'`
OBJS=`echo $SRCS | sed -e 's/\.c/\.o/'`

if [ "$1" == "clean" ]; then
    rm -rf $OBJS lex.yy.c lex.o par.tab.c par.tab.h par.o $TARGET
    
    if [ $? -ne 0 ]; then
	exit 1
    fi

    exit 0
fi

echo "compiling lex.l"
$FLEX lex.l

if [ $? -ne 0 ]; then
    exit 1
fi

echo "compiling par.y"
$BISON -d par.y

if [ $? -ne 0 ]; then
    exit 1
fi

echo "compiling lex.yy.c"
$CC -c -o lex.o lex.yy.c

if [ $? -ne 0 ]; then
    exit 1
fi

echo "compiling par.tab.c"
$CC -c -o par.o par.tab.c

if [ $? -ne 0 ]; then
    exit 1
fi

for i in $SRCS
do
    echo "compiling $i"
    $CC $CFLAGS -c -o `echo $i | sed -e 's/\.c/\.o/'` $i

    if [ $? -ne 0 ]; then
	exit 1
    fi
done

echo "linking $TARGET"
$CC $LDFLAGS lex.o par.o $OBJS -o $TARGET

if [ $? -ne 0 ]; then
    exit 1
fi

exit 0