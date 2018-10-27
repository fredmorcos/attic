CXXFLAGS =	-O2 -g -Wall
OBJS =		cv4.o
LIBS =		`pkg-config --libs --cflags opencv`
TARGET =	cv4

$(TARGET):	$(OBJS)
	$(CXX) -o $(TARGET) $(OBJS) $(LIBS)

all:	$(TARGET)

clean:
	rm -f $(OBJS) $(TARGET)

