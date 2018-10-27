# FIXME hard coded paths...
# FIXME autotools..
# i wish these things would use auto-tools :P

OGDF_LIB_DIR=$(HOME)/OGDF/_release
#OGDF_LIB_DIR=$(HOME)/OGDF/_debug
OGDF_INCLUDE_DIR=$(HOME)/OGDF

CFLAGS:=-Wall -g3
CFLAGS+=$(shell pkg-config --cflags x11 cairo) 
CFLAGS+=-I$(OGDF_INCLUDE_DIR) 

LDFLAGS:=$(shell pkg-config --libs-only-L x11 cairo) -L$(OGDF_LIB_DIR) 
LIBS:=$(shell pkg-config --libs-only-l x11 cairo) -lOGDF -lm

# uncomment these to enable COIN Osi
# you will need to have OGDF compiled with Osi Support also.
# not all layouts may work or work well without Osi
#
#OSI_INCLUDE_DIR=$(HOME)/Osi-0.98.2/include/coin
#OSI_LIB_DIR=$(HOME)/Osi-0.98.2/lib
#CFLAGS+=-DUSE_COIN -DCOIN_OSI_CLP -I$(OSI_INCLUDE_DIR)
#LDFLAGS+=-L$(OSI_LIB_DIR)
#LIBS+=-lOsiClp -lClp -lCoinUtils -lOsi 


#Uncomment to use an X surface instead of generating an svg and a png.
#CFLAGS+=-DUSE_X
#Uncomment to add some debugging helpers (like flushing after drawing)
#CFLAGS+=-DDEBUG


TOOLS=graph_draw graph_gen graph_layout graph_size

all: $(TOOLS)

#FIXME only graph_draw and graph_size need to link to cairo 

graph_gen: graph_gen.o
	g++ $(LDFLAGS) -o $@ $^ $(LIBS)

graph_layout: graph_layout.o
	g++ $(LDFLAGS) -o $@ $^ $(LIBS)

graph_draw: graph_draw.o
	g++ $(LDFLAGS) -o $@ $^ $(LIBS)

graph_size: graph_size.o
	g++ $(LDFLAGS) -o $@ $^ $(LIBS)

%.o: %.cxx
	g++ $(CFLAGS) -c $<

clean:
	rm -f *.o $(TOOLS) 
