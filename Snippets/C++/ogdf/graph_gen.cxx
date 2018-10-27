/*
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
Version 2 or later as published by the Free Software Foundation
and appearing in the file COPYING included in the packaging
of this file.

In addition, as a special exception, you have permission to link
this software with

  - the libraries of the COIN-OR Osi project (see
    http://www.coin-or.org/projects/Osi.xml);

  - and all LP-solver libraries directly supported by the
    COIN-OR Osi project,

and distribute executables, as long as you follow the requirements
of the GNU General Public License in regard to all of the software
in the executable aside from these third-party libraries.
*/

#include <ogdf/basic/graph_generators.h>
#include <ogdf/basic/Graph.h>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <getopt.h>
#include <inttypes.h>
using namespace ogdf;

char type;
unsigned long int randomSeed()
{
  
  unsigned int seed;
  struct timeval tv;
  FILE *devrandom;
  
  if ((devrandom = fopen("/dev/random","r")) == NULL)
    {
      gettimeofday(&tv,0);
      seed = tv.tv_sec + tv.tv_usec;
    }
  else
    {
      fread(&seed,sizeof(seed),1,devrandom);
      fclose(devrandom);
    }
  
  return(seed);
}

#define RAND(x,y) (x + (int)(y * (rand() / (RAND_MAX + (float)x))))
void generateGraph(ClusterGraph &cg, Graph &g, unsigned int count, unsigned int ax, unsigned int in)
{
  unsigned int seed = randomSeed();
  srand(seed);
  ax = RAND(in, ax);
  in = RAND(in, ax);
  if (type == 'b')
    {
      completeBipartiteGraph(g, ax, in);
    }
  if (type == 't')
    {
      randomTree(g, count, ax, in);
    }
  if (type == 'r')
    {
      randomGraph(g, count, ax);
    }
}


int main(int argc, char *argv[])
{
  Graph g;
  ClusterGraph cg(g);
  int xmin = 2;
  int xmax = 10;
  int count = 5; 
  int cluster = 0;
 
  while (1)
    {
      int xoptind = optind ? optind : 1;
      int option_index = 0;
      int c;
      char *endPtr;
      
      static struct option long_options[] =
        {
	  {"min", 1, 0, 0},
	  {"max", 1, 0, 0},
	  {"count", 1, 0, 0},
	  {"generator", 0, 0, 0},
	  {"type", 1, 0, 0},
	  {"o", 0, 0, 0}
	};
      
      c = getopt_long(argc, argv, "lc:t:n:x:",
		      long_options, &option_index);
      if (c == -1)
        break;
      
      switch(c)
        {
	case 'n':
	    xmin = strtoimax(argv[xoptind+1], &endPtr, 10);
	    break;
	case 'x':
	  xmax = strtoimax(argv[xoptind+1], &endPtr, 10);
	  //xmax = argv[optind];
	  break;
	case 'c':
	  count = strtoimax(argv[xoptind+1], &endPtr, 10);
	  //xmax = argv[optind];
	case 't':
	  type = argv[xoptind+1][0];
	  break;
	case 'l':
	  cluster = 1;
	  break;
	case 'g':
	  break;
	  case 'o':
	    break;
	default:
	  break;
	}
    }
  generateGraph(cg, g, count, xmax, xmin);
  if (cluster)
    {
      randomClusterGraph(cg, g, 2);
      cg.writeGML(cout);
    }
  else
    {
      g.writeGML(cout);
    }
}
