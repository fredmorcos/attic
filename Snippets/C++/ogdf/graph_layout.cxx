/*
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
Version 2 or later as published by the Free Software Foundation
and appearing in the file COPYING and included in the packaging
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

#include <ogdf/planarity/PlanarizationLayout.h>
    
#include <ogdf/planarity/VariableEmbeddingInserter.h>
#include <ogdf/planarity/FastPlanarSubgraph.h>
#include <ogdf/orthogonal/OrthoLayout.h>
#include <ogdf/planarity/EmbedderMinDepthMaxFaceLayers.h>

#include <ogdf/energybased/FMMMLayout.h>

#include <ogdf/lpsolver/LPSolver.h>

#include <ogdf/layered/OptimalHierarchyLayout.h>
#include <ogdf/layered/SugiyamaLayout.h>
#include <ogdf/layered/OptimalRanking.h>
#include <ogdf/layered/MedianHeuristic.h>

#include <ogdf/misclayout/BalloonLayout.h>
#include <ogdf/tree/TreeLayout.h>
#include <ogdf/tree/RadialTreeLayout.h>
#include <ogdf/energybased/CoinTutteLayout.h>
#include <ogdf/misclayout/CircularLayout.h>

#include <getopt.h>
using namespace ogdf;

void orth_layoutGraph(Graph &g, GraphAttributes &ga, double w, double h, int preserve)
{

  // Create the planarization layout algorithm
  PlanarizationLayout pl;
 
  // Set options for crossing minimization
  // (Defaults would be ok!)
  FastPlanarSubgraph *ps = new FastPlanarSubgraph;
  ps->runs(100);
  VariableEmbeddingInserter *ves = new VariableEmbeddingInserter;
  ves->removeReinsert(EdgeInsertionModule::rrAll);
  pl.setSubgraph(ps);
  pl.setInserter(ves);
  
  // Set options for planar embedder
  // (Defaults would be ok!)
  EmbedderMinDepthMaxFaceLayers *emb = new EmbedderMinDepthMaxFaceLayers;
  pl.setEmbedder(emb);
  
  // Set options for planar layout algorithm
  // (Seperation should be set, otherwise defaults are ok)
  OrthoLayout *ol = new OrthoLayout;
  ol->preferedDir(ogdf::odNorth);
  ol->separation(12.0);
  ol->cOverhang(12.0);
  ol->setOptions(2+4);
  ol->margin(5);
  ol->scaling(true);
  pl.setPlanarLayouter(ol);
  // Call orthogonal layout with planarization
  pl.call(ga);
}

void energy_layoutGraph(Graph &g, GraphAttributes &ga, double w, double h, bool preserve)
{
  FMMMLayout fmmm;
  
  fmmm.useHighLevelOptions(true);
  fmmm.unitEdgeLength(w + h * 2);
  fmmm.newInitialPlacement(preserve);
  fmmm.qualityVersusSpeed(FMMMLayout::qvsGorgeousAndEfficient);
  
  fmmm.call(ga);
}

void sugiyama_layoutGraph(Graph &g, GraphAttributes &ga, double w, double h, bool preserve)
{
  SugiyamaLayout SL;
  
  SL.setRanking(new OptimalRanking); 
  SL.setCrossMin(new MedianHeuristic);
#ifdef USE_COIN
  OptimalHierarchyLayout *ohl = new OptimalHierarchyLayout;
  ohl->layerDistance(12.0);
  ohl->nodeDistance(8.0);
  ohl->weightBalancing(25.8);
  SL.setLayout(ohl);
#endif

  SL.call(ga);
}

void tutte_layoutGraph(Graph &g, GraphAttributes &ga, double w, double h, bool preserve)
{
  TutteLayout tl;

  //tl.bbox(DRect(0,0,100,100)); 
  tl.call(ga);
}
void balloon_layoutGraph(Graph &g, GraphAttributes &ga, double w, double h, bool preserve)
{
  BalloonLayout l;

  l.setEvenAngles(false);
  l.callFractal(ga,0.3);
}


void tree_layoutGraph(Graph &g, GraphAttributes &ga, double w, double h, bool preserve)
{
  TreeLayout l;

  l.call(ga);
}

void radialtree_layoutGraph(Graph &g, GraphAttributes &ga, double w, double h, bool preserve)
{
  RadialTreeLayout l;

  l.call(ga);
}

void circular_layoutGraph(Graph &g, GraphAttributes &ga, double w, double h, bool preserve)
{
  CircularLayout l;

  l.call(ga);
}

int main(int argc, char *argv[])
{
  
  Graph g;
  GraphAttributes ga(g,
		     GraphAttributes::nodeGraphics | GraphAttributes::edgeGraphics |
		     GraphAttributes::nodeLabel | GraphAttributes::nodeColor |
		     GraphAttributes::edgeColor | GraphAttributes::edgeStyle |
		     GraphAttributes::nodeStyle | GraphAttributes::nodeTemplate |
		     GraphAttributes::edgeArrow);
  
  double w = 5.0;
  double h = 5.0;
  bool preserve = false; 
  char layout='e';

  while (1)
    {
      int xoptind = optind ? optind : 1;
      int option_index = 0;
      int c;
      char *endPtr;
      static struct option long_options[] = {
	{"width", 1, 0, 'w'},
	{"height", 1, 0, 'h'},
	  {"o", 1, 0, 0},
	{"layout", 1, 0, 'l'},
	{"preserve", 0, 0, 0},
      };

      c = getopt_long(argc, argv, "w:h:o:l:",
		      long_options, &option_index);
      if (c == -1)
	break;
      
      switch(c)
        {
	case 'w':
	  w = strtod(argv[xoptind+1], &endPtr);
	  ga.setAllWidth(w);
	  break;
	  case 'h':
	    h = strtod(argv[xoptind+1], &endPtr);
	    ga.setAllHeight(h);
	    break;
	case 'l':
	  layout=argv[xoptind+1][0];
	  break;
	case 'p':
	  preserve=true;
	  break;
	default:
	  break;
	}
    }
  
  if (optind < argc)
    {
#if 0
      fprintf(stderr, "non-option ARGV-elements: ");
      while (optind < argc)
        fprintf(stderr, "%s ", argv[optind++]);
      fprintf(stderr, "\n");
#endif
      ga.readGML(g,argv[optind++]);
    }
  else
    {
      ga.readGML(g,cin);
    }

  node v;
  forall_nodes(v, g)
    {
      if (!ga.nodeLine(v).length())
      {
	ga.nodeLine(v) = "#000000";
	ga.colorNode(v) = "#ffffff";
      }
    }
  
  switch (layout) {
  case 'o':
    orth_layoutGraph(g, ga, w, h, preserve);
    break;
  case 'e':
    energy_layoutGraph(g, ga, w, h, preserve);
    break;
  case 's':
    sugiyama_layoutGraph(g, ga, w, h, preserve);
    break;
  case 't':
    tutte_layoutGraph(g, ga, w, h, preserve);
    break;
  case 'b':
    balloon_layoutGraph(g, ga, w, h, preserve);
    break;
  case 'T':
    tree_layoutGraph(g, ga, w, h, preserve);
    break;
  case 'r':
    radialtree_layoutGraph(g, ga, w, h, preserve);
    break;
  case 'c':
    circular_layoutGraph(g, ga, w, h, preserve);
    break;

  }
  
  ga.writeGML(cout);
  
  return 0;
}
