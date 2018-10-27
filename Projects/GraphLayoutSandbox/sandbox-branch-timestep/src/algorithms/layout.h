#import <objc/Object.h>
#import <graph.h>

typedef enum _AlgorithmType {
	ALG_TYPE_SIMPLE,
	ALG_TYPE_FR,
	ALG_TYPE_EADES,
	ALG_TYPE_GEM,
	ALG_TYPE_GRID,
	ALG_TYPE_BH
} AlgorithmType;

typedef struct _LayoutOps {
	AlgorithmType	type;
	int				nodesNum,
					edgesNum,
					frameWidth,
					frameHeight;
	double			repConst,
					attConst,
					timestep,
					damping;
	BOOL			useWall;
	List			*nodeList,
					*edgeList;
	Point			*energy;
} LayoutOps;

@interface Layout: Object {
}

+ forceLayout: (LayoutOps *) ops;

@end

