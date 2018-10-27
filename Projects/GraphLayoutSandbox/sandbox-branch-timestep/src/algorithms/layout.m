#import "layout.h"
#import "layout-fr.h"
#import "layout-simple.h"
#import <graph.h>
//#import <stdlib.h>
#import <math.h>

@implementation Layout

+ forceLayout: (LayoutOps *) ops {
	switch(ops->type) {
		case ALG_TYPE_SIMPLE:
			[LayoutSimple layout: ops];
			break;
		case ALG_TYPE_FR:
			[LayoutFR layout: ops];
			break;
		case ALG_TYPE_EADES:
			break;
		case ALG_TYPE_GEM:
			break;
		case ALG_TYPE_GRID:
			break;
		case ALG_TYPE_BH:
			break;
	}
	
	return self;
}

@end

