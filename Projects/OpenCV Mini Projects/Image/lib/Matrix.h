#import <objc/Object.h>
#import "Point.h"

#import <cv.h>

@interface Matrix: Object {
@protected
	CvMat	*matrix;
}

+ rotationAround: (PNT) point withAngle: (int) angle;

- (CvMat *) matrix;

@end

