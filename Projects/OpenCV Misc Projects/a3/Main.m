#import <stdio.h>
#import <objc/objc.h>
#import "Image.h"
#import "Window.h"

int main (int argc, char *argv[]) {
	id	srcImg,
		dstImg,
		srcWin,
		dstWin,
		dstImg2,
		dstWin2;
	int	t1,
		t2;

	if (argc < 2) {		/* incorrect number of parameters */
help:
		printf("Usage: ./cv2 <image-filename> [T1-H T2-L]\n");
		printf("Passing a binary image will apply Connected\n");
		printf("Component Labeling and passing a grayscale\n");
		printf("image will apply Canny Edge Detection. For\n");
		printf("Canny, two threshold parameters should be\n");
		printf("passed (T1-High and T2-Low).\n");
		exit(1);
	}

	srcImg = [[Image alloc] initFromFile: argv[1]];

	if (![srcImg image]) {
		printf("File not found.\n");
		exit(1);
	}

	if ([srcImg isBin]) {
		/* connected component labeling */
		[[srcImg ccl: "output.txt"] free];
		goto quit;
	}
	else if ([srcImg isGS]) {
		/* canny edge detector */
		if (argc < 4)
			goto help;
		t1 = MAX(atoi(argv[2]), atoi(argv[3]));
		t2 = MIN(atoi(argv[2]), atoi(argv[3]));
		dstImg = [srcImg applyCannyWithT1: t1 andT2: t2];
		dstImg2 = [srcImg applyOpenCVCannyWithT1: t1 andT2: t2];
		srcWin = [[[Window alloc] initWithName: "Source Image"] showImage: srcImg];
		dstWin = [[[Window alloc] initWithName: "Canny Edges Image"] showImage: dstImg];
		dstWin2 = [[[Window alloc] initWithName: "OpenCV Canny Edges Image"] showImage: dstImg2];
		cvWaitKey(0);
		[srcWin free];
		[dstWin free];
		[dstImg free];
		[srcImg free];
		[dstImg2 free];
		[dstWin2 free];
		goto quit;
	}

quit:
	return 0;
}

