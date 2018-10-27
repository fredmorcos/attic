/* uncomment to enable sobel edge detection instead of 
 * roberts edge detection.
 */
// #define SOBEL

/* uncomment to use linked list instead of dynamic array,
 * the dynamic array is two times faster than the linked 
 * list with average sized images.
 */
// #define LINKEDLIST

/* uncomment to enable debugging information.
 */
#define DEBUG

/* uncomment to enable recursive hysteresis thresholding
 * rather than simple hysteresis thresholding.
 */
#define RECURSIVE

/* set the intensifying/disintensifying factor of the 
 * gradient magnitude images, if SOBEL is enabled, 
 * disintensification will be used, if roberts then 
 * intensification will be used.
 */
#define FACTOR 2

