/*
	This file is part of cv3.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	cv3 is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	cv3 is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with cv3.  If not, see <http://www.gnu.org/licenses/>.
*/

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

