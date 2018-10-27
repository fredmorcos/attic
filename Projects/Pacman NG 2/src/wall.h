#ifndef __WALL
#define __WALL

#define WALL_THICKNESS 10
#define WALL_HEIGHT 5
#define WALL_COLOR_R 0.5
#define WALL_COLOR_G 0.5
#define WALL_COLOR_B 0.0

enum _orientation
{
	VERTICAL = 0,
	HORIZONTAL = 1
};
typedef enum _orientation orientation;

struct _wall
{
	float fPosX, fPosY, fPosZ;
	orientation orient;
	float fLength;
};
typedef struct _wall wall;

wall *wall_new (float posx, float posy, float posz, orientation or, float len);
void wall_draw ( wall *w );
int wall_can_be_here (wall *w, int x, int z);

#endif
