#include <GL/glut.h>
#include <stdio.h>
#include <stdlib.h>

#include "global.h"
#include "wall.h"

wall *wall_new (float fposx, float fposy, float fposz, orientation orient, float flen)
{
	wall *new_wall = calloc (1, sizeof (wall));
	new_wall->fPosX = fposx;
	new_wall->fPosY = fposy;
	new_wall->fPosZ = fposz;
	new_wall->orient = orient;
	new_wall->fLength = flen;
	return new_wall;
}

void wall_draw (wall *w)
{
	glColor3f (WALL_COLOR_R, WALL_COLOR_G, WALL_COLOR_B);
	float scaling_x, scaling_y, scaling_z;
	float trans_x, trans_y, trans_z;
	if ( w->orient )
	{
		trans_x = w->fPosX*CELL_SIZE;
		trans_z = w->fPosZ*CELL_SIZE;
		scaling_x = w->fLength*CELL_SIZE;
		scaling_z = WALL_THICKNESS;
	}
	else
	{
		trans_z = w->fPosZ*CELL_SIZE;
		trans_x = w->fPosX*CELL_SIZE;
		scaling_z = w->fLength*CELL_SIZE;
		scaling_x = WALL_THICKNESS;
	}
	trans_y = w->fPosY + WALL_HEIGHT/2;
	scaling_y = WALL_HEIGHT;
	
	
	glPushMatrix ();
	glTranslated ( trans_x, trans_y, trans_z );
	glScaled ( scaling_x, scaling_y, scaling_z );
	glutSolidCube ( 1 );
	glPopMatrix ();
}

int wall_can_be_here (wall *w, int x, int z)
{
	if (w->orient == HORIZONTAL)
	{
		if (z==w->fPosZ)
		{
			//if (x<= (int) (w->fPosX + w->fLength/2) && x>= (int) (w->fPosX - w->fLength/2))
			if (x<= (w->fPosX + w->fLength/2) && x>= (w->fPosX - w->fLength/2))
			return 0;
		}
	}
	else
	{
		if (x==w->fPosX)
		{
			//if (z<= (int) (w->fPosZ + w->fLength/2) && z >= (int) (w->fPosZ - w->fLength/2))
			if (z<= (w->fPosZ + w->fLength/2) && z >= (w->fPosZ - w->fLength/2))
			return 0;
		}
	}
	return 1;
}
