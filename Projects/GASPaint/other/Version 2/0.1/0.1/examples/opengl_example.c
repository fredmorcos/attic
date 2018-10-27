#include <GL/glut.h>
#include <stdio.h>
#include <gtk/gtk.h>
#include <stdlib.h>
void fill ( GdkColor initial, GdkColor final, int x, int y );
int gx0, gy0, gx1, gy1, first_click=-1;
#define displacement 50


short writing [480][640][3];
//================================ drawRECT ====================================
void drawRect(int x0, int y0, int x1, int y1)
{
	glClear(GL_COLOR_BUFFER_BIT);
	glRecti(x0,y0,x1,y1);
}

void myMouse(int button, int state, int x, int y)
{
	if(button==GLUT_RIGHT_BUTTON && state==GLUT_DOWN)
	{
		glClear(GL_COLOR_BUFFER_BIT);
	}
	else
	{
		
		if(button==GLUT_LEFT_BUTTON && state==GLUT_DOWN)
		{
			printf("%d %d\n",x ,y);
			if(first_click==-1)
			{
				first_click=0;
				gx0=x;
				gy0=y;
			}
			else
			{
				gx1=x;
				gy1=y;
				drawRect(gx0, 480-gy0, gx1, 480-gy1);
				first_click=-1;
			}
		}
		glFlush();
	}
}


//================================== myInit ====================================
void myInit(void)
{
	glClearColor(0.0,1.0,1.0,0.0);        // set white background color
	glColor3f(1.0, 1.0, 0.0);           // set the drawing color
	glPointSize(1.0);               // a ‘dot’ is 4 by 4 pixels
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluOrtho2D( 0, 320.0*2, 240.0*2, 0 );
}

void myDisplay(void)
{
       glClear(GL_COLOR_BUFFER_BIT);      // clear the screen
       
       glBegin ( GL_LINE_LOOP );
       glVertex2i ( 0 ,0 );
       glVertex2i ( 0 ,300 );
       glVertex2i ( 300 ,300 );
       glVertex2i ( 300 ,0 );
       glEnd ();
       
       
       /*
       glBegin ( GL_LINE_LOOP );
       glVertex2i ( 50 ,50 );
       glVertex2i ( 50 ,150 );
       glVertex2i ( 150 ,150 );
       glVertex2i ( 150 ,50 );
       glEnd ();
       */
       /*
       glBegin ( GL_LINE );
       glVertex2i ( 0 ,0 );
       glVertex2i ( 150 ,150 );
       glEnd ();
       */
       
       
       //glColor3f ( 1.0, 0.0, 0.0 );
       
       filltest (  80, 90 );
       //filltest (  20, 5 );
       //fill ( c1, c2, 200, 5 );
       
       //glColor3f ( 1.0, 1.0, 0.0 );
       
       
       //drawrect ( 100, 50, 50, 100 );
       
       glFlush();                          // send all output to display
}


void myKeyboard(unsigned char key, int x, int y)
{
	if(key=='a')
	{
		gx0-=displacement;
		gx1-=displacement;
	}
	
	if(key=='d')
	{
		gx0+=displacement;
		gx1+=displacement;
	}
	
	if(key=='s')
	{
		gy0+=displacement;
		gy1+=displacement;
	}
	
	if(key=='w')
	{
		gy0-=displacement;
		gy1-=displacement;
	}
	
	drawRect(gx0,480-gy0,gx1,480-gy1);
}

//================================== MAIN ======================================
int main(int argc, char** argv)
{
	glutInit(&argc, argv);           // initialize the toolkit
	glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB); // set display mode
	glutInitWindowSize(640,480);      // set window size
	glutInitWindowPosition(100, 150); // set window position on screen
	glutCreateWindow("ANDREW"); // open the screen window
	glutDisplayFunc(myDisplay);      // register redraw function
	//glutMouseFunc(myMouse);
	//glutKeyboardFunc(myKeyboard);
	myInit();
	glutMainLoop();               // go into a perpetual loop
	return 0;
}



void filltest ( int x, int y)
{
	glReadPixels ( 0, 0, 640, 480, GL_RGB, GL_SHORT, writing );
	GdkColor c1;
       c1.red = 0;
       c1.green = 255;
       c1.blue = 255;
       
       GdkColor c2;
       c2.red = 255;
       c2.green = 0;
       c2.blue = 0;
       y=480-y;
       fill (c1, c2, x,y);
	glRasterPos2i ( 0, 480-1 );
	glDrawPixels ( 640, 480, GL_RGB, GL_SHORT, writing );
//	glFlush ();
}

void drawrect ( int x, int y, int w, int h )
{
	glReadPixels ( 0, 0, 640, 480, GL_RGB, GL_SHORT, writing );
	int i, j;
	i=x;
	while ( i<x+w )
	{
		j=y;
		while ( j<y+h )
		{
			printf ( "BEFORE ==>   X: %d, Y: %d, Color: %d %d %d\n", i, j, writing [i][j][0], writing [i][j][1], writing [i][j][2] );

			writing [j][i][0] = 255*128;
			writing [j][i][1] = 0;
			writing [j][i][2] = 0;
			
			printf ( "AFTER ==>   X: %d, Y: %d, Color: %d %d %d\n", i, j, writing [i][j][0], writing [i][j][1], writing [i][j][2] );
			j++;
		}
		i++;
	}
	glRasterPos2i ( 0, 470-1 );
	glDrawPixels ( 640, 480, GL_RGB, GL_SHORT, writing );
//	glFlush ();
}

void fill ( GdkColor initial, GdkColor final , int x, int y )
{
	/*
	GdkColor initial, GdkColor final;
	
	initial.red = R0;
	initial.green = G0;
	initial.blue = B0;
	final.red = R1;
	final.green = G1;
	final.blue = B1;
	* */
	//printf ( "x: %d, y: %d, Color %d %d %d\n", x, y, writing[x][y][0]/128, writing[x][y][1]/128, writing[x][y][2]/128 );
	if	( 
			( initial.red == final.red && initial.green == final.green && initial.blue == final.blue )
			||
			( x<0 || x>640 || y<0 || y>480 )
		)
		return;
	
	
	//printf ( "First Test Passed\n" );	
	
	
	
	
	if 	(
			( writing[y][x][0]/128 == initial.red &&writing[y][x][1]/128 == initial.green && writing[y][x][2]/128 == initial.blue )
			&&
			! ( writing[y][x][0]/128 == final.red &&writing[y][x][1]/128 == final.green && writing[y][x][2]/128 == final.blue )
		)
	{
		
		/*	
		glBegin ( GL_POINT );
		glVertex2i ( x, y );
		glEnd ();
		*/
		
		//printf ( "Second Test Passed\n" );
		
		
		
		
		writing [y][x][0] = (short) final.red*128;
		writing [y][x][1] = (short) final.green*128;
		writing [y][x][2] = (short) final.blue*128;
		
		
		
		
		
		fill ( initial, final, x+1, y );
		fill ( initial, final, x, y+1 );
		fill ( initial, final, x, y-1 );
		fill ( initial, final, x-1,	y );
		
	}
}
