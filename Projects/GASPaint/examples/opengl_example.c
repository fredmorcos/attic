#include <GL/glut.h>
#include <stdio.h>
#include <gtk/gtk.h>
#include <stdlib.h>
void fill ( GdkColor initial, GdkColor final, int x, int y );
int gx0, gy0, gx1, gy1, first_click=-1;
#define displacement 50
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
	glClearColor(1.0,1.0,1.0,0.0);        // set white background color
	glColor3f(1.0, 1.0, 0.0);           // set the drawing color
	glPointSize(1.0);               // a ‘dot’ is 4 by 4 pixels
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluOrtho2D(0, 320.0*2, 0, 240.0*2);
}

void myDisplay(void)
{
       glClear(GL_COLOR_BUFFER_BIT);      // clear the screen
       
       glBegin ( GL_LINE_LOOP );
       glVertex2i ( 0 ,0 );
       glVertex2i ( 0 ,100 );
       glVertex2i ( 100 ,100 );
       glVertex2i ( 100 ,0 );
       glEnd ();
       
       glBegin ( GL_LINE_LOOP );
       glVertex2i ( 50 ,50 );
       glVertex2i ( 50 ,150 );
       glVertex2i ( 150 ,150 );
       glVertex2i ( 150 ,50 );
       glEnd ();
       
       glBegin ( GL_LINE );
       glVertex2i ( 0 ,0 );
       glVertex2i ( 150 ,150 );
       glEnd ();
       
       
       GdkColor c1;
       c1.red = 255;
       c1.green = 255;
       c1.blue = 255;
       
       GdkColor c2;
       c2.red = 255;
       c2.green = 0;
       c2.blue = 0;
       glColor3f ( 1.0, 0.0, 0.0 );
       fill ( c1, c2, 80, 90 );
       fill ( c1, c2, 20, 5 );
       fill ( c1, c2, 200, 5 );
       glColor3f ( 1.0, 1.0, 0.0 );
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




void fill ( int R0, int G0, int B0, int R1, int G1, int B1, int x, int y )
{
	GdkColor initial, GdkColor final;
	initial.red = R0;
	initial.green = G0;
	initial.blue = B0;
	final.red = R1;
	final.green = G1;
	final.blue = B1;
	if	( 
			( initial.red == final.red && initial.green == final.green && initial.blue == final.blue )
			||
			( x<0 || x>300 || y<0 || y>300 )
		)
		return;
	
	
	float *real_color = malloc ( 3 );
	
	glReadPixels( x, y, 1, 1, GL_RGB, GL_FLOAT, real_color );
	
	
	if 	(
			( real_color[0]*255.0f == initial.red &&real_color[1]*255.0f == initial.green && real_color[2]*255.0f == initial.blue )
			&&
			! ( real_color[0]*255.0f == final.red &&real_color[1]*255.0f == final.green && real_color[2]*255.0f == final.blue )
		)
	{
		
		glBegin ( GL_POINT );
		glVertex2i ( x, y );
		glEnd ();
		
		fill ( initial, final, x+1, y );
		fill ( initial, final, x, y+1 );
		fill ( initial, final, x, y-1 );
		fill ( initial, final, x-1,	y );
	}
	free ( real_color );
}
