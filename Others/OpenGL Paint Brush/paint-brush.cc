/*	Salma Ismail -- 1-2225 -- E8
	Question 2 on Assignment 1
*/

/*	build: g++ -lglut q2.cc
	run: ./a.out
	works well with -O, -Os, -O2 & -O3 optimizations.
*/
#include "stdafx.h"
#include <GL/glut.h>
#include <stdio.h>
#include <math.h>

#define WINDOW_WIDTH		640
#define WINDOW_HEIGHT		480
/* #define NUM_STRIP_POINTS	100 */
#define NUM_FREE_POINTS		WINDOW_HEIGHT*WINDOW_WIDTH

class point {
	public: int x, y;
};

/* function prototypes */
void myInit(void);
void myDisplay(void);
void myMouse(int, int, int, int);
/* void reset_strip_points(void); */
void myMotion(int, int);
void reset_free_points(void);
void set_circle(point, int);

enum CHOSEN_COLOR	{RED, GREEN, BLUE,
					YELLOW, ORANGE, VIOLET};
enum CHOSEN_TOOL	{FREE, LINE, LINE_STRIP,
					TRIANGLE, QUAD, CIRCLE};

CHOSEN_COLOR	chosen_color;
CHOSEN_TOOL		chosen_tool;

point line_points[2];
point triangle_points[3];
point quad_points[4];
/* going to use the free_points instead of the strip_points
	because the space in memory is reserved anyways (free_points
	is larger than strip_points), they aren't used at the
	same time and are reset at each tool change.
/* point strip_points[NUM_STRIP_POINTS]; */
point free_points[NUM_FREE_POINTS];
const double degree_to_radian=(3.14159/180);
bool draw=false;
static int num_clicks;
static int free_moves;

void myInit() {
	glClearColor(1.0, 1.0, 1.0, 0.0);
	glColor3f(0.6f, 0.5f, 0.3f);
	glPointSize(8.0);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	/* gluOrtho2D(left, right, bottom, top); */
	gluOrtho2D(0.0, 640.0, 0.0, 480.0);
	glLineWidth(2.0);
}

void myDisplay() {
	glClear(GL_COLOR_BUFFER_BIT);
	/* Draw the color picker holder */
	glColor3f(0.8f, 0.8f, 0.8f);
	glRecti(0,0,310,60);
	
	/* Draw the tool picker holder */
	glColor3f(0.8f, 0.8f, 0.8f);
	glRecti(0,480,60,480-310);
		
	/* Draw the color boxes */
	glColor3f(1.0f, 0.0f, 0.0f);	/* red */
	glRecti(10,10,50,50);
	
	glColor3f(0.0f, 1.0f, 0.0f);	/* green */
	glRecti(60,10,100,50);
	
	glColor3f(0.0f, 0.0f, 1.0f);	/* blue */
	glRecti(110,10,150,50);

	glColor3f(1.0f, 1.0f, 0.0f);	/* yellow */
	glRecti(160,10,200,50);
	
	glColor3f(1.0f, 0.6f, 0.0f);	/* orange */
	glRecti(210,10,250,50);
	
	glColor3f(0.6f, 0.03f, 1.0f);	/* violet */
	glRecti(260,10,300,50);
	
	/* draw the toolbox with the selected tool */
	glColor3f(1.0f, 1.0f, 1.0f);	/* white */
	glRecti(10,470,50,430);
	glRecti(10,420,50,380);
	glRecti(10,370,50,330);
	glRecti(10,320,50,280);
	glRecti(10,270,50,230);
	glRecti(10,220,50,180);
	glColor3f(0.9f, 0.9f, 0.9f);	/* dark_grey */
	if (chosen_tool == FREE)		{	glRecti(10,470,50,430);		}
	if (chosen_tool == LINE)		{	glRecti(10,420,50,380);		}
	if (chosen_tool == LINE_STRIP)	{	glRecti(10,370,50,330);		}
	if (chosen_tool == TRIANGLE) 	{	glRecti(10,320,50,280);		}
	if (chosen_tool == QUAD) 		{	glRecti(10,270,50,230);		}
	if (chosen_tool == CIRCLE) 		{	glRecti(10,220,50,180);		}
	
	glColor3f(0.0f, 0.0f, 0.0f);	/* black */
	glBegin(GL_LINE_STRIP);			/* free */
		glVertex2i(25,455); glVertex2i(20,440);
		glVertex2i(40,460); glVertex2i(35,440);
	glEnd();

	glBegin(GL_LINES);				/* line */
		glVertex2i(20,390); glVertex2i(40,410);
	glEnd();
	
	glBegin(GL_LINE_STRIP);			/* line strip */
		glVertex2i(15,340); glVertex2i(25,360);
		glVertex2i(35,340); glVertex2i(45,360);
	glEnd();
	
	glBegin(GL_LINE_LOOP);			/* triangle */
		glVertex2i(20,290); glVertex2i(30,310);
		glVertex2i(40,290);
	glEnd();
	
	glBegin(GL_LINE_LOOP);			/* quad */
		glVertex2i(20,240); glVertex2i(30,260);
		glVertex2i(40,250); glVertex2i(35,240);
	glEnd();
	
	glBegin(GL_LINE_LOOP);			/* circle */
		point button_points[360];
		for (int i=0; i<360; i++) {
			double i_radian=i*degree_to_radian;
			button_points[i].x=30+(int)(10*(cos(i_radian)));
			button_points[i].y=200+(int)(10*(sin(i_radian)));
		}
		for (int i=0; i<360; i++) {
			glVertex2i(button_points[i].x, button_points[i].y);
		}
	glEnd();
	
	/* apply the chosen color to the box */
	glColor3f(0.8f,0.8f,0.8f);
	glRecti(580,0,640,60);
	if (chosen_color == RED)			{ glColor3f(1.00f, 0.00f, 0.00f); }
	else if (chosen_color == GREEN)		{ glColor3f(0.00f, 1.00f, 0.00f); }
	else if (chosen_color == BLUE)		{ glColor3f(0.00f, 0.00f, 1.00f); }
	else if (chosen_color == YELLOW)	{ glColor3f(1.00f, 1.00f, 0.00f); }
	else if (chosen_color == ORANGE)	{ glColor3f(1.00f, 0.60f, 0.00f); }
	else if (chosen_color == VIOLET)	{ glColor3f(0.60f, 0.03f, 1.00f); }
	glRecti(590,10,630,50);
	
	if (draw==true) {
		if (chosen_tool == FREE) {
			printf("free drawing\n");
			glBegin(GL_LINE_STRIP);
				for (int i=0; i<NUM_FREE_POINTS; i++) {
					if (free_points[i].x!=-1) {
						glVertex2i(free_points[i].x, free_points[i].y);
					}
					else {
						break;
					}
				}
			glEnd();
		}
		else if (chosen_tool == LINE) {
			printf("drawing line\n");
			/* printf("x1:%d, y1:%d\nx2:%d, y2:%d\n",line_points[0].x,line_points[0].y,
														line_points[1].x,line_points[1].y); */
			glBegin(GL_LINES);
				for (int i=0; i<2; i++) {
					glVertex2i(line_points[i].x, line_points[i].y);
				}
			glEnd();
		}
		else if (chosen_tool == LINE_STRIP) {
			printf("drawing strip\n");
			glBegin(GL_LINE_STRIP);
				for (int i=0; i<NUM_FREE_POINTS; i++) {
					if (free_points[i].x!=-1) {
						glVertex2i(free_points[i].x, free_points[i].y);
					}
					else {
						break;
					}
				}
			glEnd();
		}
		else if (chosen_tool == TRIANGLE) {
			printf("drawing triangle\n");
			glBegin(GL_LINE_LOOP);
				for (int i=0; i<3; i++) {
					glVertex2i(triangle_points[i].x, triangle_points[i].y);
				}				
			glEnd();
		}
		else if (chosen_tool == QUAD) {
			printf("drawing quad\n");
			glBegin(GL_LINE_LOOP);
				for (int i=0; i<4; i++) {
					glVertex2i(quad_points[i].x, quad_points[i].y);
				}				
			glEnd();
		}
		else if (chosen_tool == CIRCLE) {
			printf("drawing circle\n");
			glBegin(GL_LINE_LOOP);
				for (int i=0; i<NUM_FREE_POINTS; i++) {
					if (free_points[i].x!=-1) {
						glVertex2i(free_points[i].x, free_points[i].y);
					}
					else {
						break;
					}
				}
			glEnd();
		}
	}
	glFlush();
	glutSwapBuffers();
}

/* using reset_free_points() instead */
/* void reset_strip_points() {
	draw=false;
	for(int i=0; i<NUM_STRIP_POINTS; i++) {
		if (strip_points[i].x!=-1) {
			strip_points[i].x=-1;
		}
		else {
			break;
		}
	}
} */

void reset_free_points() {
	free_moves=0;
	draw=false;
	for(int i=0; i<NUM_FREE_POINTS; i++) {
		if (free_points[i].x!=-1) {
			free_points[i].x=-1;
		}
		else {
			break;
		}
	}
}

void set_circle(point center, int radius) {
	for (int i=0; i<360; i++) {
		double i_radian=i*degree_to_radian;
		free_points[i].x=center.x+(int)(radius*(cos(i_radian)));
		free_points[i].y=center.y+(int)(radius*(sin(i_radian)));
		printf("step:%d, radius:%d, cos(i):%f, sin(i):%f\n", i, radius, (cos(i)), (sin(i)));
	}
}

void myMouse (int button, int state, int click_x, int click_y) {
	if (button==GLUT_LEFT_BUTTON && state==GLUT_DOWN) {
		/* printf("MOUSE X:%d, Y:%d\n", click_x, click_y); */
		/* select the tool */
		if (click_x>10 && click_x<50 && click_y>480-470 && click_y<480-430) {
			chosen_tool=FREE;
			printf("FREE\n");
			num_clicks=0;
			reset_free_points();
		}
		else if (click_x>10 && click_x<50 && click_y>480-420 && click_y<480-380) {
			chosen_tool=LINE;
			printf("LINE\n");
			num_clicks=0;
			reset_free_points();
		}
		else if (click_x>10 && click_x<50 && click_y>480-370 && click_y<480-330) {
			chosen_tool=LINE_STRIP;
			printf("LINE_STRIP\n");
			num_clicks=0;
			reset_free_points();
		}
		else if (click_x>10 && click_x<50 && click_y>480-320 && click_y<480-280) {
			chosen_tool=TRIANGLE;
			printf("TRIANGLE\n");
			num_clicks=0;
			reset_free_points();
		}
		else if (click_x>10 && click_x<50 && click_y>480-270 && click_y<480-230) {
			chosen_tool=QUAD;
			printf("QUAD\n");
			num_clicks=0;
			reset_free_points();
		}
		else if (click_x>10 && click_x<50 && click_y>480-220 && click_y<480-180) {
			chosen_tool=CIRCLE;
			printf("CIRCLE\n");
			num_clicks=0;
			reset_free_points();
		}
		/* select the color */
		else if (click_x>10 && click_x<50 && click_y>480-50 && click_y<480-10) {
			printf("RED\n");
			chosen_color=RED;
		}
		else if (click_x>60 && click_x<100 && click_y>480-50 && click_y<480-10) {
			printf("GREEN\n");
			chosen_color=GREEN;
		}
		else if (click_x>110 && click_x<150 && click_y>480-50 && click_y<480-10) {
			printf("BLUE\n");
			chosen_color=BLUE;
		}
		else if (click_x>160 && click_x<200 && click_y>480-50 && click_y<480-10) {
			printf("YELLOW\n");
			chosen_color=YELLOW;
		}
		else if (click_x>210 && click_x<250 && click_y>480-50 && click_y<480-10) {
			printf("ORANGE\n");
			chosen_color=ORANGE;
		}
		else if (click_x>260 && click_x<300 && click_y>480-50 && click_y<480-10) {
			printf("VIOLET\n");
			chosen_color=VIOLET;
		}
		else if (click_x>60 && click_y<480-60) {
			draw=false;
			if (chosen_tool==LINE) {
				if (num_clicks<2) {
					/* printf("num_clicks: %d\n", num_clicks); */
					line_points[num_clicks].x=click_x;
					line_points[num_clicks].y=480-click_y;
					if (++num_clicks==2) {
						num_clicks=0;
						draw=true;
					}
				}
			}
			else if (chosen_tool==LINE_STRIP) {
				if (num_clicks<NUM_FREE_POINTS) {
					free_points[num_clicks].x=click_x;
					free_points[num_clicks++].y=480-click_y;
					draw=true;
				}
				else {
					printf("no more space for strip points :(");
				}
			}
			else if (chosen_tool==TRIANGLE) {
				if (num_clicks<3) {
					triangle_points[num_clicks].x=click_x;
					triangle_points[num_clicks].y=480-click_y;
					if (++num_clicks==3) {
						num_clicks=0;
						draw=true;
					}
				}
			}
			else if (chosen_tool==QUAD) {
				if (num_clicks<4) {
					quad_points[num_clicks].x=click_x;
					quad_points[num_clicks].y=480-click_y;
					if (++num_clicks==4) {
						num_clicks=0;
						draw=true;
					}
				}
			}
			else if (chosen_tool==CIRCLE) {
				if (num_clicks<2) {
					line_points[num_clicks].x=click_x;
					line_points[num_clicks].y=480-click_y;
					if (++num_clicks==2) {
					/* printf("entered circle!!!\n"); */
						num_clicks=0;
						/* not a very accurate way of getting the radius of a circle,
							but will do the job after 10 hours of coding and 2 liters
							of caffeine */
						if ((abs(line_points[1].x-line_points[0].x))
								>(abs(line_points[1].y-line_points[0].y))) {
							set_circle(line_points[0], abs(line_points[1].x-line_points[0].x));
							/* printf("delta X is larger\n"); */
						}
						else {
							set_circle(line_points[0], abs(line_points[1].y-line_points[0].y));
							/* printf("delta Y is larger\n"); */
						}
						draw=true;
					}
				}
			}
		}
		myDisplay();
		glFlush();
	}
	else if (button==GLUT_RIGHT_BUTTON && state==GLUT_DOWN) {
		exit(0);
	}
}

void myMotion(int move_x, int move_y) {
	if (chosen_tool==FREE /* && move_x%2!=0 */ && move_x>60 && move_x<640 &&
											move_y<480-60) {
		/* printf("move x:%d, y:%d\n", move_x, move_y); */
		/* printf("free points item %d\n", free_moves); */
		free_points[free_moves].x=move_x;
		free_points[free_moves].y=480-move_y;
		free_moves++;
		draw=true;
		myDisplay();
	}
}

int main(int argc, char** argv) {
	reset_free_points();
	printf("Welcome!\n");
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
	glutInitWindowSize(WINDOW_WIDTH, WINDOW_HEIGHT);
	glutInitWindowPosition(100, 150);
	glutCreateWindow("Answer to Question II - Paint Brush");
	glutDisplayFunc(myDisplay);
	glutMouseFunc(myMouse);
	glutMotionFunc(myMotion);
	myInit();
	glutMainLoop();
	
	return 0;
}
