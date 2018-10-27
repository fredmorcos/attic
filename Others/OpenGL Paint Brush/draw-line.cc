/*	Salma Ismail -- 1-2225 -- E8
	Question 1 on Assignment 1
*/

/*	build: g++ -lglut q2.cc
	run: ./a.out
	works well with -O, -Os, -O2 & -O3 optimizations.
*/
#include "stdafx.h"
#include <GL/glut.h>
#include <stdio.h>
#include <math.h>

#define WINDOW_WIDTH	640
#define WINDOW_HEIGHT	480

class point {
	public: int x, y;
};

point corners[2];

void swap(int *n1, int *n2) {
	int tmp=*n1;
	*n1=*n2;
	*n2=tmp;
}

void drawLine(int x1, int y1, int x2, int y2) {
	bool steep;
	int delta_x=x2-x1;	
	int delta_y=y2-y1;
	int y_step;
	int tmp_y;
	int error;
	
	if (delta_x==0) {
		printf("Delta X = 0");
		return;
	}

	if (abs(delta_y)>abs(delta_x)) {
		steep=true;
		swap(&x1, &y1);
		swap(&x2, &y2);
	}
	else {
		steep=false;
	}
	
	delta_x=x2-x1;
	delta_y=abs(y2-y1);
	error=0;
	tmp_y=y1;
	
	if (x1>x2) {
		swap(&x1, &x2);
		swap(&y1, &y2);
	}

	if (y1<y2) {
		y_step=1;
	}
	else {
		y_step=-1;
	}
	
	glClear(GL_COLOR_BUFFER_BIT);
	for (int i=x1; i<=x2; i++){
		glBegin(GL_POINTS);
		if (steep==true) {
			glVertex2i(tmp_y,i);
		}
		else {
			glVertex2i(i,tmp_y);
		}
		glEnd();
		
		error+=delta_y;
		
		if ((2*error)>=delta_x) {
			tmp_y+=y_step;
			/* tmp_y=(((y2-y1)/(x2-x1))*(i-x1))+y1; */
			error-=delta_x;
		}
	}
}

void MyMouse (int button, int state, int click_x, int click_y){
	static int number_of_clicks=0;
	if (button==GLUT_LEFT_BUTTON && state==GLUT_DOWN) {
		corners[number_of_clicks].x=click_x;
		printf("X%d: %d\n", number_of_clicks, click_x);
		corners[number_of_clicks].y=WINDOW_HEIGHT-click_y;
		printf("Y%d: %d\n", number_of_clicks, WINDOW_HEIGHT-click_y);
		if (++number_of_clicks==2){
			drawLine(corners[0].x, corners[0].y,
						corners[1].x, corners[1].y);
			number_of_clicks=0;
		}
	}
	else if (button==GLUT_RIGHT_BUTTON && state==GLUT_DOWN) {
		exit(0);
	}
	glFlush();
}

void myInit(){
	glClearColor(1.0, 1.0, 1.0, 0.0);
	glColor3f(0.6f, 0.5f, 0.3f);
	glPointSize(2.0);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	/* gluOrtho2D(left, right, bottom, top); */
	gluOrtho2D(0.0, 640.0, 0.0, 480.0);
	glLineWidth(2.0);
}

void myDisplay() {
	glClear(GL_COLOR_BUFFER_BIT);
	/* to test where the origin (0,0) is */
	/* glBegin(GL_LINE_LOOP);
		glVertex2i(0, 0);
		glVertex2i(0, 10);
		glVertex2i(10, 10);
		glVertex2i(10, 0);
	glEnd(); */
	glFlush();
}

int main(int argc, char** argv) {
	printf("Welcome!\n");
	/* printf("a=5, b=7\n");
	int a=5, b=7;
	swap(&a,&b);
	printf("a=%d, b=%d\n", a, b); */
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
	glutInitWindowSize(WINDOW_WIDTH, WINDOW_HEIGHT);
	glutInitWindowPosition(100, 150);
	glutCreateWindow("Answer to Question I");
	glutDisplayFunc(myDisplay);
	glutMouseFunc(MyMouse);
	myInit();
	glutMainLoop();
	
	return 0;
}
