//Bricks 

#include <GL/glut.h>
#include <stdio.h>
#include "stdafx.h"
#include <math.h>

/*function prototypes*/
void myinit(void);;
void myDisplay(void);
void myKeyboard(void);
void drawPaddle(int, int, int, int);
void myTimer(int);
void drawBall(int, int, int);

enum TYPES_OF_BRICKS		{WALL, BRICK, PADDLE};
enum COLOR 						{GREY, RED,YELLOW, BLUE, PURPLE, GREEN, BLACK};

/*class point {
	public:	int x, y;
};*/

class Brick{
	 public: 	int x,y,height,width;
	 				TYPES_OF_BRICKS type;
	 				COLOR color;
	  				bool visible;
};

//Brick leftWall;
//Brick rightWall;
Brick paddle;
int num_bricks= 6*15;
int num_walls=2;
Brick wallBricks[2];
Brick bricks[6*15] ;

void myinit(void){
	 glClearColor(1.0,1.0,1.0,0.0);
	 glColor3f(1.0f,1.0f,1.0f);
	 glMatrixMode(GL_PROJECTION);
	 glLoadIdentity();
	 gluOrtho2D(0.0, 640.0, 480.0, 0.0);
}

void myDisplay(void){
	 glClear(GL_COLOR_BUFFER_BIT);
	/*Draw the walls*/
	 for (int l=0; l<num_walls; l++){
	 	wallBricks[l].type=WALL;
	 	wallBricks[l].color=BLACK;
	 	wallBricks[l].visible= true;
	 	wallBricks[l].x= l*620;
	 	wallBricks[l].y=0;
	 	wallBricks[l].width= 20;
	 	wallBricks[l].height= 480;
	 	//glRecti(0,0,20,480);
	 	 glColor3f(0.0f, 0.0f, 0.0f);
	 	glRecti( wallBricks[l].x , wallBricks[l].y , wallBricks[l].x+wallBricks[l].width , wallBricks[l].height);
	}
	
	/*will check if a brick is visible give each row of bricks a color and draw only if visible*/
	 //glPushMatrix();
	 for (int i=0; i<num_bricks; i++){
	 	if(bricks[i].visible==true){
	 		if( i >=0 && i<15){
	 			bricks[i].color= GREY;
	 			glColor3f(0.8f, 0.8f, 0.8f);
	 			glRecti( (i*37)+20+(i*3) , 20 , (i*37)+57+(i*3) , 35);
	 		}
	 		else if (i>=15 && i<30){
	 			bricks[i].color=RED;
	 			glColor3f(1.0f, 0.0f, 0.0f);
	 			glRecti( ((i-15)*37)+20+((i-15)*3) , 38 , ((i-15)*37)+57+((i-15)*3) , 53);
	 		}
	 		else if (i>=30	&& i<45){
	 			bricks[i].color= YELLOW;
	 			glColor3f(1.0f, 1.0f, 0.0f);
	 			glRecti( ((i-30)*37)+20+((i-30)*3) , 56 , ((i-30)*37)+57+((i-30)*3) , 71);
	 		}
	 		else if (i>=45 && i<60){
	 			bricks[i].color= BLUE;
	 			glColor3f(0.0f, 0.0f, 1.0f);
	 			glRecti( ((i-45)*37)+20+((i-45)*3) , 74 , ((i-45)*37)+57+((i-45)*3) , 89);
	 		}
	 		else if(i>=60 &&i<75){
	 			bricks[i].color= PURPLE;
	 			glColor3f(0.6f,0.03f,1.0f);
	 			glRecti( ((i-60)*37)+20+((i-60)*3) , 92 , ((i-60)*37)+57+((i-60)*3) , 107);
	 		}
	 		else if(i>=75 &&i< 90){
	 			bricks[i].color= GREEN;
	 			glColor3f(0.0f, 1.0f, 0.0f);
	 			glRecti( ((i-75)*37)+20+((i-75)*3) , 110 , ((i-75)*37)+57+((i-75)*3) , 125);
	 		}
	 	}
	}
	/*Draw the Paddle*/
	glColor3f(0.1f, 0.1f, 1.0f);
	glRecti(paddle.x , paddle.y , paddle.x+paddle.width , paddle.y+paddle.height);
	
	glutTimerFunc(50,myTimer, 1);
	glFlush();
	glutSwapBuffers();
}

void drawPaddle(int xpos, int ypos, int width, int height){
	paddle.x= xpos;
	paddle.y= ypos;
	paddle.width= width;
	paddle.height= height;
	paddle.type= PADDLE;
	paddle.color= BLUE;
	paddle.visible= true;
	myDisplay();
}

/*void set_circle(point center, int radius) {
	for (int i=0; i<360; i++) {
		double i_radian=i*degree_to_radian;
		free_points[i].x=center.x+(int)(radius*(cos(i_radian)));
		free_points[i].y=center.y+(int)(radius*(sin(i_radian)));
		printf("step:%d, radius:%d, cos(i):%f, sin(i):%f\n", i, radius, (cos(i)), (sin(i)));
	}
}*/

void reset_bricks(void){
	for (int i=0; i<num_bricks; i++){
		bricks[i].visible=true;
		bricks[i].type=BRICK;
		bricks[i].width=37;
		bricks[i].height=15;
	}
}

void myTimer(int value){
	 glutPostRedisplay();
}

void myKeyboard(unsigned char key, int x, int y){
	 if(key == 'q' || key == 'Q')
	 		exit(0);
	 else if(key=='4'){
	 	//move the paddle to the left...redraw
	 	if(paddle.x > 20){
	 		paddle.x=paddle.x-20;
	 		drawPaddle(paddle.x , 480-20-10 , 40 , 10);
	 	}
	 	else drawPaddle(paddle.x , 480-20-10 , 40 , 10);
	 }
	 else if(key=='6'){
	 	//move the paddle to the right...redraw
	 	if (paddle.x < 580){
	 		paddle.x= paddle.x+20;
	 		drawPaddle(paddle.x , 480-20-10 , 40 , 10);
	 	}
	 	else drawPaddle(paddle.x , 480-20-10 , 40 , 10);
	 }
}
void drawBall(int x,int y, int r){
	int z;
	int X,Y;
	glColor3f(1,1,1);
	glBegin(GL_POLYGON);
	for(int i=0;i<=360;i++){
		X=(int)(cos((i*3.14159265358979323846) /180) * r)+x;
		Y=(int)(sin((i*3.14159265358979323846) /180)* r)+y;
		glVertex2f(X, Y);
	}
	glEnd();
	glFlush();
}

int main(int argc,char** argv){
	reset_bricks();
	glutInit(&argc,argv);
	glutInitDisplayMode(GLUT_DOUBLE| GLUT_RGB);
	glutInitWindowSize(640,480);
	glutInitWindowPosition(100,100);
	glutCreateWindow("Bricks");
	glutDisplayFunc(myDisplay);
	drawPaddle(320, 480-30, 40, 10);
	//glutMouseFunc(myMouse);
	glutKeyboardFunc(myKeyboard);
	myinit();
	glutMainLoop();
}

