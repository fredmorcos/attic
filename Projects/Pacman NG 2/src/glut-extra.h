#ifndef __GLUT
#define __GLUT

// void initGlut ();
void initGlut (void (*cbDisplay) (void), void (*cbKeyboard) (unsigned char ucKey, int iX, int iY), void (*cbKeyboardSpecial) (int iKey, int iX, int iY));
void initGlutWindow (int iWidth, int iHeight);

#endif
