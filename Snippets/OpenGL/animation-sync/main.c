#define GL_GLEXT_PROTOTYPES
#include <GL/gl.h>
#include <GL/glx.h>

#include <X11/Xlib.h>
#include <X11/keysym.h>

#include <sys/time.h>
#include <unistd.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>


#define OBJ_WIDTH 100
#define OBJ_HEIGHT 100

struct window {
    int w, h;
};
static struct window window;
static GLuint vbos[2];

static GLfloat vertices[] = {
     0,  0, 0,
     0, OBJ_HEIGHT, 0,
    OBJ_WIDTH,  0, 0,
    OBJ_WIDTH, OBJ_HEIGHT, 0
};
static int     verticesCount  = 4;
static GLubyte colors[] = {
    255,   0,   0,
      0, 255,   0,
    255,   0,   0,
      0, 255,   0
};
static GLboolean fullscreen = GL_FALSE;
static int     colorsCount  = 4;
static GLint frames = 0;

static GLint desiredFps = 30;
static GLint animDuration = 5000;
static int goingBack = 1;


static int syncToVblank = 0;
static int syncCount;
void (*video_sync_get)();
void (*video_sync)();

/* return current time (in seconds) */
static double current_time(void)
{
    struct timeval tv;
#ifdef __VMS
    (void) gettimeofday(&tv, NULL );
#else
    struct timezone tz;
    (void) gettimeofday(&tv, &tz);
#endif
    return (double) tv.tv_sec + tv.tv_usec / 1000000.0;
}


static void createVbos()
{
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glGenBuffersARB(2, vbos);

    glBindBufferARB(GL_ARRAY_BUFFER_ARB, vbos[0]);
    glBufferDataARB(GL_ARRAY_BUFFER_ARB,
                    sizeof(GLfloat) * 3 * verticesCount,
                    vertices, GL_STATIC_DRAW_ARB);
    glVertexPointer(3, GL_FLOAT, 0, 0);

    glBindBufferARB(GL_ARRAY_BUFFER_ARB, vbos[1]);
    glBufferDataARB(GL_ARRAY_BUFFER_ARB,
                    sizeof(3) * sizeof(GLubyte) * colorsCount,
                    colors, GL_STATIC_DRAW_ARB);
    glColorPointer(3, GL_UNSIGNED_BYTE, 0, 0);
}

static void init()
{
    fprintf(stderr, "GL_RENDERER   = %s\n", (char *) glGetString(GL_RENDERER));
    fprintf(stderr, "GL_VERSION    = %s\n", (char *) glGetString(GL_VERSION));
    fprintf(stderr, "GL_VENDOR     = %s\n", (char *) glGetString(GL_VENDOR));

    srand(time(0));
    createVbos();

    glClearColor(1.0, 1.0, 1.0, 1.0);
    glShadeModel(GL_SMOOTH);


    video_sync_get = glXGetProcAddress((unsigned char *)"glXGetVideoSyncSGI");
    video_sync = glXGetProcAddress((unsigned char *)"glXWaitVideoSyncSGI");
    if (!video_sync_get || !video_sync) {
        fprintf(stderr, "failed to get sync functions\n");
        exit(1);
    }

    video_sync_get(&syncCount);
    ++syncCount;
}

static int durationInFrames = 0;

void computePos(int *x, int *y)
{
    float percent = frames/(desiredFps * animDuration/1000.);
    int fullw = window.w - OBJ_WIDTH;

    if (!percent)
        goingBack = !goingBack;

    if (goingBack)
        *x = fullw - fullw * percent;
    else
        *x = fullw * percent;

    if (durationInFrames) {
        fullw = desiredFps * animDuration/1000.;
        if (goingBack)
            *x = fullw - fullw * percent;
        else
            *x = fullw * percent;
    }

    *y = window.h/2 - OBJ_HEIGHT/2;
}

void draw()
{
    int x, y;
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    computePos(&x, &y);

    glLoadIdentity();
    glPushMatrix();
    glTranslatef(x, y, 0);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, verticesCount);
    glPopMatrix();

    glFlush();

    ++frames;
}

void reshape(int w, int h)
{
    window.w = w;
    window.h = h;
    /*animDuration = window.w;*/
    glViewport(0, 0, window.w, window.h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0, window.w, 0, window.h, -999999, 999999);
    glMatrixMode(GL_MODELVIEW);
}


/*
 * Create an RGB, double-buffered window.
 * Return the window and context handles.
 */
static void
make_window(Display *dpy, const char *name,
            int x, int y, int width, int height,
            Window *winRet, GLXContext *ctxRet)
{
    int attribs[] = { GLX_RGBA,
                      GLX_RED_SIZE, 1,
                      GLX_GREEN_SIZE, 1,
                      GLX_BLUE_SIZE, 1,
                      GLX_DOUBLEBUFFER,
                      GLX_DEPTH_SIZE, 1,
                      None };
    int scrnum;
    XSetWindowAttributes attr;
    unsigned long mask;
    Window root;
    Window win;
    GLXContext ctx;
    XVisualInfo *visinfo;

    scrnum = DefaultScreen( dpy );
    root = RootWindow( dpy, scrnum );
    width = DisplayWidth(dpy, scrnum);

    if (fullscreen) {
        x = 0; y = 0;
        width = DisplayWidth( dpy, scrnum );
        height = DisplayHeight( dpy, scrnum );
    }

    visinfo = glXChooseVisual( dpy, scrnum, attribs );
    if (!visinfo) {
        printf("Error: couldn't get an RGB, Double-buffered visual\n");
        exit(1);
    }

    /* window attributes */
    attr.background_pixel = 0;
    attr.border_pixel = 0;
    attr.colormap = XCreateColormap( dpy, root, visinfo->visual, AllocNone);
    attr.event_mask = StructureNotifyMask | ExposureMask | KeyPressMask;
    attr.override_redirect = fullscreen;
    mask = CWBackPixel | CWBorderPixel | CWColormap | CWEventMask | CWOverrideRedirect;

    win = XCreateWindow( dpy, root, 0, 0, width, height,
                         0, visinfo->depth, InputOutput,
                         visinfo->visual, mask, &attr );

    /* set hints and properties */
    {
        XSizeHints sizehints;
        sizehints.x = x;
        sizehints.y = y;
        sizehints.width  = width;
        sizehints.height = height;
        sizehints.flags = USSize | USPosition;
        XSetNormalHints(dpy, win, &sizehints);
        XSetStandardProperties(dpy, win, name, name,
                               None, (char **)NULL, 0, &sizehints);
    }

    ctx = glXCreateContext( dpy, visinfo, NULL, True );
    if (!ctx) {
        printf("Error: glXCreateContext failed\n");
        exit(1);
    }

    XFree(visinfo);

    *winRet = win;
    *ctxRet = ctx;
}


static void
event_loop(Display *dpy, Window win)
{
    while (1) {
        while (XPending(dpy) > 0) {
            XEvent event;
            XNextEvent(dpy, &event);
            switch (event.type) {
            case Expose:
                /* we'll redraw below */
                break;
            case ConfigureNotify:
                reshape(event.xconfigure.width, event.xconfigure.height);
                break;
            case KeyPress:
            {
                char buffer[10];
                XLookupString(&event.xkey, buffer, sizeof(buffer),
                              NULL, NULL);
                if (buffer[0] == 27) {
                    /* escape */
                    return;
                }
            }
            }
        }

        {
            static double tRate0 = -1.0;
            static double tPost0 = -1.0;
            double t = current_time();

            if (tRate0 < 0.0)
                tRate0 = t;
            if (tPost0 < 0.0)
                tPost0 = t;

            if (t - tRate0 >= animDuration/1000.) {
                GLfloat seconds = t - tRate0;
                GLfloat fps = frames / seconds;
                printf("%d frames in %3.1f seconds = %6.3f FPS\n", frames, seconds,
                       fps);
                tRate0 = t;
                frames = 0;
            }

            if (t - tPost0 >= (1000./desiredFps)/1000.) {
                draw();
                if (syncToVblank) {
                    video_sync(2, (syncCount + 1) % 2, &syncCount);
                }
                glXSwapBuffers(dpy, win);
                tPost0 = t;
            }
        }
    }
}

int
main(int argc, char *argv[])
{
   const int winWidth = 1000, winHeight = 500;
   Display *dpy;
   Window win;
   GLXContext ctx;
   char *dpyName = NULL;
   int i;

   dpy = XOpenDisplay(dpyName);
   if (!dpy) {
      printf("Error: couldn't open display %s\n",
	     dpyName ? dpyName : getenv("DISPLAY"));
      return -1;
   }

   make_window(dpy, "Animation synced", 0, 0, winWidth, winHeight, &win, &ctx);
   XMapWindow(dpy, win);
   glXMakeCurrent(dpy, win, ctx);

   init();

   if (argc < 2) {
        fprintf(stderr, "Run: %s <num>\n", argv[0]);
        fprintf(stderr, "\t <num> is number of frames per second\n");
        fprintf(stderr, "\t -s make it sync to vblank\n");
        fprintf(stderr, "\t eg. :'%s 24' or '%s -s 120'\n",
                argv[0], argv[0]);
        exit(1);
    }
   for (i = 1; i < argc; ++i) {
       if (argv[i][0] == '-') {
           syncToVblank = (argv[i][1] == 's');
       } else
           desiredFps = atoi(argv[i]);
   }
   printf("Desired FPS = %d\n", desiredFps);
   printf("Sync to vblank = %s\n", syncToVblank ? "yes" : "no");

   /* Set initial projection/viewing transformation.
    * We can't be sure we'll get a ConfigureNotify event when the window
    * first appears.
    */
   reshape(winWidth, winHeight);

   event_loop(dpy, win);

   glXDestroyContext(dpy, ctx);
   XDestroyWindow(dpy, win);
   XCloseDisplay(dpy);

   return 0;
}
