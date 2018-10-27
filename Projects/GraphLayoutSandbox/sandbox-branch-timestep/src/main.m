#import <app.h>
#import <graph.h>
#import <layout.h>
#import <nodes.h>
#import <time.h>

static gboolean area_expose(GtkWidget *, GdkEventExpose *, gpointer);
static void nodeapply_click(GtkButton *, gpointer);
static void edgeapply_click(GtkButton *, gpointer);
static void execute_click(GtkButton *, gpointer);
static void save_click(GtkButton *, gpointer);
static void stop_click(GtkButton *, gpointer);
static gboolean area_press(GtkWidget *, GdkEventButton *, gpointer);
static gboolean area_release(GtkWidget *, GdkEventButton *, gpointer);
static gboolean area_motion(GtkWidget *, GdkEventMotion *, gpointer);
static void wall_toggle(GtkToggleButton *, gpointer);
static gboolean area_config(GtkWidget *, GdkEventConfigure *, gpointer);

SBApp *app = NULL;
Graph *graph = NULL;
Node *drag_node = NULL;
char filename[] = "graph.png";
BOOL run = YES;

int main(int argc, char* argv[])
{
	[Gtk init: &argc: &argv];

	app = [[[SBApp alloc] init] maximize];
	[app->drawarea onExpose: (GCallback) area_expose: NULL];
	[app->drawarea onButtonPress: (GCallback) area_press: NULL];
	[app->drawarea onButtonRelease: (GCallback) area_release: NULL];
	[app->drawarea onMotionNotify: (GCallback) area_motion: NULL];
	[app->generalbox->edgeapply onClicked: (GCallback) edgeapply_click: NULL];
	[app->generalbox->nodeapply onClicked: (GCallback) nodeapply_click: NULL];
	[app->layoutbox->execute onClicked: (GCallback) execute_click: NULL];
	[app->layoutbox->stop onClicked: (GCallback) stop_click: NULL];
	[app->layoutbox->walltog onToggled: (GCallback) wall_toggle: NULL];
	[app->drawarea onConfigure: (GCallback) area_config: NULL];
	[app->filebox->save onClicked: (GCallback) save_click: NULL];

	graph = [[Graph alloc] init];
	(void)area_config(NULL, NULL, NULL);
	
	[Gtk main];
	[graph free];

	return 0;
}

static void execute_click(GtkButton *widget, gpointer data) {
	LayoutOps	*ops = g_malloc(sizeof(LayoutOps));
	char		*type = [app->layoutbox->typecombo activeText];
	time_t		before, after;
	double		time_diff = 0.0,
				min_energy = 0.0;

	if (g_strcmp0(type, "Simple") == 0)
		ops->type = ALG_TYPE_SIMPLE;
	else if (g_strcmp0(type, "Fruchterman-Reingold") == 0)
		ops->type = ALG_TYPE_FR;
	else if (g_strcmp0(type, "Eades") == 0)
		ops->type = ALG_TYPE_EADES;
	else if (g_strcmp0(type, "GEM") == 0)
		ops->type = ALG_TYPE_GEM;
	else if (g_strcmp0(type, "Grid") == 0)
		ops->type = ALG_TYPE_GRID;
	else if (g_strcmp0(type, "Barnes-Hut") == 0)
		ops->type = ALG_TYPE_BH;

	ops->repConst = [app->layoutbox->coulconstspin valueInt];
	ops->attConst = [app->layoutbox->springconstspin valueInt];
	ops->timestep = [app->layoutbox->timestepspin valueInt] / 100.0;
	ops->damping = [app->layoutbox->dampingspin valueInt] / 10.0;

	ops->nodeList = [graph nodeList];
	ops->edgeList = [graph edgeList];
	ops->nodesNum = [ops->nodeList size];
	ops->edgesNum = [ops->edgeList size];
	ops->frameWidth = [graph width];
	ops->frameHeight = [graph height];
	ops->useWall = [graph wall];
	ops->energy = [[Point alloc] init];

	min_energy = ops->nodesNum;

	[app->layoutbox->stop sensitive: YES];
	[app->layoutbox->execute sensitive: NO];

	time(&before);

	while ([Layout forceLayout: ops]) {
		[app->drawarea queueDraw];
		while (g_main_context_iteration(NULL, FALSE));

		g_print("min: %d, x: %d, y: %d, damping: %d\n", (int) min_energy,
				(int) ops->energy->x, (int) ops->energy->y, (int) ops->damping);

		if ((ops->energy->x <= min_energy && ops->energy->y <= min_energy) 
				|| run == NO)
//		if ((ops->energy->x <= 1.0 && ops->energy->y <= 1.0) || run == NO)
			break;
	}

	time(&after);
	time_diff = difftime(after, before);

	g_print("Time: %d\n", (int) time_diff);

	[app->drawarea queueDraw];
	while (g_main_context_iteration(NULL, FALSE));

	run = YES;

	[app->layoutbox->stop sensitive: NO];
	[app->layoutbox->execute sensitive: YES];

	[ops->energy free];
	g_free(ops);
}

static void stop_click(GtkButton *widget, gpointer data) {
	run = NO;
}

static gboolean area_expose(GtkWidget *widget, GdkEventExpose *event, gpointer data) {
	static cairo_t *context;
	context = gdk_cairo_create(widget->window);
	gdk_window_clear(widget->window);
	
	/* clip around the draw area to make things faster */
	cairo_rectangle(context, 0.0, 0.0, 
			widget->allocation.width + widget->allocation.width, 
			widget->allocation.height + widget->allocation.height);
	cairo_clip(context);

	cairo_set_line_width(context, 1.0);

	[graph expose: context];
	cairo_destroy(context);

	return FALSE;
}

static void nodeapply_click(GtkButton *button, gpointer data) {
	int i = 0;
	[[graph nodeList] clear];
	Node *temp;
	while (i < [app->generalbox->nodespin valueInt]) {
		temp = [[Node alloc] init];
//		if (i % 2)
			[graph addNode: [temp size: 10]];
//		else
//			[graph addNode: [temp size: 20]];

		[Nodes nodeRandom: temp: [graph width]: [graph height]];
		i++;
	}

	edgeapply_click(NULL, NULL);
	[app->drawarea queueDraw];
}

static void edgeapply_click(GtkButton *button, gpointer data) {
	[[graph edgeList] clear];
	const char *text = [app->generalbox->edgecombo activeText];
	if (g_strcmp0(text, "None") == 0)
		;
	if (g_strcmp0(text, "Circular") == 0)
		[Nodes edgesCircular: graph];
	else if (g_strcmp0(text, "Centered") == 0)
		[Nodes edgesCentered: graph];
	else if (g_strcmp0(text, "Interconnected") == 0)
		[Nodes edgesInterconnected: graph];
	else if (g_strcmp0(text, "Binary Tree") == 0)
		[Nodes edgesBinaryTree: graph];

	[app->drawarea queueDraw];
}

static gboolean area_press(GtkWidget *widget, GdkEventButton *event, gpointer data) {
	Node *n = [graph getNodeByPos: event->x: event->y];
	if (event->button == 3)			/* right-click */
		[n lock: ![n lock]];
	
	else if (event->button == 1)	/* dragging */
		drag_node = n;

	[app->drawarea queueDraw];
	return FALSE;
}

static gboolean area_release(GtkWidget *widget, GdkEventButton *event, gpointer data) {
	drag_node = NULL;
	return FALSE;
}

static gboolean area_motion(GtkWidget *widget, GdkEventMotion *event, gpointer data) {
	if (drag_node) {
		drag_node->x = event->x;
		drag_node->y = event->y;
		[app->drawarea queueDraw];
	}
	return FALSE;
}

static void wall_toggle(GtkToggleButton *toggle, gpointer data) {
	[graph wall: [app->layoutbox->walltog active]];
	[app->drawarea queueDraw];
}

static gboolean area_config(GtkWidget *widget, GdkEventConfigure *event, gpointer data) {
	[[graph width: [app->drawarea widget]->allocation.width]
			height: [app->drawarea widget]->allocation.height];
	[app->drawarea queueDraw];
	return FALSE;
}

static void save_click(GtkButton *button, gpointer data) {
	int w = [app->drawarea widget]->allocation.width;
	int h = [app->drawarea widget]->allocation.height;

	cairo_surface_t *surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, w, h);
	cairo_t *context = cairo_create(surface);
	cairo_set_source_rgb(context, 1.0, 1.0, 1.0);
	cairo_rectangle(context, 0, 0, w, h);
	cairo_fill(context);
	[graph expose: context];
	cairo_surface_write_to_png(surface, filename);
	cairo_destroy(context);
	cairo_surface_destroy(surface);
}

