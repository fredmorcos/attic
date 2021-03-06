/*
copyright 2003, 2004 Alexander Malmberg <alexander@malmberg.org>
*/

#include "Document.h"

#include <math.h>
#include <time.h>

#include <Foundation/NSFileManager.h>
#include <AppKit/NSBezierPath.h>
#include <AppKit/NSFontManager.h>
#include <AppKit/NSScrollView.h>
#include <AppKit/NSWindow.h>
#include <AppKit/DPSOperators.h>
#include <GNUstepGUI/GSDisplayServer.h>

#include <svg.h>


@interface SVGRenderState : NSObject <NSCopying>
{
@public
	NSWindow *window;

	svg_paint_t fill_paint,stroke_paint;
	double fill_opacity,stroke_opacity;

	svg_color_t color;
	double opacity;

	double stroke_width;

	int fill_rule;

	NSString *font_family;
	svg_font_style_t font_style;
	double font_size;
	double font_weight;
	svg_text_anchor_t text_anchor;

	float *dash;
	int num_dash;
	float dash_offset;
}

@end

@implementation SVGRenderState

-(id) copyWithZone: (NSZone *)zone
{
	SVGRenderState *new=NSCopyObject(self,0,zone);

	[new->window retain];
	[new->font_family retain];

	if (new->dash)
	{
		new->dash=malloc(sizeof(float)*new->num_dash);
		memcpy(new->dash,dash,sizeof(float)*new->num_dash);
	}

	return new;
}

-(void) dealloc
{
	if (dash)
		free(dash);
	DESTROY(window);
	DESTROY(font_family);
	[super dealloc];
}

@end


@interface SVGRenderContext : NSObject
{
@public
	NSGraphicsContext *ctxt;

	NSWindow *result;
	NSSize size;

	double scale;

	SVGRenderState *current;
	NSMutableArray *states;
}

-(void) prepareRender: (double)a_scale;
-(void) finishRender;


-(double) lengthToPoints: (svg_length_t *)l;


-(void) arcTo: (double)rx : (double) ry
	: (double)x_axis_rotation
	: (int)large_arc_flag
	: (int)sweep_flag
	: (double)x : (double)y;


-(svg_status_t) beginGroup: (double)opacity;
-(svg_status_t) endGroup: (double)opacity;

-(svg_status_t) setViewportDimension: (svg_length_t *)width :(svg_length_t *)height;
-(svg_status_t) applyViewbox: (svg_view_box_t)viewbox
	: (svg_length_t *)width : (svg_length_t *)height;

-(svg_status_t) renderRect: (svg_length_t *)x : (svg_length_t *)y
	: (svg_length_t *)width : (svg_length_t *)height
	: (svg_length_t *)rx : (svg_length_t *)ry;
-(svg_status_t) renderPath;
-(svg_status_t) renderText: (const unsigned char *)utf8;
-(svg_status_t) renderEllipse: (svg_length_t *)cx : (svg_length_t *)cy
	: (svg_length_t *)rx : (svg_length_t *)ry;

@end

@implementation SVGRenderContext

-(void) prepareRender: (double)a_scale
{
	DESTROY(result);
	ctxt=GSCurrentContext();
	states=[[NSMutableArray alloc] init];
	current=nil;
	scale=a_scale;
	size=NSMakeSize(500*scale,500*scale);
}

-(void) finishRender
{
//	printf("result=%@, current=%@, states=%i\n",result,current,[states count]);
	ctxt=nil;
	DESTROY(states);
}


-(void) dealloc
{
	DESTROY(result);
	[super dealloc];
}


-(double) lengthToPoints: (svg_length_t *)l
{
	switch (l->unit)
	{
	case SVG_LENGTH_UNIT_PT:
		return l->value*1.25;

	case SVG_LENGTH_UNIT_PX:
		return l->value/*/1.25*/;

	case SVG_LENGTH_UNIT_CM:
		return l->value/2.54*72*1.25;

	case SVG_LENGTH_UNIT_MM:
		return l->value/25.4*72*1.25;

	case SVG_LENGTH_UNIT_IN:
		return l->value*72*1.25;

	case SVG_LENGTH_UNIT_PC:
		return l->value/6*72*1.25;

	case SVG_LENGTH_UNIT_PCT:
		if (l->orientation==SVG_LENGTH_ORIENTATION_HORIZONTAL)
			return l->value/100*size.width/scale;
		else if (l->orientation==SVG_LENGTH_ORIENTATION_VERTICAL)
			return l->value/100*size.height/scale;
		else
			return l->value/100*sqrt(size.width*size.width+size.height*size.height)*sqrt(2)/scale;

	default:
		printf("unhandled unit %i\n",l->unit);
		return l->value;
	}
}

-(void) setColor: (svg_color_t *)c
{
	DPSsetrgbcolor(ctxt,
		svg_color_get_red(c)/255.0,
		svg_color_get_green(c)/255.0,
		svg_color_get_blue(c)/255.0);
}


/*
A few methods based on code in libxsvg:
*/

/* libxsvg - Render XVG documents using the Xr library
 *
 * Copyright � 2002 USC/Information Sciences Institute
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of
 * Information Sciences Institute not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  Information Sciences Institute
 * makes no representations about the suitability of this software for
 * any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * INFORMATION SCIENCES INSTITUTE DISCLAIMS ALL WARRANTIES WITH REGARD
 * TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL INFORMATION SCIENCES
 * INSTITUTE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
 * OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author: Carl Worth <cworth@isi.edu>
 */
/* The ellipse and arc functions below are:
 
   Copyright (C) 2000 Eazel, Inc.
  
   Author: Raph Levien <raph@artofcode.com>

   This is adapted from svg-path in Gill.
*/
/* 4/3 * (1-cos 45Â)/sin 45Â = 4/3 * sqrt(2) - 1 */
#define SVG_ARC_MAGIC ((double) 0.5522847498)
-(void) _pathArcSegment: (double)xc : (double)yc
	: (double)th0 : (double)th1
	: (double)rx : (double)ry : (double)x_axis_rotation
{
    double sin_th, cos_th;
    double a00, a01, a10, a11;
    double x1, y1, x2, y2, x3, y3;
    double t;
    double th_half;

    sin_th = sin (x_axis_rotation * (M_PI / 180.0));
    cos_th = cos (x_axis_rotation * (M_PI / 180.0)); 
    /* inverse transform compared with rsvg_path_arc */
    a00 = cos_th * rx;
    a01 = -sin_th * ry;
    a10 = sin_th * rx;
    a11 = cos_th * ry;

    th_half = 0.5 * (th1 - th0);
    t = (8.0 / 3.0) * sin (th_half * 0.5) * sin (th_half * 0.5) / sin (th_half);
    x1 = xc + cos (th0) - t * sin (th0);
    y1 = yc + sin (th0) + t * cos (th0);
    x3 = xc + cos (th1);
    y3 = yc + sin (th1);
    x2 = x3 + t * sin (th1);
    y2 = y3 - t * cos (th1);

    DPScurveto(ctxt, a00 * x1 + a01 * y1, a10 * x1 + a11 * y1,
		a00 * x2 + a01 * y2, a10 * x2 + a11 * y2,
		a00 * x3 + a01 * y3, a10 * x3 + a11 * y3);
}

/**
 * _xsvg_path_arc_to: Add an arc to the given path
 *
 * rx: Radius in x direction (before rotation).
 * ry: Radius in y direction (before rotation).
 * x_axis_rotation: Rotation angle for axes.
 * large_arc_flag: 0 for arc length <= 180, 1 for arc >= 180.
 * sweep: 0 for "negative angle", 1 for "positive angle".
 * x: New x coordinate.
 * y: New y coordinate.
 *
 **/
-(void) arcTo: (double)rx : (double) ry
	: (double)x_axis_rotation
	: (int)large_arc_flag
	: (int)sweep_flag
	: (double)x
	: (double)y
{
    double sin_th, cos_th;
    double a00, a01, a10, a11;
    double x0, y0, x1, y1, xc, yc;
    double d, sfactor, sfactor_sq;
    double th0, th1, th_arc;
    int i, n_segs;
    double dx, dy, dx1, dy1, Pr1, Pr2, Px, Py, check;
    double curx;
    double cury;

	{
		float x,y;
		DPScurrentpoint(ctxt,&x,&y);
		curx=x;
		cury=y;
	}

    sin_th = sin (x_axis_rotation * (M_PI / 180.0));
    cos_th = cos (x_axis_rotation * (M_PI / 180.0));

    dx = (curx - x) / 2.0;
    dy = (cury - y) / 2.0;
    dx1 =  cos_th * dx + sin_th * dy;
    dy1 = -sin_th * dx + cos_th * dy;
    Pr1 = rx * rx;
    Pr2 = ry * ry;
    Px = dx1 * dx1;
    Py = dy1 * dy1;
    /* Spec : check if radii are large enough */
    check = Px / Pr1 + Py / Pr2;
    if(check > 1)
    {
        rx = rx * sqrt(check);
        ry = ry * sqrt(check);
    }

    a00 = cos_th / rx;
    a01 = sin_th / rx;
    a10 = -sin_th / ry;
    a11 = cos_th / ry;
    x0 = a00 * curx + a01 * cury;
    y0 = a10 * curx + a11 * cury;
    x1 = a00 * x + a01 * y;
    y1 = a10 * x + a11 * y;
    /* (x0, y0) is current point in transformed coordinate space.
       (x1, y1) is new point in transformed coordinate space.
       
       The arc fits a unit-radius circle in this space.
    */
    d = (x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0);
    sfactor_sq = 1.0 / d - 0.25;
    if (sfactor_sq < 0) sfactor_sq = 0;
    sfactor = sqrt (sfactor_sq);
    if (sweep_flag == large_arc_flag) sfactor = -sfactor;
    xc = 0.5 * (x0 + x1) - sfactor * (y1 - y0);
    yc = 0.5 * (y0 + y1) + sfactor * (x1 - x0);
    /* (xc, yc) is center of the circle. */
    
    th0 = atan2 (y0 - yc, x0 - xc);
    th1 = atan2 (y1 - yc, x1 - xc);
    
    th_arc = th1 - th0;
    if (th_arc < 0 && sweep_flag)
	th_arc += 2 * M_PI;
    else if (th_arc > 0 && !sweep_flag)
	th_arc -= 2 * M_PI;

    /* XXX: I still need to evaluate the math performed in this
       function. The critical behavior desired is that the arc must be
       approximated within an arbitrary error tolerance, (which the
       user should be able to specify as well). I don't yet know the
       bounds of the error from the following computation of
       n_segs. Plus the "+ 0.001" looks just plain fishy. -cworth */
    n_segs = ceil (fabs (th_arc / (M_PI * 0.5 + 0.001)));
    
    for (i = 0; i < n_segs; i++) {
	[self _pathArcSegment: xc : yc
					: th0 + i * th_arc / n_segs
					: th0 + (i + 1) * th_arc / n_segs
					: rx : ry : x_axis_rotation];
    }
}


-(svg_status_t) renderRect: (svg_length_t *)x : (svg_length_t *)y
	: (svg_length_t *)width : (svg_length_t *)height
	: (svg_length_t *)rx : (svg_length_t *)ry
{
	double cx,cy,cw,ch;
	double crx,cry;

	cx=[self lengthToPoints: x];
	cy=[self lengthToPoints: y];
	cw=[self lengthToPoints: width];
	ch=[self lengthToPoints: height];
	crx=[self lengthToPoints: rx];
	cry=[self lengthToPoints: ry];

	if (crx>cw/2) crx=cw/2;
	if (cry>ch/2) cry=ch/2;

	switch (current->fill_paint.type)
	{
	case SVG_PAINT_TYPE_COLOR:
		[self setColor: &current->fill_paint.p.color];
		DPSsetalpha(ctxt,current->fill_opacity);
		if (rx>0 || ry>0)
		{
			DPSmoveto(ctxt, cx + crx, cy);
			DPSlineto(ctxt, cx + cw - crx, cy);
			[self arcTo: crx : cry : 0 : 0 : 1 : cx + cw : cy + cry];
			DPSlineto(ctxt, cx + cw, cy + ch - cry);
			[self arcTo: crx : cry : 0 : 0 : 1 : cx + cw - crx : cy + ch];
			DPSlineto(ctxt, cx + crx, cy + ch);
			[self arcTo: crx : cry : 0 : 0 : 1 : cx : cy + ch - cry];
			DPSlineto(ctxt, cx, cy + cry);
			[self arcTo: crx : cry : 0 : 0 : 1 : cx + crx : cy];
			DPSclosepath(ctxt);
			DPSfill(ctxt);
		}
		else
			DPSrectfill(ctxt,cx,cy,cw,ch);
		break;
/*
	case SVG_PAINT_TYPE_CURRENTCOLOR:
		[self setColor: &current->color];
		DPSsetalpha(ctxt,current->opacity);
		if (rx>0 || ry>0)
		{
			DPSmoveto(ctxt, cx + crx, cy);
			DPSlineto(ctxt, cx + cw - crx, cy);
			[self arcTo: crx : cry : 0 : 0 : 1 : cx + cw : cy + cry];
			DPSlineto(ctxt, cx + cw, cy + ch - cry);
			[self arcTo: crx : cry : 0 : 0 : 1 : cx + cw - crx : cy + ch];
			DPSlineto(ctxt, cx + crx, cy + ch);
			[self arcTo: crx : cry : 0 : 0 : 1 : cx : cy + ch - cry];
			DPSlineto(ctxt, cx, cy + cry);
			[self arcTo: crx : cry : 0 : 0 : 1 : cx + crx : cy];
			DPSclosepath(ctxt);
			DPSfill(ctxt);
		}
		else
			DPSrectfill(ctxt,cx,cy,cw,ch);
		break;
*/
	case SVG_PAINT_TYPE_NONE:
		break;
	}

	switch (current->stroke_paint.type)
	{
	case SVG_PAINT_TYPE_COLOR:
		[self setColor: &current->stroke_paint.p.color];
		DPSsetalpha(ctxt,current->stroke_opacity);
		DPSrectstroke(ctxt,cx,cy,cw,ch);
		break;
/*
	case SVG_PAINT_TYPE_CURRENTCOLOR:
		[self setColor: &current->color];
		DPSsetalpha(ctxt,current->opacity);
		DPSrectstroke(ctxt,cx,cy,cw,ch);
		break;
*/
	case SVG_PAINT_TYPE_NONE:
		break;
	}

	return SVG_STATUS_SUCCESS;
}

-(svg_status_t) renderEllipse: (svg_length_t *)lcx : (svg_length_t *)lcy
	: (svg_length_t *)lrx : (svg_length_t *)lry
{
	double cx,cy,rx,ry;

	cx=[self lengthToPoints: lcx];
	cy=[self lengthToPoints: lcy];
	rx=[self lengthToPoints: lrx];
	ry=[self lengthToPoints: lry];

	DPSmoveto(ctxt, cx + rx, cy);
	DPScurveto(ctxt, cx + rx, cy + ry * SVG_ARC_MAGIC, cx + rx * SVG_ARC_MAGIC, cy + ry, cx, cy + ry);
	DPScurveto(ctxt, cx - rx * SVG_ARC_MAGIC, cy + ry, cx - rx, cy + ry * SVG_ARC_MAGIC, cx - rx, cy);
	DPScurveto(ctxt, cx - rx, cy - ry * SVG_ARC_MAGIC, cx - rx * SVG_ARC_MAGIC, cy - ry, cx, cy - ry);
	DPScurveto(ctxt, cx + rx * SVG_ARC_MAGIC, cy - ry, cx + rx, cy - ry * SVG_ARC_MAGIC, cx + rx, cy);
	DPSclosepath(ctxt);
	[self renderPath];

	return SVG_STATUS_SUCCESS;
}

/*
End of methods based on libxsvg code.
*/


-(svg_status_t) beginGroup: (double)opacity
{
	if (current)
	{
		if (!result)
		{
			printf("beginGroup: with current but no result\n");
			return SVG_STATUS_INVALID_CALL;
		}
		current=[current copy];

		DPSgsave(ctxt);
		if (opacity<1.0)
		{
			NSAffineTransform *ctm=GSCurrentCTM(ctxt);

			DESTROY(current->window);
			current->window=[[NSWindow alloc]
				initWithContentRect: NSMakeRect(0,0,size.width,size.height)
				styleMask: 0
				backing: NSBackingStoreRetained
				defer: NO];
			[current->window setReleasedWhenClosed: NO];
			[GSCurrentServer() windowdevice: [current->window windowNumber]];
			GSSetCTM(ctxt,ctm);

			DPSgsave(ctxt);
			DPSinitmatrix(ctxt);
			DPSinitclip(ctxt);
			DPScompositerect(ctxt,0,0,size.width,size.height,NSCompositeClear);
			DPSgrestore(ctxt);
		}
	}
	else
	{
		DPSgsave(ctxt);
		current=[[SVGRenderState alloc] init];
	}
	[states addObject: current];
	[current release];

	return SVG_STATUS_SUCCESS;
}

-(svg_status_t) endGroup: (double)opacity
{
	DPSgrestore(ctxt);

	if (opacity!=1.0)
	{
		NSWindow *w=[current->window retain];
		DPSgsave(ctxt);
		DPSinitmatrix(ctxt);
		DPSinitclip(ctxt);
		DPSdissolve(ctxt,0,0,size.width,size.height,[w gState],0,0,opacity);
		DPSgrestore(ctxt);
	}

	[states removeObjectAtIndex: [states count]-1];
	if ([states count])
		current=[states objectAtIndex: [states count]-1];
	else
		current=nil;

	return SVG_STATUS_SUCCESS;
}


-(svg_status_t) setViewportDimension: (svg_length_t *)width :(svg_length_t *)height
{
	int w,h;

	if (result)
	{
		printf("setViewportDimension: Already have size, ignoring.\n");
		return SVG_STATUS_SUCCESS;
	}

	w=ceil([self lengthToPoints: width])*scale;
	h=ceil([self lengthToPoints: height])*scale;
	size=NSMakeSize(w,h);

	result=current->window=[[NSWindow alloc]
		initWithContentRect: NSMakeRect(0,0,size.width,size.height)
		styleMask: 0
		backing: NSBackingStoreRetained
		defer: NO];
	[current->window setReleasedWhenClosed: NO];
	[result retain];

	[GSCurrentServer() windowdevice: [current->window windowNumber]];
	DPSinitmatrix(ctxt);
	DPSinitclip(ctxt);
	DPScompositerect(ctxt,0,0,size.width,size.height,NSCompositeClear);
	DPStranslate(ctxt,0,size.height);
	DPSscale(ctxt,scale,-scale);

	return SVG_STATUS_SUCCESS;
}

-(svg_status_t) applyViewbox: (svg_view_box_t)viewbox
	: (svg_length_t *)width : (svg_length_t *)height
{
	double w,h;
	w=[self lengthToPoints: width];
	h=[self lengthToPoints: height];
	DPSscale(ctxt,w/viewbox.box.width,h/viewbox.box.height);
	DPStranslate(ctxt,-viewbox.box.x,-viewbox.box.y);
	return SVG_STATUS_SUCCESS;
}


-(svg_status_t) renderPath
{
	switch (current->fill_paint.type)
	{
	case SVG_PAINT_TYPE_COLOR:
		[self setColor: &current->fill_paint.p.color];
		DPSsetalpha(ctxt,current->fill_opacity);
		if (current->stroke_paint.type!=SVG_PAINT_TYPE_NONE)
			DPSgsave(ctxt);
		if (current->fill_rule)
			DPSeofill(ctxt);
		else
			DPSfill(ctxt);
		if (current->stroke_paint.type!=SVG_PAINT_TYPE_NONE)
			DPSgrestore(ctxt);
		break;
/*
	case SVG_PAINT_TYPE_CURRENTCOLOR:
		[self setColor: &current->color];
		DPSsetalpha(ctxt,current->opacity);
		if (current->stroke_paint.type!=SVG_PAINT_TYPE_NONE)
			DPSgsave(ctxt);
		if (current->fill_rule)
			DPSeofill(ctxt);
		else
			DPSfill(ctxt);
		if (current->stroke_paint.type!=SVG_PAINT_TYPE_NONE)
			DPSgrestore(ctxt);
		break;
*/
	case SVG_PAINT_TYPE_NONE:
		break;
	}

	switch (current->stroke_paint.type)
	{
	case SVG_PAINT_TYPE_COLOR:
		[self setColor: &current->stroke_paint.p.color];
		DPSsetalpha(ctxt,current->stroke_opacity);
		DPSstroke(ctxt);
		break;
/*
	case SVG_PAINT_TYPE_CURRENTCOLOR:
		[self setColor: &current->color];
		DPSsetalpha(ctxt,current->opacity);
		DPSstroke(ctxt);
		break;
*/
	case SVG_PAINT_TYPE_NONE:
		break;
	}

	return SVG_STATUS_SUCCESS;
}


-(svg_status_t) renderText: (const unsigned char *)utf8
{
	NSFont *f;
	NSFontManager *fm;
	NSArray *fonts,*font;
	int w=ceil(current->font_weight/80);
	int score,best;
	int i;

	if (utf8 == NULL)
	  return SVG_STATUS_SUCCESS;
	
	fm=[NSFontManager sharedFontManager];

	{
		NSArray *families;
		NSString *family;

		families=[current->font_family componentsSeparatedByString: @","];

		fonts=nil;
		for (i=0;i<[families count];i++)
		{
			family=[families objectAtIndex: i];

			family=[family stringByTrimmingCharactersInSet: [NSCharacterSet whitespaceCharacterSet]];
			if ([family hasPrefix: @"'"])
				family=[[family substringToIndex: [family length]-1] substringFromIndex: 1];

			if ([family isEqual: @"serif"])
				family=@"Times";
			else if ([family isEqual: @"sans-serif"])
				family=@"Helvetica";
			else if ([family isEqual: @"monospace"])
				family=@"Courier";

			fonts=[fm availableMembersOfFontFamily: family];
			if (!fonts || ![fonts count])
				fonts=[fm availableMembersOfFontFamily: [family capitalizedString]];
			if (fonts && [fonts count])
				break;
		}


		if (!fonts || ![fonts count])
			fonts=[fm availableMembersOfFontFamily: @"Helvetica"];
	}

	f=nil;
	best=1e6;
	for (i=0;i<[fonts count];i++)
	{
		unsigned int traits;

		font=[fonts objectAtIndex: i];
		score=abs([[font objectAtIndex: 2] intValue]-w);

		traits=[[font objectAtIndex: 3] unsignedIntValue]&~NSBoldFontMask;
		if (current->font_style)
		{
			if (!(traits&NSItalicFontMask))
				score+=10;
			else
				traits&=~NSItalicFontMask;
		}

		if (traits)
			score+=10;

		if (score<best)
		{
			best=score;
			f=[NSFont fontWithName: [font objectAtIndex: 0]
				size: current->font_size];
		}
	}

	if (!f)
		f=[NSFont userFontOfSize: current->font_size];

	DPSscale(ctxt,1,-1);
	[f set];

	switch (current->fill_paint.type)
	{
	case SVG_PAINT_TYPE_COLOR:
		[self setColor: &current->fill_paint.p.color];
		DPSsetalpha(ctxt,current->fill_opacity);
		DPSshow(ctxt,utf8);
		break;
/*
	case SVG_PAINT_TYPE_CURRENTCOLOR:
		[self setColor: &current->color];
		DPSsetalpha(ctxt,current->opacity);
		DPSshow(ctxt,utf8);
		break;
*/
	case SVG_PAINT_TYPE_NONE:
		break;
	}

	switch (current->stroke_paint.type)
	{
	case SVG_PAINT_TYPE_COLOR:
		[self setColor: &current->stroke_paint.p.color];
		DPSsetalpha(ctxt,current->stroke_opacity);
		DPScharpath(ctxt,utf8,0);
		DPSstroke(ctxt);
		break;
/*
	case SVG_PAINT_TYPE_CURRENTCOLOR:
		[self setColor: &current->color];
		DPSsetalpha(ctxt,current->opacity);
		DPScharpath(ctxt,utf8,0);
		DPSstroke(ctxt);
		break;
*/
	case SVG_PAINT_TYPE_NONE:
		break;
	}

	DPSscale(ctxt,1,-1);

	return SVG_STATUS_SUCCESS;
}


@end


#define FUNC(name,args...) \
	static svg_status_t r_##name(void *closure, ##args) \
	{ \
		SVGRenderContext *self=(SVGRenderContext *)closure; \


static int indent=1;
#define I printf("%*c",indent,' ')

static svg_status_t r_begin_group(void *closure,double opacity)
{
	SVGRenderContext *self=(SVGRenderContext *)closure;
//	I;printf("begin_group(%g)\n",opacity);
	indent+=3;

	return [self beginGroup: opacity];
}

static svg_status_t r_begin_element(void *closure)
{
	SVGRenderContext *self=(SVGRenderContext *)closure;
//	I;printf("begin_element()\n");
	indent+=3;

	DPSgsave(self->ctxt);
	self->current=[self->current copy];
	[self->states addObject: self->current];
	[self->current release];

	return SVG_STATUS_SUCCESS;
}

static svg_status_t r_end_element(void *closure)
{
	SVGRenderContext *self=(SVGRenderContext *)closure;
	indent-=3;
//	I;printf("end_element()\n");

	DPSgrestore(self->ctxt);
	[self->states removeObjectAtIndex: [self->states count]-1];
	if ([self->states count])
		self->current=[self->states objectAtIndex: [self->states count]-1];
	else
		self->current=nil;

	return SVG_STATUS_SUCCESS;
}

static svg_status_t r_end_group(void *closure,double opacity)
{
	SVGRenderContext *self=(SVGRenderContext *)closure;
	indent-=3;
//	I;printf("end_group(%g)\n",opacity);

	return [self endGroup: opacity];
}


FUNC(move_to,double x,double y)
/*	I;printf("move_to((%g %i %i),(%g %i %i))\n",
		x->value,x->unit,x->orientation,
		y->value,y->unit,y->orientation);*/

//	DPSmoveto(self->ctxt,[self lengthToPoints: x],[self lengthToPoints: y]);
	DPSmoveto(self->ctxt,x,y);
	return SVG_STATUS_SUCCESS;
}

FUNC(line_to,double x,double y)
/*	I;printf("line_to((%g %i %i),(%g %i %i))\n",
		x->value,x->unit,x->orientation,
		y->value,y->unit,y->orientation);*/

//	DPSlineto(self->ctxt,[self lengthToPoints: x],[self lengthToPoints: y]);
	DPSlineto(self->ctxt,x,y);	
	return SVG_STATUS_SUCCESS;
}

FUNC(curve_to,
     double x1,double y1,
     double x2,double y2,
     double x3,double y3)

/*	I;printf("curve_to((%g %i %i),(%g %i %i), (%g %i %i),(%g %i %i), (%g %i %i),(%g %i %i))\n",
		x1->value,x1->unit,x1->orientation,
		y1->value,y1->unit,y1->orientation,
		x2->value,x2->unit,x2->orientation,
		y2->value,y2->unit,y2->orientation,
		x3->value,x3->unit,x3->orientation,
		y3->value,y3->unit,y3->orientation
		);*/
/*	DPScurveto(self->ctxt,
		[self lengthToPoints: x1],[self lengthToPoints: y1],
		[self lengthToPoints: x2],[self lengthToPoints: y2],
		[self lengthToPoints: x3],[self lengthToPoints: y3]);
*/
	DPScurveto(self->ctxt,x1,y1,x2,y2,x3,y3);
	return SVG_STATUS_SUCCESS;
}

FUNC(quadratic_curve_to,
        double x1, 
        double y1,
        double x2,
        double y2)
  float x,y;
//  I;printf("quadratic_curve_to(%g %g %g %g)\n",x1,y1,x2,y2);
  DPScurrentpoint(self->ctxt,&x,&y);
  DPScurveto(self->ctxt,
  	x + 2.0/3.0 * (x1 - x),
	y + 2.0/3.0 * (y1 - y),
	x2 + 2.0/3.0 * (x1 - x2),
	y2 + 2.0/3.0 * (y1 - y2),
	x2,y2);
	return SVG_STATUS_SUCCESS;
}

FUNC(arc_to,
     double rx,double ry,
     double x_axis_rotation,
     int large_arc_flag,
     int sweep_flag,
     double x,double y)

/*	I;printf("arc_to((%g %i %i),(%g %i %i), %g, %i, %i, (%g %i %i),(%g %i %i))\n",
		rx->value,rx->unit,rx->orientation,
		ry->value,ry->unit,ry->orientation,
		x_axis_rotation,
		large_arc_flag,
		sweep_flag,
		x->value,x->unit,x->orientation,
		y->value,y->unit,y->orientation
		);*/
/*
	[self arcTo: [self lengthToPoints: rx] : [self lengthToPoints: ry]
		: x_axis_rotation
		: large_arc_flag
		: sweep_flag
		: [self lengthToPoints: x] : [self lengthToPoints: y]];
*/
	[self arcTo: rx 
		: ry 
		: x_axis_rotation
		: large_arc_flag
		: sweep_flag
	        : x
		: y];
	return SVG_STATUS_SUCCESS;
}

FUNC(close_path)
//	I;printf("close_path()\n");

	DPSclosepath(self->ctxt);
	return SVG_STATUS_SUCCESS;
}


FUNC(set_color,const svg_color_t *color)
//	I;printf("set_color(%08x)\n",color->rgb);
	self->current->color=*color;
	return SVG_STATUS_SUCCESS;
}

FUNC(set_fill_opacity,double opacity)
//	I;printf("set_fill_opacity(%g)\n",opacity);
	self->current->fill_opacity=opacity;
	return SVG_STATUS_SUCCESS;
}

FUNC(set_fill_paint,const svg_paint_t *paint)
//	I;printf("set_fill_paint((%i %08x))\n",paint->type,paint->p.color.rgb);
	self->current->fill_paint=*paint;
	return SVG_STATUS_SUCCESS;
}

FUNC(set_fill_rule,svg_fill_rule_t fill_rule)
//	I;printf("set_fill_rule(%i)\n",fill_rule);
	if (fill_rule==SVG_FILL_RULE_NONZERO)
		self->current->fill_rule=0;
	else
		self->current->fill_rule=1;
	return SVG_STATUS_SUCCESS;
}

FUNC(set_font_family,const char *family)
//	I;printf("set_font_family(%s)\n",family);
	ASSIGN(self->current->font_family,[NSString stringWithCString: family]);
	return SVG_STATUS_SUCCESS;
}

FUNC(set_font_size,double size)
//	I;printf("set_font_size(%g)\n",size);
	self->current->font_size=size;
	return SVG_STATUS_SUCCESS;
}

FUNC(set_font_style,svg_font_style_t style)
//	I;printf("set_font_style(%i)\n",style);
	self->current->font_style=style;
	return SVG_STATUS_SUCCESS;
}

FUNC(set_font_weight,unsigned int weight)
//	I;printf("set_font_weight(%i)\n",weight);
	self->current->font_weight=weight;
	return SVG_STATUS_SUCCESS;
}

FUNC(set_opacity,double opacity)
//	I;printf("set_opacity(%g)\n",opacity);
	self->current->opacity=opacity;
	return SVG_STATUS_SUCCESS;
}

FUNC(set_stroke_dash_array,double *dashes,int num_dashes)
/*	I;printf("set_stroke_dash_array(%p,%i)\n",
		dashes,num_dashes);*/

	if (self->current->dash)
		free(self->current->dash);
	self->current->dash=NULL;
	self->current->num_dash=0;

	if (dashes && num_dashes)
	{
		float *dash=malloc(sizeof(float)*num_dashes);
		int i;
		for (i=0;i<num_dashes;i++)
			dash[i]=dashes[i];
		self->current->dash=dash;
		self->current->num_dash=num_dashes;
		DPSsetdash(self->ctxt,
			self->current->dash,
			self->current->num_dash,
			self->current->dash_offset);
	}
	else
		DPSsetdash(self->ctxt,NULL,0,0.0);

	return SVG_STATUS_SUCCESS;
}

FUNC(set_stroke_dash_offset,svg_length_t *offset)
/*	I;printf("set_stroke_dash_offset((%g %i %i))\n",
		offset->value,offset->unit,offset->orientation
		);*/
	self->current->dash_offset=[self lengthToPoints: offset];
	DPSsetdash(self->ctxt,
		self->current->dash,
		self->current->num_dash,
		self->current->dash_offset);
	return SVG_STATUS_SUCCESS;
}

FUNC(set_stroke_line_cap,svg_stroke_line_cap_t line_cap)
	int i;
//	I;printf("set_stroke_line_cap(%i)\n",line_cap);

	switch (line_cap)
	{
	default:
	case SVG_STROKE_LINE_CAP_BUTT:
		i=NSButtLineCapStyle;
		break;
	case SVG_STROKE_LINE_CAP_ROUND:
		i=NSRoundLineCapStyle;
		break;
	case SVG_STROKE_LINE_CAP_SQUARE:
		i=NSSquareLineCapStyle;
		break;
	}
	DPSsetlinecap(self->ctxt,i);

	return SVG_STATUS_SUCCESS;
}

FUNC(set_stroke_line_join,svg_stroke_line_join_t line_join)
	int i;
//	I;printf("set_stroke_line_join(%i)\n",line_join);

	switch (line_join)
	{
	case SVG_STROKE_LINE_JOIN_BEVEL:
		i=NSBevelLineJoinStyle;
		break;
	default:
	case SVG_STROKE_LINE_JOIN_MITER:
		i=NSMiterLineJoinStyle;
		break;
	case SVG_STROKE_LINE_JOIN_ROUND:
		i=NSRoundLineJoinStyle;
		break;
	}
	DPSsetlinejoin(self->ctxt,i);

	return SVG_STATUS_SUCCESS;
}

FUNC(set_stroke_miter_limit,double miter_limit)
//	I;printf("set_stroke_miter_limit(%g)\n",miter_limit);
	DPSsetmiterlimit(self->ctxt,miter_limit);
	return SVG_STATUS_SUCCESS;
}

FUNC(set_stroke_opacity,double opacity)
//	I;printf("set_stroke_opacity(%g)\n",opacity);
	self->current->stroke_opacity=opacity;
	return SVG_STATUS_SUCCESS;
}

FUNC(set_stroke_paint,const svg_paint_t *paint)
//	I;printf("set_stroke_paint((%i %08x))\n",paint->type,paint->p.color.rgb);
	self->current->stroke_paint=*paint;
	return SVG_STATUS_SUCCESS;
}

FUNC(set_stroke_width,svg_length_t *width)
/*	I;printf("set_stroke_width((%g %i %i)\n",
		width->value,width->unit,width->orientation
		);*/
	DPSsetlinewidth(self->ctxt,[self lengthToPoints: width]);
	return SVG_STATUS_SUCCESS;
}

FUNC(set_text_anchor,svg_text_anchor_t anchor)
//	I;printf("set_text_anchor(%i)\n",anchor);
	self->current->text_anchor=anchor;
	return SVG_STATUS_SUCCESS;
}


FUNC(transform,double a,double b,double c,double d,double e,double f)
	float mat[6];
//	I;printf("transform(%g,%g,%g,%g,%g,%g)\n",a,b,c,d,e,f);
	mat[0]=a;
	mat[1]=b;
	mat[2]=c;
	mat[3]=d;
	mat[4]=e;
	mat[5]=f;
	DPSconcat(self->ctxt,mat);
	return SVG_STATUS_SUCCESS;
}

FUNC(apply_viewbox,svg_view_box_t viewbox,svg_length_t *width,svg_length_t *height)
/*	I;printf("apply_viewbox(((%g %g)+(%g %g),%i,%i),(%g %i %i),(%g %i %i))\n",
		viewbox.box.x,viewbox.box.y,viewbox.box.width,viewbox.box.height,
		viewbox.aspectratio,viewbox.meet_or_slice,

		width->value,width->unit,width->orientation,
		height->value,height->unit,height->orientation
		);*/
	return [self applyViewbox: viewbox :width:height];
}

FUNC(set_viewport_dimension,svg_length_t *width,svg_length_t *height)
/*	I;printf("set_viewport_dimension((%g %i %i),(%g %i %i))\n",
		width->value,width->unit,width->orientation,
		height->value,height->unit,height->orientation
		);*/
	return [self setViewportDimension: width:height];
}

FUNC(render_line,
        svg_length_t *x1,
        svg_length_t *y1, 
        svg_length_t *x2,
        svg_length_t *y2)
//	I;printf("is this right? x1 %g y1 %g x2 %g y2 %g\n",[self lengthToPoints:x1],[self lengthToPoints:y1],[self lengthToPoints:x2],[self lengthToPoints:y2]);
	DPSmoveto(self->ctxt,[self lengthToPoints:x1],[self lengthToPoints:y1]);
	DPSlineto(self->ctxt,[self lengthToPoints:x2],[self lengthToPoints:y2]);
        [self renderPath];
        return SVG_STATUS_SUCCESS;
}

FUNC(render_path)
//	I;printf("render_path()\n");
	return [self renderPath];
}

FUNC(render_ellipse,
     svg_length_t *cx,svg_length_t *cy,
	  svg_length_t *rx,svg_length_t *ry)
/*	I;printf("render_ellipse((%g %i %i),(%g %i %i), (%g %i %i),(%g %i %i))\n",
		cx->value,cx->unit,cx->orientation,
		cy->value,cy->unit,cy->orientation,
		rx->value,rx->unit,rx->orientation,
		ry->value,ry->unit,ry->orientation);*/

	return [self renderEllipse: cx : cy : rx : ry];
}

FUNC(render_rect,
     svg_length_t *x,svg_length_t *y,
	  svg_length_t *width,svg_length_t *height,
	  svg_length_t *rx,svg_length_t *ry)

/*	I;printf("render_rect((%g %i %i),(%g %i %i), (%g %i %i),(%g %i %i), (%g %i %i),(%g %i %i))\n",
		x->value,x->unit,x->orientation,
		y->value,y->unit,y->orientation,
		width->value,width->unit,width->orientation,
		height->value,height->unit,height->orientation,
		rx->value,rx->unit,rx->orientation,
		ry->value,ry->unit,ry->orientation);*/

	return [self renderRect: x:y :width:height :rx:ry];
}

FUNC(render_text,svg_length_t *x,svg_length_t *y,unsigned char *utf8)
//	I;printf("render_text(\"%s\")\n",utf8);
	
	DPSmoveto(self->ctxt,[self lengthToPoints:x],[self lengthToPoints:y]);
	return [self renderText: utf8];
}

FUNC(render_image,
     unsigned char *data,
     unsigned int data_width,unsigned int data_height,
     svg_length_t *x,svg_length_t *y,
     svg_length_t *width,svg_length_t *height)

/*	I;printf("render_image(%p,%ix%i,(%g %i %i),(%g %i %i),(%g %i %i),(%g %i %i))\n",
		data,data_width,data_height,
		x->value,x->unit,x->orientation,
		y->value,y->unit,y->orientation,
		width->value,width->unit,width->orientation,
		height->value,height->unit,height->orientation);*/

	{
		double cx,cy,cw,ch;
		cx=[self lengthToPoints: x];
		cy=[self lengthToPoints: y];
		cw=[self lengthToPoints: width];
		ch=[self lengthToPoints: height];
		NSDrawBitmap(
			NSMakeRect(cx,cy,cw,ch),
			data_width,data_height,
			8,4,32,data_width*4,NO,YES,NSDeviceRGBColorSpace,
			(const unsigned char * const *)&data);
	}

	return SVG_STATUS_SUCCESS;
}


static svg_render_engine_t engine=
{
r_begin_group,
r_begin_element,
r_end_element,
r_end_group,

r_move_to,
r_line_to,
r_curve_to,
r_quadratic_curve_to,
r_arc_to,
r_close_path,

r_set_color,
r_set_fill_opacity,
r_set_fill_paint,
r_set_fill_rule,
r_set_font_family,
r_set_font_size,
r_set_font_style,
r_set_font_weight,
r_set_opacity,
r_set_stroke_dash_array,
r_set_stroke_dash_offset,
r_set_stroke_line_cap,
r_set_stroke_line_join,
r_set_stroke_miter_limit,
r_set_stroke_opacity,
r_set_stroke_paint,
r_set_stroke_width,
r_set_text_anchor,

r_transform,
r_apply_viewbox,
r_set_viewport_dimension,

r_render_line,
r_render_path,
r_render_ellipse,
r_render_rect,
r_render_text,
r_render_image
};


@interface SVGView : NSView
{
	SVGRenderContext *svg;
}

-(void) setSVGRenderContext: (SVGRenderContext *)s;

@end

@implementation SVGView

-(void) setSVGRenderContext: (SVGRenderContext *)s
{
	ASSIGN(svg,s);
	[self setNeedsDisplay: YES];
}

-(void) dealloc
{
	DESTROY(svg);
	[super dealloc];
}

-(BOOL) isOpaque
{
	return YES;
}

-(void) drawRect: (NSRect)r
{
	NSGraphicsContext *ctxt=GSCurrentContext();

	if (svg)
	{
		DPSsetgray(ctxt,1.0);
		DPSrectfill(ctxt,0,0,svg->size.width,svg->size.height);
		DPScomposite(ctxt,0,0,svg->size.width,svg->size.height,[svg->result gState],0,0,NSCompositeSourceOver);
	}
}

@end


@implementation Document

+(void) openFile: (NSString *)apath
{
	[[self alloc] initWithFile: apath];
}


-(void) reload: (id)sender
{
	{
		svg_t *svg;
		clock_t t;
		SVGRenderContext *svg_render_context=[[SVGRenderContext alloc] init];

		svg_create(&svg);
//		printf("parsing...\n");
		svg_parse(svg,[[NSFileManager defaultManager] fileSystemRepresentationWithPath: path]);

//		printf("rendering...\n");
		t=clock();
		[svg_render_context prepareRender: scale];
		svg_render(svg,&engine,svg_render_context);
		[svg_render_context finishRender];
		t=clock()-t;
//		printf("done: %15.8f seconds\n",t/(double)CLOCKS_PER_SEC);

		[svg_view setFrame: NSMakeRect(0,0,svg_render_context->size.width,svg_render_context->size.height)];
		[svg_view setSVGRenderContext: svg_render_context];
		[svg_render_context release];

		svg_destroy(svg);
	}
}


- initWithFile: (NSString *)apath
{
	NSWindow *win;

	win=[[NSWindow alloc] initWithContentRect: NSMakeRect(100,100,450,300)
		styleMask: NSClosableWindowMask|NSTitledWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask
		backing: NSBackingStoreRetained
		defer: YES];
	if (!(self=[super initWithWindow: win])) return nil;
	ASSIGN(path,apath);
	[win setTitleWithRepresentedFilename: path];
	[win setDelegate: self];

	[NSObject enableDoubleReleaseCheck: YES];


	scale=1.0;

	svg_view=[[SVGView alloc]
		initWithFrame: NSMakeRect(0,0,10,10)];

	[self reload: nil];

	scroll_view=[[NSScrollView alloc] init];
	[scroll_view setDocumentView: svg_view];
	[svg_view release];
	[scroll_view setHasHorizontalScroller: YES];
	[scroll_view setHasVerticalScroller: YES];

	[win setContentView: scroll_view];
	[scroll_view release];

	[win release];

	[self showWindow: nil];


	return self;
}


-(void) windowWillClose: (NSNotification *)n
{
	[self autorelease];
}

-(void) dealloc
{
	DESTROY(path);
	[super dealloc];
}


#define SCALE(a,b) \
	-(void) scale_##a##_##b: (id)sender \
	{ \
		scale=a##.##b; \
		[self reload: nil]; \
	}

SCALE(0,1)
SCALE(0,25)
SCALE(0,5)
SCALE(0,75)
SCALE(1,0)
SCALE(1,5)
SCALE(2,0)
SCALE(3,0)
SCALE(4,0)
SCALE(5,0)

@end

