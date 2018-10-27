
#include <xcb/xcb.h>
#include <xcb/xcb_aux.h>
#include <xcb/render.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/*
 * FUNCTION PROTOTYPES
 */
void print_version_info(xcb_render_query_version_reply_t *reply);
int print_formats_info(xcb_render_query_pict_formats_reply_t *reply);
int draw_window(xcb_connection_t *conn, xcb_render_query_pict_formats_reply_t *reply);
xcb_render_pictformat_t get_pictformat_from_visual(xcb_render_query_pict_formats_reply_t *reply, xcb_visualid_t visual);
xcb_render_pictforminfo_t *get_pictforminfo(xcb_render_query_pict_formats_reply_t *reply, xcb_render_pictforminfo_t *query);

xcb_connection_t   *c;
xcb_render_pictformat_t pf;

xcb_render_fixed_t make_fixed(int16_t i, int16_t f)
{
    return (i << 16) | (f & 0xffff);
}

void print_version_info(xcb_render_query_version_reply_t *reply)
{
    
    fprintf(stdout, "Render Version: %d.%d\n", reply->major_version, 
            reply->minor_version);
}

int print_formats_info(xcb_render_query_pict_formats_reply_t *reply)
{
    xcb_render_pictforminfo_t *first_forminfo;
    int num_formats;
    int num_screens;
    int num_depths;
    int num_visuals;
    xcb_render_pictforminfo_iterator_t forminfo_iter;
    xcb_render_pictscreen_iterator_t     screen_iter;
    
    forminfo_iter = xcb_render_query_pict_formats_formats_iterator(reply);
    screen_iter =  xcb_render_query_pict_formats_screens_iterator(reply);

    fprintf(stdout, "Number of PictFormInfo iterations: %d\n", forminfo_iter.rem);

    num_formats = reply->num_formats;
    first_forminfo = forminfo_iter.data;
    pf = first_forminfo->id;
    while(forminfo_iter.rem)
    {
        xcb_render_pictforminfo_t *forminfo = (xcb_render_pictforminfo_t *)forminfo_iter.data;

        fprintf(stdout, "PICTFORMINFO #%d\n", 1 + num_formats - forminfo_iter.rem);
        fprintf(stdout, "    PICTFORMAT ID:          %d\n", forminfo->id);
        fprintf(stdout, "    PICTFORMAT Type:        %d\n", forminfo->type);
        fprintf(stdout, "    PICTFORMAT Depth:       %d\n", forminfo->depth);
        fprintf(stdout, "        Direct RedShift:    %d\n", forminfo->direct.red_shift);
        fprintf(stdout, "        Direct RedMask:     %d\n", forminfo->direct.red_mask);
        fprintf(stdout, "        Direct BlueShift:   %d\n", forminfo->direct.blue_shift);
        fprintf(stdout, "        Direct BlueMask:    %d\n", forminfo->direct.blue_mask);
        fprintf(stdout, "        Direct GreenShift:  %d\n", forminfo->direct.green_shift);
        fprintf(stdout, "        Direct GreenMask:   %d\n", forminfo->direct.green_mask);
        fprintf(stdout, "        Direct AlphaShift:  %d\n", forminfo->direct.alpha_shift);
        fprintf(stdout, "        Direct AlphaMask:   %d\n", forminfo->direct.alpha_mask);
        fprintf(stdout, "\n");
        xcb_render_pictforminfo_next(&forminfo_iter);
    }

    num_screens = reply->num_screens;
    while(screen_iter.rem)
    {
        xcb_render_pictdepth_iterator_t depth_iter;
        xcb_render_pictscreen_t *cscreen = screen_iter.data;
        
        fprintf(stdout, "Screen #%d\n", 1 + num_screens - screen_iter.rem);
        fprintf(stdout, "    Depths for this screen:    %d\n", cscreen->num_depths);
        fprintf(stdout, "    Fallback PICTFORMAT:       %d\n", cscreen->fallback);
        depth_iter = xcb_render_pictscreen_depths_iterator(cscreen);

        num_depths = cscreen->num_depths;
        while(depth_iter.rem)
        {
            xcb_render_pictvisual_iterator_t    visual_iter;
            xcb_render_pictdepth_t *cdepth = depth_iter.data;

            fprintf(stdout, "    Depth #%d\n", 1 + num_depths - depth_iter.rem);
            fprintf(stdout, "        Visuals for this depth:    %d\n", cdepth->num_visuals);
            fprintf(stdout, "        Depth:                     %d\n", cdepth->depth);
            visual_iter = xcb_render_pictdepth_visuals_iterator(cdepth);

            num_visuals = cdepth->num_visuals;
            while(visual_iter.rem)
            {
                xcb_render_pictvisual_t *cvisual = visual_iter.data;
                
                fprintf(stdout, "        Visual #%d\n", 1 + num_visuals - visual_iter.rem);
                fprintf(stdout, "            VISUALID:      %d\n", cvisual->visual);
                fprintf(stdout, "            PICTFORMAT:    %d\n", cvisual->format);
                xcb_render_pictvisual_next(&visual_iter);
            }
            xcb_render_pictdepth_next(&depth_iter);
        }
        xcb_render_pictscreen_next(&screen_iter);
    }
    return 0;
}

int draw_window(xcb_connection_t *conn, xcb_render_query_pict_formats_reply_t *reply)
{
    xcb_window_t          window;
    xcb_drawable_t        window_drawable, tmp, root_drawable;
    xcb_pixmap_t          surfaces[4], alpha_surface;
    xcb_render_pictformat_t      alpha_mask_format, window_format, surface_format, no_format = {0};
    xcb_render_picture_t         window_pict, pict_surfaces[4], alpha_pict, 
                        no_picture = {0}, root_picture;
    xcb_render_pictforminfo_t    *forminfo_ptr, *alpha_forminfo_ptr, query;
    uint32_t          value_mask, value_list[4];
    xcb_rectangle_t       pict_rect[1], window_rect;
    xcb_render_color_t           pict_color[4], back_color, alpha_color;
    xcb_screen_t          *root;
    xcb_render_trapezoid_t       traps[4];
    xcb_render_triangle_t        triangles[4];
    xcb_render_pointfix_t        tristrips[9];
    xcb_render_pointfix_t        trifans[9];
    int index;

    root = xcb_setup_roots_iterator(xcb_get_setup(c)).data;
    root_drawable = root->root;
   
    /* Setting query so that it will search for an 8 bit alpha surface. */
    query.id = 0;
    query.type = XCB_RENDER_PICT_TYPE_DIRECT;
    query.depth = 8;
    query.direct.red_mask = 0;
    query.direct.green_mask = 0;
    query.direct.blue_mask = 0;
    query.direct.alpha_mask = 255;

    /* Get the xcb_render_pictformat_t associated with the window. */
    window_format = get_pictformat_from_visual(reply, root->root_visual);

    /* Get the xcb_render_pictformat_t we will use for the alpha mask */
    alpha_forminfo_ptr = get_pictforminfo(reply, &query);
    alpha_mask_format = alpha_forminfo_ptr->id;
    
    /* resetting certain parts of query to search for the surface format */
    query.depth = 32;
    query.direct.alpha_mask = 0;
  
    /* Get the surface forminfo and xcb_render_pictformat_t */
    forminfo_ptr = get_pictforminfo(reply, &query);
    surface_format = forminfo_ptr->id;
    
    /* assign XIDs to all of the drawables and pictures */
    for(index = 0; index < 4; index++)
    {
        surfaces[index] = xcb_generate_id(conn);
        pict_surfaces[index] = xcb_generate_id(conn);
    }
    alpha_surface = xcb_generate_id(conn);
    alpha_pict = xcb_generate_id(conn);
    window = xcb_generate_id(conn);
    window_pict = xcb_generate_id(conn);
    window_drawable = window;
    root_picture = xcb_generate_id(conn);
    
    /* Here we will create the pixmaps that we will use */
    for(index = 0; index < 4; index++)
    {
        surfaces[index] = xcb_generate_id(conn);
        xcb_create_pixmap(conn, 32, surfaces[index], root_drawable, 600, 600);
    }
    alpha_surface = xcb_generate_id(conn);
    xcb_create_pixmap(conn, 8, alpha_surface, root_drawable, 600, 600);
    
    /* initialize the value list */
    value_mask = XCB_CW_EVENT_MASK;
    value_list[0] = XCB_EXPOSE;
    
    /* Create the window */
    xcb_create_window(conn, /* xcb_connection_t */
            0,  /* depth, 0 means it will copy it from the parent */
            window, root_drawable, /* window and parent */
            0, 0,   /* x and y */
            600, 600,   /* width and height */
            0,  /* border width */
            XCB_WINDOW_CLASS_INPUT_OUTPUT,    /* class */
            root->root_visual,   /* xcb_visualid_t */
            value_mask, value_list); /* LISTofVALUES */
    
    /* 
     * Create the pictures 
     */
    value_mask = 1<<0; /* repeat (still needs to be added to xcb_render.m4) */
    value_list[0] = 1;

    xcb_render_create_picture(conn, root_picture, root_drawable, window_format,
            value_mask, value_list);
    xcb_render_create_picture(conn, window_pict, window_drawable, window_format,
            value_mask, value_list);
    tmp = alpha_surface;
    xcb_render_create_picture(conn, alpha_pict, tmp, alpha_mask_format,
            value_mask, value_list);
    for(index = 0; index < 4; index++)
    {
        tmp = surfaces[index];
        xcb_render_create_picture(conn, pict_surfaces[index], tmp, surface_format,
                value_mask, value_list);
    }

    /* 
     * initialize the rectangles
     */
    window_rect.x = 0;
    window_rect.y = 0;
    window_rect.width = 600;
    window_rect.height = 600;

    pict_rect[0].x = 0;
    pict_rect[0].y = 0;
    pict_rect[0].width = 600;
    pict_rect[0].height = 600;
   
    /* 
     * initialize the colors
     */
    back_color.red = 0xffff;
    back_color.green = 0xffff;
    back_color.blue = 0xffff;
    back_color.alpha = 0xffff;
   
    pict_color[0].red = 0x5fff;
    pict_color[0].green = 0x0000;
    pict_color[0].blue = 0x0000;
    pict_color[0].alpha = 0x5fff;
    
    pict_color[1].red = 0x0000;
    pict_color[1].green = 0x5fff;
    pict_color[1].blue = 0x0000;
    pict_color[1].alpha = 0x5fff;

    pict_color[2].red = 0x0000;
    pict_color[2].green = 0x0000;
    pict_color[2].blue = 0x5fff;
    pict_color[2].alpha = 0x5fff;

    pict_color[3].red = 0x0000;
    pict_color[3].green = 0x0000;
    pict_color[3].blue = 0x5fff;
    pict_color[3].alpha = 0x5fff;

    alpha_color.red = 0x0000;
    alpha_color.green = 0x0000;
    alpha_color.blue = 0x0000;
    alpha_color.alpha = 0xffff;

    /* Create the trapeziod dimensions */
    traps[0].top = make_fixed(300, 32000);
    traps[0].bottom = make_fixed(416, 0);
    traps[0].left.p1.y = make_fixed(250, 0);
    traps[0].left.p1.x = make_fixed(300, 0);
    traps[0].left.p2.y = make_fixed(500, 0);
    traps[0].left.p2.x = make_fixed(100, 0);
    traps[0].right.p1.y = make_fixed(250, 0);
    traps[0].right.p1.x = make_fixed(300, 0);
    traps[0].right.p2.y = make_fixed(505, 6000);
    traps[0].right.p2.x = make_fixed(456, 512);

    /* Create the triangle dimensions */
    triangles[0].p1.x = make_fixed(100, 40000);
    triangles[0].p1.y = make_fixed(100, 0);
    triangles[0].p2.x = make_fixed(400, 0);
    triangles[0].p2.y = make_fixed(150, 30000);
    triangles[0].p3.x = make_fixed(30, 0);
    triangles[0].p3.y = make_fixed(456, 0);

    /* Create the tristrip dimensions */
    tristrips[0].x = make_fixed(400, 0);
    tristrips[0].y = make_fixed(50, 0);
    tristrips[1].x = make_fixed(436, 0);
    tristrips[1].y = make_fixed(50, 0);
    tristrips[2].x = make_fixed(398, 0);
    tristrips[2].y = make_fixed(127, 0);
    tristrips[3].x = make_fixed(450, 0);
    tristrips[3].y = make_fixed(120, 0);
    tristrips[4].x = make_fixed(450, 0);
    tristrips[4].y = make_fixed(180, 0);
    tristrips[5].x = make_fixed(503, 0);
    tristrips[5].y = make_fixed(124, 0);
    tristrips[6].x = make_fixed(500, 0);
    tristrips[6].y = make_fixed(217, 0);
    tristrips[7].x = make_fixed(542, 0);
    tristrips[7].y = make_fixed(237, 0);
    tristrips[8].x = make_fixed(501, 0);
    tristrips[8].y = make_fixed(250, 0);

    /* Create the trifan dimensions */
    trifans[0].x = make_fixed(424, 0);
    trifans[0].y = make_fixed(415, 0);
    trifans[1].x = make_fixed(375, 0);
    trifans[1].y = make_fixed(355, 0);
    trifans[2].x = make_fixed(403, 0);
    trifans[2].y = make_fixed(350, 0);
    trifans[3].x = make_fixed(430, 0);
    trifans[3].y = make_fixed(380, 0);
    trifans[4].x = make_fixed(481, 0);
    trifans[4].y = make_fixed(400, 0);
    trifans[5].x = make_fixed(475, 0);
    trifans[5].y = make_fixed(437, 0);
    trifans[6].x = make_fixed(430, 0);
    trifans[6].y = make_fixed(444, 0);
    trifans[7].x = make_fixed(400, 0);
    trifans[7].y = make_fixed(430, 0);

    /* 
     * Map the window
     */
    xcb_map_window(conn, window);
    
    /*
     * Play around with Render
     */

    xcb_render_fill_rectangles(conn, XCB_RENDER_PICT_OP_SRC, alpha_pict, alpha_color, 1, pict_rect);
    xcb_render_fill_rectangles(conn, XCB_RENDER_PICT_OP_SRC, pict_surfaces[0], pict_color[0], 1, pict_rect);
    xcb_render_fill_rectangles(conn, XCB_RENDER_PICT_OP_SRC, pict_surfaces[1], pict_color[1], 1, pict_rect);
    xcb_render_fill_rectangles(conn, XCB_RENDER_PICT_OP_SRC, pict_surfaces[2], pict_color[2], 1, pict_rect);
    xcb_render_fill_rectangles(conn, XCB_RENDER_PICT_OP_SRC, pict_surfaces[3], pict_color[3], 1, pict_rect);

    xcb_flush(conn);
    sleep(1);

    xcb_render_fill_rectangles(conn, XCB_RENDER_PICT_OP_OVER, window_pict, back_color, 1, &window_rect);

    xcb_flush(conn);
    sleep(1);


    /* Composite the first pict_surface onto the window picture */
    xcb_render_composite(conn, XCB_RENDER_PICT_OP_OVER, pict_surfaces[0], no_picture /* alpha_pict */, window_pict,
            0, 0, 0, 0, 200, 200,
            400, 400);
    xcb_flush(conn);
    sleep(1);
/*
    xcb_render_composite(conn, XCB_RENDER_PICT_OP_OVER, pict_surfaces[0], alpha_pict, window_pict,
            0, 0, 0, 0, 0, 0,
            200, 200);
    xcb_flush(conn);
    sleep(1);
*/
    xcb_render_composite(conn, XCB_RENDER_PICT_OP_OVER, pict_surfaces[1], no_picture /* alpha_pict */, window_pict,
            0, 0, 0, 0, 0, 0,
            400, 400);
    xcb_flush(conn);
    sleep(1);
    
    xcb_render_composite(conn, XCB_RENDER_PICT_OP_OVER, pict_surfaces[2], no_picture /* alpha_pict */, window_pict,
            0, 0, 0, 0, 200, 0,
            400, 400);
    xcb_flush(conn);
    sleep(1);
    
    xcb_render_composite(conn, XCB_RENDER_PICT_OP_OVER, pict_surfaces[3],  no_picture /* alpha_pict */, window_pict,
            0, 0, 0, 0, 0, 200,
            400, 400);
    xcb_flush(conn);
    sleep(1);

    xcb_render_trapezoids(conn, XCB_RENDER_PICT_OP_OVER, pict_surfaces[0], window_pict, alpha_mask_format, 0, 0, 1, &traps[0]);
    xcb_flush(conn);
    sleep(1);

    xcb_render_triangles(conn, XCB_RENDER_PICT_OP_OVER, pict_surfaces[1], window_pict, no_format, 0, 0, 1, &triangles[0]);
    xcb_flush(conn);
    sleep(1);
    
    xcb_render_tri_strip(conn, XCB_RENDER_PICT_OP_OVER, pict_surfaces[2], window_pict, no_format, 0, 0, 9, &tristrips[0]);
    xcb_flush(conn);
    sleep(1);
    
    xcb_render_tri_fan(conn, XCB_RENDER_PICT_OP_OVER, pict_surfaces[3], window_pict, no_format, 0, 0, 8, &trifans[0]);
    xcb_flush(conn);
    sleep(2);
    
    
    /* Free up all of the resources we used */
    for(index = 0; index < 4; index++)
    {
        xcb_free_pixmap(conn, surfaces[index]);
        xcb_render_free_picture(conn, pict_surfaces[index]);
    }
    xcb_free_pixmap(conn, alpha_surface);
    xcb_render_free_picture(conn, alpha_pict);
    xcb_render_free_picture(conn, window_pict);
    xcb_render_free_picture(conn, root_picture);
   
    /* sync up and leave the function */
    xcb_flush(conn);
    return 0;
}


/**********************************************************
 * This function searches through the reply for a 
 * PictVisual who's xcb_visualid_t is the same as the one
 * specified in query. The function will then return the
 * xcb_render_pictformat_t from that PictVIsual structure. 
 * This is useful for getting the xcb_render_pictformat_t that is
 * the same visual type as the root window.
 **********************************************************/
xcb_render_pictformat_t get_pictformat_from_visual(xcb_render_query_pict_formats_reply_t *reply, xcb_visualid_t query)
{
    xcb_render_pictscreen_iterator_t screen_iter;
    xcb_render_pictscreen_t    *cscreen;
    xcb_render_pictdepth_iterator_t  depth_iter;
    xcb_render_pictdepth_t     *cdepth;
    xcb_render_pictvisual_iterator_t visual_iter; 
    xcb_render_pictvisual_t    *cvisual;
    xcb_render_pictformat_t  return_value;
    
    screen_iter = xcb_render_query_pict_formats_screens_iterator(reply);

    while(screen_iter.rem)
    {
        cscreen = screen_iter.data;
        
        depth_iter = xcb_render_pictscreen_depths_iterator(cscreen);
        while(depth_iter.rem)
        {
            cdepth = depth_iter.data;

            visual_iter = xcb_render_pictdepth_visuals_iterator(cdepth);
            while(visual_iter.rem)
            {
                cvisual = visual_iter.data;

                if(cvisual->visual == query)
                {
                    return cvisual->format;
                }
                xcb_render_pictvisual_next(&visual_iter);
            }
            xcb_render_pictdepth_next(&depth_iter);
        }
        xcb_render_pictscreen_next(&screen_iter);
    }
    return_value = 0;
    return return_value;
}

xcb_render_pictforminfo_t *get_pictforminfo(xcb_render_query_pict_formats_reply_t *reply, xcb_render_pictforminfo_t *query)
{
    xcb_render_pictforminfo_iterator_t forminfo_iter;
    
    forminfo_iter = xcb_render_query_pict_formats_formats_iterator(reply);

    while(forminfo_iter.rem)
    {
        xcb_render_pictforminfo_t *cformat;
        cformat  = forminfo_iter.data;
        xcb_render_pictforminfo_next(&forminfo_iter);

        if( (query->id != 0) && (query->id != cformat->id) )
        {
            continue;
        }

        if(query->type != cformat->type)
        {
            continue;
        }
        
        if( (query->depth != 0) && (query->depth != cformat->depth) )
        {
            continue;
        }
        
        if( (query->direct.red_mask  != 0)&& (query->direct.red_mask != cformat->direct.red_mask))
        {
            continue;
        }
        
        if( (query->direct.green_mask != 0) && (query->direct.green_mask != cformat->direct.green_mask))
        {
            continue;
        }
        
        if( (query->direct.blue_mask != 0) && (query->direct.blue_mask != cformat->direct.blue_mask))
        {
            continue;
        }
        
        if( (query->direct.alpha_mask != 0) && (query->direct.alpha_mask != cformat->direct.alpha_mask))
        {
            continue;
        }
        
        /* This point will only be reached if the pict format   *
         * matches what the user specified                      */
        return cformat; 
    }
    
    return NULL;
}

int main(int argc, char *argv[])
{
    xcb_render_query_version_cookie_t version_cookie;
    xcb_render_query_version_reply_t    *version_reply;
    xcb_render_query_pict_formats_cookie_t formats_cookie;
    xcb_render_query_pict_formats_reply_t *formats_reply;
    xcb_render_pictformat_t  rootformat;
    xcb_screen_t *root;
    int screen_num;
    
    xcb_render_pictforminfo_t  forminfo_query, *forminfo_result;
    
    c = xcb_connect(0, &screen_num);
    root = xcb_aux_get_screen(c, screen_num);
    
    version_cookie = xcb_render_query_version(c, (uint32_t)0, (uint32_t)3);
    version_reply = xcb_render_query_version_reply(c, version_cookie, 0);

    print_version_info(version_reply);
    
    formats_cookie = xcb_render_query_pict_formats(c);
    formats_reply = xcb_render_query_pict_formats_reply(c, formats_cookie, 0);

    draw_window(c, formats_reply);
    
    print_formats_info(formats_reply);
   
    forminfo_query.id = 0;
    forminfo_query.type = XCB_RENDER_PICT_TYPE_DIRECT;
    forminfo_query.depth = 8;
    forminfo_query.direct.red_mask = 0;
    forminfo_query.direct.green_mask = 0;
    forminfo_query.direct.blue_mask = 0;
    forminfo_query.direct.alpha_mask = 255;
    
    forminfo_result = get_pictforminfo(formats_reply, &forminfo_query);
    fprintf(stdout, "\n***** found PICTFORMAT:  %d *****\n",
            forminfo_result->id);
    rootformat = get_pictformat_from_visual(formats_reply, root->root_visual);
    fprintf(stdout, "\n***** found root PICTFORMAT:   %d *****\n", rootformat);
   
#if 0
    draw_window(c, formats_reply);
#endif
    
    /* It's very important to free the replys. We don't want memory leaks. */
    free(version_reply);
    free(formats_reply);

    xcb_disconnect(c);

    exit(0);
}
