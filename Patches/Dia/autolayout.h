/* Dia -- an diagram creation/manipulation program -*- c -*-
 * 
 * This is where automatic layouting happens.
 * 
 * Copyright (C) 2008 Frederic-Gerald Morcos
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef AUTOLAYOUT_H
#define AUTOLAYOUT_H

#include <glib.h>

#include "object.h"
#include "geometry.h"

typedef enum _GraphObjectType {
	GRAPH_OBJECT_TYPE_EDGE = 0,
	GRAPH_OBJECT_TYPE_NODE
} GraphObjectType;

typedef struct _Edge {
	DiaObject	*object, 	/* the "connection" object */
				*pointA, 	/* the first connected object */
				*pointB;	/* the second connected object */
} Edge;

typedef struct _Node {
	DiaObject	*object;	/* the shape */
	Point		netforce,	/* force vector affecting the node */
				velocity;	/* speed vector of the node */
	double		size,		/* size of the object, width * height */
				mass;		/* normalized size */
} Node;

typedef struct _Graph {
	GList	*edges,			/* list of "connectors" to layout */
			*nodes;			/* list of shapes to layout */
} Graph;

gboolean layout_force (GList *list, double repulsion_const,
						double attraction_const, double timestep,
						double damping, Rectangle wall, gboolean use_wall);
					
Graph *create_graph_from_list(GList *list);
void graph_free(Graph *graph);
Point attraction(double constant, Node *n, Edge *e);
Point repulsion(double constant, Node *n1, Node *n2);
void normalize_masses(GList *nodes);
GraphObjectType graph_type_from_object(DiaObject *object);

#endif	/* AUTOLAYOUT_H */
