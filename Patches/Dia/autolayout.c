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

/* 
 * TODO:
 * 1. Use something else other than a GList for the lists.
 * 		It is too slow O(n) for nearly _everything_.
 * 2. Integrate the "graph" properties into DiaObject so we don't 
 * 		have to rebuild a graph from the diagram everytime we call 
 * 		layout_force().
 * 3. Currently the algorithm is O(n^2), we can use the Barnes-Hut 
 * 		method to have O(n log n).
 * 4. Get mass normalization into the diagram code so we avoid an 
 * 		O(n) everytime we call layout_force().
 */

#include "autolayout.h"
#include "geometry.h"

#include <math.h>

/* 
 * 1. create a graph object made up of the currently selected objects
 * 2. layout the graph using a force-directed method
 * 3. free the graph (without freeing the objects!)
 * 
 * returns a boolean of whether to stop re-running (TRUE) or to continue 
 * running (FALSE). see app/commands.c objects_layout_callback().
 */
gboolean layout_force (GList *selected, gdouble repulsion_const,
						gdouble attraction_const, gdouble timestep,
						gdouble damping, Rectangle wall, gboolean use_wall)
{
	Graph	*diagram = create_graph_from_list(selected);
	GList	*cur_node = diagram->nodes,
			*oth_node = NULL,
			*cur_edge = NULL;
	Point	p;
			
	normalize_masses(diagram->nodes);
	
	while (cur_node)
	{
		((Node *)cur_node->data)->netforce.x = 0.0;
		((Node *)cur_node->data)->netforce.y = 0.0;
		
		/* repulsion */
		oth_node = diagram->nodes;
		while (oth_node)
		{
			if (((Node *)cur_node->data)->object != 
				((Node *)oth_node->data)->object)
			{
				p = repulsion(repulsion_const, 
							(Node *)cur_node->data, 
							(Node *)oth_node->data);
				((Node *)cur_node->data)->netforce.x = 
					((Node *)cur_node->data)->netforce.x + p.x;
				((Node *)cur_node->data)->netforce.y = 
					((Node *)cur_node->data)->netforce.y + p.y;
			}
			
			oth_node = oth_node->next;
		}
		
		/* attraction */
		cur_edge = diagram->edges;
		while (cur_edge)
		{
			if (((Node *)cur_node->data)->object == 
				((Edge *)cur_edge->data)->pointA ||
				((Node *)cur_node->data)->object == 
				((Edge *)cur_edge->data)->pointB)
			{
				p = attraction(attraction_const, 
								(Node *)cur_node->data, 
								(Edge *)cur_edge->data);
				((Node *)cur_node->data)->netforce.x = 
					((Node *)cur_node->data)->netforce.x + p.x;
				((Node *)cur_node->data)->netforce.y = 
					((Node *)cur_node->data)->netforce.y + p.y;
			}
			cur_edge = cur_edge->next;
		}
		
		/* node velocity */
		((Node *)cur_node->data)->velocity.x = 
			((Node *)cur_node->data)->velocity.x +
			(timestep * ((Node *)cur_node->data)->netforce.x) * damping;
		((Node *)cur_node->data)->velocity.y = 
			((Node *)cur_node->data)->velocity.y +
			(timestep * ((Node *)cur_node->data)->netforce.y) * damping;
		
		DiaObject *cur_node_dia_object = ((Node *)cur_node->data)->object;
		Point *new_pos = g_malloc(sizeof(Point));
		/* node object position */
		new_pos->x = cur_node_dia_object->position.x + (timestep * ((Node *)cur_node->data)->velocity.x);
		new_pos->y = cur_node_dia_object->position.y + (timestep * ((Node *)cur_node->data)->velocity.y);
		cur_node_dia_object->ops->move(cur_node_dia_object, new_pos);
			
/*		cur_node_dia_object->position.x = cur_node_dia_object->position.x +
			(timestep * ((Node *)cur_node->data)->velocity.x);
		cur_node_dia_object->position.y = cur_node_dia_object->position.y +
			(timestep * ((Node *)cur_node->data)->velocity.y);
*/					
		cur_node = cur_node->next;
	}
	
	graph_free(diagram);
	return 0;
}

/* hooke attraction */
Point attraction(double constant, Node *n, Edge *e)
{
	Point	p;
	Node	*a = g_malloc(sizeof(Node)),
			*b = g_malloc(sizeof(Node));
	
	if (e->pointA == n->object)
	{
		a->object = e->pointA;
		b->object = e->pointB;
	}
	else
	{
		a->object = e->pointB;
		b->object = e->pointA;
	}
	
	p.x = -constant * (a->object->position.x - b->object->position.x);
	p.y = -constant * (a->object->position.y - b->object->position.y);
	
	g_free(a);
	g_free(b);
	
	return p;
}

/* coulomb repulsion */
Point repulsion(double constant, Node *n1, Node *n2)
{
	Point	p;
/*	double	masses = n1->mass * n2->mass; */
	double	masses = n1->size * n2->size;
	double	dx = n1->object->position.x - n2->object->position.x;
	double	dy = n1->object->position.y - n2->object->position.y;
	double	denom = pow(dx * dx + dy * dy, 3 / 2);
	double	numer = masses * constant / 100.0;
	double	forcex = (numer * dx) / denom;
	double	forcey = (numer * dy) / denom;
	
	p.x = forcex;
	p.y = forcey;
	
	return p;
}

/* 
 * here we normalize the masses of all nodes so we don't have to use 
 * the electrostatic constant when calculating the repulsions.
 * 
 * 1. we get the smallest size
 * 2. divide all the sizes by the smallest size
 */
void normalize_masses(GList *nodes)
{
	if (!nodes) return;
	
	GList	*n = NULL;
	double	min = ((Node *)nodes->data)->size;
	
	n = nodes;
	while (n)
	{
		if (min > ((Node *)n->data)->size)
			min = ((Node *)n->data)->size;
		
		n = n->next;
	}
	
	n = nodes;
	while (n)
	{
		((Node *)n->data)->mass = ((Node *)n->data)->size / min;
		n = n->next;
	}
}

/* 
 * 1. iterate over the selected objects
 * 2. create two lists, one for "connections/lines" (edges)
 * 		and another for "normal" objects (nodes)
 * 3. return the graph made up of these object lists
 */
Graph *create_graph_from_list(GList *list)
{
	Graph		*tmp;
	Edge		*etmp = NULL;
	Node		*ntmp = NULL;
	Rectangle	r;
	
	tmp = g_malloc(sizeof(Graph));
	tmp->edges = NULL;
	tmp->nodes = NULL;
	
	while (list)
	{
		if (graph_type_from_object(list->data) == GRAPH_OBJECT_TYPE_EDGE)
		{
			/* 
			 * basically, if one of the handles in the "connection" 
			 * isn't connected to anything, then using it for layout 
			 * is pointless. so here we only add fully connected 
			 * "connections" to our graph.
			 */
			if (((DiaObject *)list->data)->handles[0]->connected_to && 
				((DiaObject *)list->data)->handles[1]->connected_to)
			{
				etmp = g_malloc(sizeof(Edge));
				etmp->object = list->data;
				etmp->pointA = etmp->object->handles[0]->connected_to->object;
				etmp->pointB = etmp->object->handles[1]->connected_to->object;
				tmp->edges = g_list_append(tmp->edges, etmp);
			}
		}
		else
		{
			ntmp = g_malloc(sizeof(Node));
			ntmp->object = list->data;
			ntmp->netforce.x = 0.0;
			ntmp->netforce.y = 0.0;
			ntmp->velocity.x = 0.0;
			ntmp->velocity.y = 0.0;
			r = ((DiaObject *)list->data)->bounding_box;
			ntmp->size = (r.right - r.left) * (r.bottom - r.top);
			ntmp->mass = 0.0;
			tmp->nodes = g_list_append(tmp->nodes, ntmp);
		}
			
		list = g_list_next(list);
	}
	
	return tmp;
}

/* check if an object can be an edge (connection) and return so */
GraphObjectType graph_type_from_object(DiaObject *object)
{
	if (g_str_equal(object->type->name, "Standard - Line") ||
			g_str_equal(object->type->name, "Standard - Arc") ||
			g_str_equal(object->type->name, "Standard - ZigZagLine") ||
			g_str_equal(object->type->name, "Standard - PolyLine") ||
			g_str_equal(object->type->name, "Standard - BezierLine"))
		return GRAPH_OBJECT_TYPE_EDGE;
	else
		return GRAPH_OBJECT_TYPE_NODE;
}

/* free the nodes and edges lists _without_ freeing the dia objects */
void graph_free(Graph *graph)
{
	while (graph->nodes)
	{
		g_free(graph->nodes->data);
		graph->nodes = g_list_next(graph->nodes);
	}
	
	while (graph->edges)
	{
		g_free(graph->edges->data);
		graph->edges = g_list_next(graph->edges);
	}
	
	g_list_free(graph->nodes);
	g_list_free(graph->edges);
	g_free(graph);
}
