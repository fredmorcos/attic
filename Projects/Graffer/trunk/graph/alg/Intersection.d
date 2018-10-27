/*
	This file is part of Grafer.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	Grafer is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Grafer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Grafer.  If not, see <http://www.gnu.org/licenses/>.
*/

module graph.alg.Intersection;

private import
	graph.adt.Graph,
	graph.adt.Edge;

public void highlightIntersections (Graph* g) {
	foreach (i, ref e; g.edges) {
		foreach (ref f; g.edges [i + 1 .. $]) {
			if (intersect (e, f)) {
				e.selected = true;
				f.selected = true;
			}
			else {
				e.selected = false;
				f.selected = false;
			}
		}
	}
}

private bool intersect (Edge* e, Edge* f) {
	double	xi = - (e.constant - f.constant) / (e.slope - f.slope),
			yi = e.constant + (e.slope * xi);
	
	/* Does intersection point lie on both lines? */
	if (e.lies (xi, yi) && f.lies (xi, yi))
		return true;
	else
		return false;
}

