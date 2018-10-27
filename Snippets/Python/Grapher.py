import pygame
import sys
import random
import math

pygame.init()
pygame.key.set_repeat(1, 1)
window = pygame.display.set_mode((600, 600))

NodeRad = 5
NodeDiam = NodeRad * 2
EConst = 200
SConst = 0.1
SLen = 5
Damp = 0.5
TimeStep = 0.8

class Graph(object):
	def __init__(self, surface):
		self.surface = surface
		self.nodes = []
		self.edges = []

		width, height = self.surface.get_width(), self.surface.get_height()

		for i in range(20):
			self.nodes.append(Node(self.surface,
				[int(random.uniform(NodeRad, width - NodeRad)),
				 int(random.uniform(NodeRad, height - NodeRad))
				]))
		self.nodes[0].pos = [self.surface.get_width() / 2,
							 self.surface.get_height() / 2]
		for n in self.nodes[1:]:
			self.edges.append(Edge(self.surface, self.nodes[0], n))

	def render(self):
		for n in self.nodes:
			n.render()

		for e in self.edges:
			e.render()

	def layout(self):
		for n in self.nodes[1:]:
			for n2 in self.nodes:
				if n != n2:
					dis = (n.pos[0] - n2.pos[0], n.pos[1] - n2.pos[1])
					numer = (NodeDiam ** 2) * EConst
					denom = ((dis[0] ** 2) + (dis[1] ** 2)) ** 1.5
					n.force[0] += numer * dis[0] / denom
					n.force[1] += numer * dis[1] / denom

			for e in self.edges:
				if n == e.src or n == e.dst:
					if n == e.src:
						a = e.src
						b = e.dst
					else:
						a = e.dst
						b = e.src

					deltadis = (a.pos[0] - b.pos[0], a.pos[1] - b.pos[1])
					distance = math.sqrt(deltadis[0] ** 2 + deltadis[1] ** 2)
					factor = -SConst * (distance - SLen) / distance

					a.force[0] += deltadis[0] * factor
					a.force[1] += deltadis[1] * factor

			n.velocity[0] = (n.velocity[0] + TimeStep * n.force[0]) * Damp
			n.velocity[1] = (n.velocity[1] + TimeStep * n.force[1]) * Damp
			n.pos[0] += int(n.velocity[0] * TimeStep)
			n.pos[1] += int(n.velocity[1] * TimeStep)
			n.force = [0, 0]

class Edge(object):
	def __init__(self, surface, src = None, dst = None):
		self.surface = surface
		self.src = src
		self.dst = dst

	def render(self):
		pygame.draw.line(self.surface, (0, 0, 0), self.src.pos, self.dst.pos)

class Node(object):
	def __init__(self, surface, pos = [0, 0]):
		self.surface = surface
		self.pos = pos
		self.force = [0, 0]
		self.velocity = [0, 0]

	def render(self):
		pygame.draw.circle(self.surface, (0, 0, 0), self.pos, NodeRad)

if __name__ == "__main__":
	clock = pygame.time.Clock()
	graph = Graph(window)
	while True:
		for e in pygame.event.get():
			if e.type == pygame.QUIT:
				sys.exit()
			if e.type == pygame.KEYDOWN:
				graph.layout()
		window.fill((255, 255, 255))
		graph.render()
		pygame.display.flip()
		clock.tick(25)
