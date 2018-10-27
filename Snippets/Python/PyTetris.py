import pygame
pygame.init()

AppName = "PyTetris"
AppWindowSize = (500, 600)

PiecePadding = 2
GridBlockWidth = 10
GridBlockHeight = 20
GridBlockSize = 30
GridPixelWidth = GridBlockWidth * GridBlockSize + 1
GridPixelHeight = GridBlockHeight * GridBlockSize + 1

WhiteColor = pygame.Color("white")
RedColor = pygame.Color("red")
GreyColor = pygame.Color("grey")

class Piece(object):
	def __init__(self):
		object.__init__(self)
		self.blockArray = [	[1, 0],
							[1, 0],
							[1, 1]
						  ]
		self.widthInBlocks, self.heightInBlocks = len(self.blockArray[0]), \
												  len(self.blockArray)
		self.width, self.height = GridBlockSize * self.widthInBlocks, \
								  GridBlockSize * self.heightInBlocks
		self.surface = pygame.Surface((self.width, self.height))
		self.draw()
		
	def draw(self):
		self.surface.set_colorkey(WhiteColor)
		self.surface.fill(WhiteColor)
		
		for i in range(self.widthInBlocks):
			for j in range(self.heightInBlocks):
				if self.blockArray[j][i] == 1:
					self.surface.fill(RedColor,
						(i * GridBlockSize + PiecePadding,
						 j * GridBlockSize + PiecePadding, 
						GridBlockSize - PiecePadding - 1,
						GridBlockSize - PiecePadding - 1))
		
	def render(self, parentSurface):
		parentSurface.blit(self.surface, self.surface.get_rect())

class App(object):
	def __init__(self):
		object.__init__(self)
		
		pygame.display.set_caption(AppName)
		self.mainWindow = pygame.display.set_mode(AppWindowSize)
		self.grid = Grid()
		self.currentPiece = Piece()
		
	def render_event(self):
		self.mainWindow.fill(WhiteColor)
		self.grid.render(self.mainWindow)
		self.currentPiece.render(self.mainWindow)
		pygame.display.flip()
		
class Grid(object):
	def __init__(self):
		object.__init__(self)
		self.grid = pygame.Surface((GridPixelWidth, GridPixelHeight))
		self.draw()
		
	def draw(self):
		self.grid.fill(WhiteColor)
		
		i = 0
		while i <= GridPixelWidth:
			pygame.draw.line(self.grid, GreyColor, (i, 0), (i, GridPixelHeight))
			i += GridBlockSize
		i = 0
		while i <= GridPixelHeight:
			pygame.draw.line(self.grid, GreyColor, (0, i), (GridPixelWidth, i))
			i += GridBlockSize
		
	def render(self, parentSurface):
		parentSurface.blit(self.grid, self.grid.get_rect())

if __name__ == "__main__":
	quit = False
	app = App()
	clock = pygame.time.Clock()
	
	while not quit:
		for event in pygame.event.get():
			if event.type == pygame.QUIT:
				quit = True
				break
		
		if quit == True:
			break
		
		app.render_event()
		clock.tick(10)
	pygame.quit()
