#!/usr/bin/env python2

'''
Public Domain
Fred Morcos <fred.morcos@gmail.com>
'''

import pygame
import sys

BlockSize = 20
WinSize = 600
BoardSize = WinSize / BlockSize
Black = pygame.color.Color("black")
Grey = pygame.color.Color("grey")

pygame.init()
pygame.display.set_caption("Game of Life - Click: draw/erase - S to start")
win = pygame.display.set_mode((WinSize, WinSize))


class Board(object):
    def __init__(self):
        self.blocks = []

        for i in range(BoardSize):
            list = []
            for j in range(BoardSize):
                list.append(0)
            self.blocks.append(list)

    def render(self, surface):
        i = 0
        j = 0

        while i <= WinSize:
            pygame.draw.line(surface, Grey, (i, 0), (i, WinSize))
            i += BlockSize

        while j <= WinSize:
            pygame.draw.line(surface, Grey, (0, j), (WinSize, j))
            j += BlockSize

        for i in range(BoardSize):
            for j in range(BoardSize):
                if self.blocks[i][j] == 1:
                    pygame.draw.rect(surface, Black,
                                     (i * BlockSize, j * BlockSize,
                                      BlockSize, BlockSize))

    def numOfNeighbors(self, x, y):
        i = x - 1
        j = y - 1
        res = 0

        while i <= x + 1:
            j = y - 1
            while j <= y + 1:
                if not (i == x and j == y):
                    m = i
                    n = j
                    if i >= BoardSize:
                        m = i - BoardSize
                    if j >= BoardSize:
                        n = j - BoardSize

                    if self.blocks[m][n] == 1:
                        res += 1
                j += 1
            i += 1

        return res

    def runOnce(self):
        res = Board()

        for i in range(BoardSize):
            for j in range(BoardSize):
                n = self.numOfNeighbors(i, j)

                if self.blocks[i][j] == 1 and n < 2:
                    res.blocks[i][j] = 0
                if self.blocks[i][j] == 1 and n > 3:
                    res.blocks[i][j] = 0
                if self.blocks[i][j] == 1 and (n == 2 or n == 3):
                    res.blocks[i][j] = 1
                if self.blocks[i][j] == 0 and n == 3:
                    res.blocks[i][j] = 1

        return res


if __name__ == "__main__":
    clock = pygame.time.Clock()
    board = Board()
    run = False

    while True:
        for e in pygame.event.get():
            if e.type == pygame.QUIT:
                sys.exit()
            if e.type == pygame.MOUSEBUTTONDOWN:
                i = e.pos[0] / BlockSize
                j = e.pos[1] / BlockSize

                if board.blocks[i][j] == 1:
                    board.blocks[i][j] = 0
                else:
                    board.blocks[i][j] = 1
            if e.type == pygame.KEYDOWN and e.key == pygame.K_s:
                run = True

        if run:
            board = board.runOnce()

        win.fill((255, 255, 255))
        board.render(win)
        pygame.display.flip()
        clock.tick(2)
