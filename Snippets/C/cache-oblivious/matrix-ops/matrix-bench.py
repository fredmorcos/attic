#!/usr/bin/python2

class Matrix(object):
    def __init__(self, size = 10000, zeros = False):
        self.items = []

        if zeros == True:
            self.reset()

    def fill(self):
        i = 0
        while i < len(self.items):
            self.items[i] = i
            i += 1

    def reset(self):
        i = 0
        while i < len(self.items):
            self.items[i] = 0
            i += 1

    def __str__(self):
        pass

def mat_new_with_size(size = 10000):
    m = []
    v = 0
    
    for i in xrange(size):
        m += [[]]
        
        for j in xrange(size):
            m[i] += [v]
            v += 1

    return m

def mat_reset(m):
    for i in m:
        for j in i:
            j = 0

def mat_print(m):
    for i in m:
        for j in i:
            print j,
        print

# def mat_

if __name__ == '__main__':
    m1 = mat_new_with_size(4)
    mat_reset(m1)
    mat_print(m1)
