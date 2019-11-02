#!/bin/python3

import os
import sys
import bisect

def runningMedian(a):
    res = []
    l = []
    for ai in a:
        bisect.insort(l, ai)
        if len(res) % 2:
            index1 = (len(l)//2)-1
            index2 = len(l)//2
            res += [(l[index1] + l[index2])/2]
        else:
            res += [l[len(l)//2]]
    return ["{:.1f}".format(num) for num in res]


if __name__ == '__main__':
    fptr = open(os.environ['OUTPUT_PATH'], 'w')

    a_count = int(input())

    a = []

    for _ in range(a_count):
        a_item = int(input())
        a.append(a_item)

    result = runningMedian(a)

    fptr.write('\n'.join(map(str, result)))
    fptr.write('\n')

    fptr.close()
