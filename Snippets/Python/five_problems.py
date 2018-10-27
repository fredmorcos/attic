def sum_forloop(l):
    acc = 0
    for i in l:
        acc += i
    return acc


def sum_whileloop(l):
    acc = 0
    idx = 0
    while idx < len(l):
        acc += l[idx]
        idx += 1
    return acc


def sum_recursion(l, acc=0):
    if l == []:
        return acc
    else:
        return sum_recursion(l[1:], acc + l[0])


def alternating_lists(l1, l2, res=[]):
    if l1 == [] or l2 == []:
        res += l1
        res += l2
        return res
    else:
        return alternating_lists(l1[1:], l2[1:], res[:] + [l1[0], l2[0]])


def fib100(i=0, res=[]):
    if i == 0 or i == 1:
        res += [i]
        return fib100(i + 1, res)
    elif i == 100:
        return res
    else:
        res += [res[i - 1] + res[i - 2]]
        return fib100(i + 1, res)

print(sum_forloop([1, 2, 3, 4, 5]))
print(sum_whileloop([1, 2, 3, 4, 5]))
print(sum_recursion([1, 2, 3, 4, 5]))

print(alternating_lists([1, 2, 3], ["a", "b", "c"]))
print(alternating_lists([1, 2, 3], ["a", "b", "c", "d"]))

print(fib100())
