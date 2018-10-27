def bsearch(item, lst):
    if len(lst) == 1:
        if item == lst[0]:
            return 0
        else:
            return -1
    elif len(lst) == 0:
        return -1
    else:
        piv_idx = len(lst) // 2
        piv = lst[piv_idx]

        if item == piv:
            return piv_idx
        if item < piv:
            res = bsearch(item, lst[:piv_idx])
            if res == -1:
                return -1
            else:
                return res
        else:
            res = bsearch(item, lst[piv_idx + 1:])
            if res == -1:
                return -1
            else:
                return res + 1 + piv_idx
