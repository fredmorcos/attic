def generalizedGCD(num, arr):
    def gcd(n1, n2):
        while True:
            if n1 == n2:
                return n1
            if n1 > n2:
                n1 -= n2
                continue
            n2 -= n1

    if num == 0:
        assert not arr
        return 0
    if num == 1:
        assert len(arr) == 1
        return arr[0]
    res = gcd(arr[0], arr[1])
    return generalizedGCD(num - 1, [res] + arr[2:])
