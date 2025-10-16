def and_list(X):
    if len(X) == 0:
        return 0

    # This is the core of the function. It's a tail-recursive accumulator
    # function which ANDs the current accumulator value with the remainder/tail
    # of the list.
    def helper(X, acc):
        if len(X) == 0:
            # Empty list, we're finished, return the accumulator.
            return acc
        return helper(X[1:], acc & X[0])

    # Call the accumulator function with the starting values.
    return helper(X[1:], X[0])


def xor_list(X):
    if len(X) == 0:
        return 0

    # This is the core of the function. It's a tail-recursive accumulator
    # function which XORs the current accumulator value with the remainder/tail
    # of the list.
    def helper(X, acc):
        if len(X) == 0:
            # Empty list, we're finished, return the accumulator.
            return acc
        return helper(X[1:], acc ^ X[0])

    # Call the accumulator function with the starting values.
    return helper(X[1:], X[0])


# This function makes sublists of a certain length e.g. sublists of length 1 of
# [3,5,1] would be [3],[5],[1] and sublists of length 2 would be [3,5],[5,1].
def make_sublists_of_len(X, sublist_len):
    sublists = []
    # Start at each element and create a sublist of the specified length.
    for i in range(0, len(X)):
        # Don't try to create sublists of lengths that go beyond the list.
        if i + sublist_len > len(X):
            break
        sublists.append(X[i : i + sublist_len])
    return sublists


# This function creates the sublists as specified. It calls
# make_sublists_of_len with each possible length of sublists we could have.
def make_sublists(X):
    sublists = []
    # For each possible length of a sublist...
    for i in range(1, len(X) + 1):
        # Create sublists of that length starting at each element of the list.
        for e in make_sublists_of_len(X, i):
            sublists.append(e)
    return sublists


def f(X):
    if len(X) == 0:
        return 0

    # Create all the sublists...
    sublists = make_sublists(X)

    # ... XOR their values
    xored = list(map(lambda sublist: xor_list(sublist), sublists))

    # and finally AND those values.
    return and_list(xored)


# The inefficient version of f(X) has N² complexity because of make_sublists
# which loops over N (from 1 to N + 1) and calls make_sublists_of_len each
# time, which loops over N (from 0 to N).
#
# I don't consider list.append() to have an effect on complexity here, because
# it be done efficiently in O(1) using a different data structure than a linked
# list (e.g. an amortized array or vector).
#
# I will also ignore the list(map(...)) call because that can also be trivially
# made more efficient and does not dominate the N² call to make_sublists.
def f_eff(X):
    if len(X) == 0:
        return 0

    # Create all the sublists...
    sublists = make_sublists(X)

    # ... XOR their values
    xored = list(map(lambda sublist: xor_list(sublist), sublists))

    # and finally AND those values.
    return and_list(xored)


if __name__ == '__main__':
    # should output 1
    print("and_list([3,5,1]) == %s" % and_list([3,5,1]))

    # should output 3
    print("and_list([3]) == %s" % and_list([3]))

    # should output 7
    print("xor_list([3,5,1]) == %s" % xor_list([3,5,1]))

    # should output 3
    print("xor_list([3]) == %s" % xor_list([3]))
