def cellCompete(states, days):
    def next_state(left, right):
        if left == right \
           or (left is None and right == 0) \
           or (left == 0 and right is None):
            return 0
        return 1

    def get_neighbors(states, index):
        if index == 0:
            return (None, states[1])
        if index == len(states) - 1:
            return (states[-2], None)
        return (states[index-1], states[index+1])

    while days > 0:
        next_states = [0] * len(states)

        for i, _ in enumerate(states):
            left, right = get_neighbors(states, i)
            next_states[i] = next_state(left, right)

        states = next_states
        days -= 1

    return next_states
