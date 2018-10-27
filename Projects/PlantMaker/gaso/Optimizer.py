def run(orders):
    # sort by nearest-deadline-first
    return sorted(orders, lambda a, b: cmp(a.deadline, b.deadline))
