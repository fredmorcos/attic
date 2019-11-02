def pop_toys(num_toys, top_toys, toys, num_quotes, quotes):
    assert num_toys == len(toys)
    assert num_quotes == len(quotes)

    # Map from toy name to
    toys_dict = {}
    for toy in toys:
        assert toy not in toys_dict
        toys_dict[toy] = 0

    # All toys have a score of 0 at the beginning
    toy_scores = {0: toys}

    for quote in map(lambda q: q.split(" "), quotes):
        for word in map(lambda w: w.strip(), quote):
            if not word or word not in toys_dict:
                continue
            old_score = toys_dict[word]
            assert word in toy_scores[old_score]
            toy_scores[old_score].remove(word)
            if not toy_scores[old_score]:
                del toy_scores[old_score]
            new_score = old_score + 1
            if new_score not in toy_scores:
                toy_scores[new_score] = []
            assert word not in toy_scores[new_score]
            toy_scores[new_score] += [word]
            toys_dict[word] += 1

    print(toy_scores)

    result = []
    keys = list(toy_scores.keys())
    keys.sort(reverse=True)

    for key in keys:
        toy_scores[key].sort()
        for toy in toy_scores[key]:
            result += [toy]
            top_toys -= 1
            if top_toys == 0:
                return result
    return result
