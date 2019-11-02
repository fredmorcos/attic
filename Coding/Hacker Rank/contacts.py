#!/bin/python3

import os
import sys

def contacts_slow(queries):
    contacts_db = {}
    counts = []

    for query in queries:
        command = query[0]

        if command == "add":
            contact = query[1]
            level = contacts_db
            for c in contact:
                if c not in level:
                    level[c] = []
                level = level[c]
        elif command == "find":
            partial = query[1]
            level = contacts_db
            for c in partial:
                if c not in level:
                    counts += [0]
                    break
                else:
                    level = level[c]
            print(level)
            counts += [len(level)]
    print(contacts_db)
    print(counts)
    return counts

    contacts_db = []
    counts = []

    for query in queries:
        command = query[0]

        if command == "add":
            contact = query[1]
            contacts_db += [contact]
        elif command == "find":
            partial = query[1]
            contacts_db.sort()
            do_exit = False
            count = 0
            for contact in contacts_db:
                if contact.startswith(partial):
                    count += 1
                    do_exit = True
                elif do_exit == True:
                    break
            counts += [count]

    return counts


def contacts(queries):
    class Trie:
        def __init__(self):
            self.children = {}
            self.is_word = False
            self.count = 0

        def insert(self, word):
            self.count += 1
            if word == "":
                self.is_word = True
                return
            if word[0] not in self.children:
                self.children[word[0]] = Trie()
            self.children[word[0]].insert(word[1:])

        def is_prefix(self, prefix):
            if prefix == "":
                return self
            if prefix[0] not in self.children:
                return None
            return self.children[prefix[0]].is_prefix(prefix[1:])

    contacts_db = {}
    counts = []
    for query in queries:
        command = query[0]
        if command == "add":
            contact = query[1]
            if contact[0] not in contacts_db:
                contacts_db[contact[0]] = Trie()
            contacts_db[contact[0]].insert(contact[1:])
        elif command == "find":
            prefix = query[1]
            if prefix[0] in contacts_db:
                res = contacts_db[prefix[0]].is_prefix(prefix[1:])
                counts += [0] if res is None else [res.count]
            else:
                counts += [0]
    return counts

if __name__ == '__main__':
    fptr = open(os.environ['OUTPUT_PATH'], 'w')

    queries_rows = int(input())

    queries = []

    for _ in range(queries_rows):
        queries.append(input().rstrip().split())

    result = contacts(queries)

    fptr.write('\n'.join(map(str, result)))
    fptr.write('\n')

    fptr.close()
