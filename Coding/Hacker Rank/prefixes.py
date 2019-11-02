#!/bin/python3

import os
import sys

def prefixes(words):
    class Trie:
        def __init__(self, letter):
            self.letter = letter
            self.children = {}
            self.is_word = False

        def insert(self, word):
            if word == "":
                self.is_word = True
                return
            if word[0] not in self.children:
                self.children[word[0]] = Trie(word[0])
            self.children[word[0]].insert(word[1:])

        def contains_prefix_of(self, word):
            if word == "":
                if self.is_word:
                    return True
                return False
            if self.is_word:
                return True
            if word[0] not in self.children:
                return False
            return self.children[word[0]].contains_prefix_of(word[1:])

        def is_prefix_of(self, prefix):
            if prefix == "":
                return True
            if prefix[0] in self.children:
                return self.children[prefix[0]].is_prefix_of(prefix[1:])
            return False

    db = {}
    for word in words:
        if word[0] in db:
            if db[word[0]].is_word:
                print("BAD SET")
                print(word)
                return
            if db[word[0]].contains_prefix_of(word[1:]):
                print("BAD SET")
                print(word)
                return
            if db[word[0]].is_prefix_of(word[1:]):
                print("BAD SET")
                print(word)
                return
            db[word[0]].insert(word[1:])
        else:
            db[word[0]] = Trie(word[0])
            db[word[0]].insert(word[1:])
    print("GOOD SET")

if __name__ == '__main__':
    words_rows = int(input())

    words = []

    for _ in range(words_rows):
        words.append(input())

    prefixes(words)
