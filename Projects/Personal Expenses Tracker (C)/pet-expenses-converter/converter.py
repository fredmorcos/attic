#!/usr/bin/python

from datetime import datetime

with open ("exp.csv", "r") as f:
    s = f.readlines()

tags = s[0].split(",")
tags.remove("")
tags.remove("\n")

print(tags)

tail = s[1:]

for line in tail:
    sl = line.split(",")
    sl.remove("\n")
    date = datetime.strptime(sl[0], "%m/%d/%Y")
    datestr = date.strftime("%Y-%m-%d")

    for i, am in enumerate(sl[1:]):
        if am != "":
            print(str(am) + " " + datestr + " " + tags[i])
