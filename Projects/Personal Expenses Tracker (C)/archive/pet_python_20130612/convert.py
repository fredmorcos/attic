#!/usr/bin/env python

import sys
import json

from datetime import datetime
from datetime import timedelta
from datetime import date as d

import calendar as cal

today = d.today

VERSION = "0.2"

class Expenses:
    def __init__(self, options):
        self.exps = {}
        self.options = options

    def add_date(self, date_val):
        if type(date_val) is str:
            date = Expenses.parse_date(date_val)
        elif type(date_val) is d:
            date = date_val
        else:
            raise ValueError("%s is not a date value" % date_val)

        if date not in self.exps.keys():
            self.exps[date] = []

        return date

    def add_expense(self, date_val, exp):
        if type(exp) is not Expense:
            raise ValueError("%s is not an expense value" % exp)

        date = self.add_date(date_val)
        self.exps[date].append(exp)

    def get_latest_date(self):
        return sorted(self.exps.keys())[-1]

    def __str__(self):
        res = ""

        for date in sorted(self.exps.keys()):
            if len(self.exps[date]) > 0:
                res += str(date)

                if self.options["extended"]:
                    res += " (" + Expenses.get_extended_date(date) + ")\n"
                else:
                    res += "\n"

                for exp in self.exps[date]:
                    res += "  " + str(exp) + "\n"

        # return and avoid a final newline
        return res[:-1]

    def to_json(self):
        res = {}

        for k in self.exps.keys():
            if type(k) is not d:
                raise ValueError("Bad type of date %s" % k)
            else:
                res_exp = []

                for e in self.exps[k]:
                    res_exp.append(e.to_json())

                res[str(k)] = res_exp

        return res

    @classmethod
    def get_extended_date(cls, date):
        return ("%s %s %d, %d" %
                (cal.day_name[cal.weekday(date.year, date.month, date.day)],
                 cal.month_name[date.month],
                 date.day,
                 date.year))

    @classmethod
    def parse_date(cls, date_str):
        try:
            return datetime.strptime(date_str, "%Y-%m-%d").date()
        except ValueError as e:
            raise ValueError("Invalid date: %s" % str(e))

    @classmethod
    def from_json(cls, data, options):
        exps = Expenses(options)

        for d in data.keys():
            date = exps.add_date(d)

            if type(data[date.isoformat()]) is not list:
                raise ValueError("Invalid list of expenses for date %s" % date)

            for exp in data[date.isoformat()]:
                exps.add_expense(date, Expense.from_json(date, exp))

        return exps

class Expense:
    def __init__(self, date, amount, tags, note):
        ESTR_BAD_AMT_VAL = "Bad amount value for an expense at date %s"
        ESTR_BAD_TAG_LST = "Bad tags list for an expense at date %s"
        ESTR_EMP_TAG_LST = "Tags list for an expense at date %s is empty"
        ESTR_INV_NTE     = "Note for an expense at date %s is invalid"
        ESTR_INV_TAG     = "Tag for an expense at date %s is invalid"

        if type(amount) is not float and type(amount) is not int:
            raise ValueError(ESTR_BAD_AMT_VAL % date)
        elif type(tags) is not list:
            raise ValueError(ESTR_BAD_TAG_LST % date)
        elif len(tags) == 0:
            raise ValueError(ESTR_EMP_TAG_LST % date)
        elif note is not None and type(note) is not str:
            raise ValueError(ESTR_INV_NTE % date)

        for tag in tags:
            if type(tag) is not str:
                raise ValueError(ESTR_INV_TAG)

        self.amount = float(amount)
        self.tags = tags
        self.note = note

    def __str_tags__(self):
        res = "["
        for tag in self.tags:
            res += tag + ", "
        return res[:-2] + "]"

    def __str_note__(self):
        if self.note is None or self.note == "":
            return "<none>"
        else:
            return self.note

    def __str__(self):
        return ("%.2f %s %s" % (self.amount, self.__str_tags__(),
                                self.__str_note__()))

    def to_json(self):
        return {"amount": self.amount, "tags": self.tags, "note": self.note}

    @classmethod
    def parse_amount(cls, amount_str):
        return float(amount_str)

    @classmethod
    def parse_tags(cls, tags_str):
        __tags = tags_str.split(",")
        new_tags = []

        for tag in __tags:
            if tag.strip() == "":
                continue
            else:
                new_tags += [tag.strip()]

        if len(new_tags) == 0:
            raise ValueError("List of tags is empty")

        return new_tags

    @classmethod
    def parse_note(cls, note_str):
        return note_str.strip()

    @classmethod
    def from_json(cls, date, data):
        if data["amount"] is None:
            raise ValueError("No amount value for expense at date" % date)

        if data["tags"] is None:
            raise ValueError("No tags value for expense at date" % date)

        return Expense(date, data["amount"], data["tags"],
                       data.get("note", None))

if __name__ == "__main__":
    filename = None

    if len(sys.argv) > 1:
        filename = sys.argv[1]
        print("file to convert: %s" % filename, file=sys.stderr)
    else:
        sys.exit(1)

    exps = Expenses(dict(extended=False))

    i = 1

    try:
        with open(filename) as f:
            for line in f:
                if len(line.strip()) == 0:
                    print("line %d: <<<empty line>>>" % i, file=sys.stderr)
                else:
                    __splitline = filter(lambda x: len(x) != 0, line.split(" "))
                    splitline = []
                    for elem in __splitline:
                        splitline.append(elem)
                    if len(splitline) >= 6:
                        if splitline[0].strip() == "{":
                            if splitline[-1].strip() == "}":
                                finline = splitline[1:-1]
                                assert(len(finline) == len(splitline) - 2)
                                a = Expense.parse_amount(finline[0])
                                # d = Expense.parse_date(finline[1])
                                t = Expense.parse_tags(finline[2])
                                if finline[3].find('"') != -1:
                                    if finline[-1].find('"') != -1:
                                        if finline[3][0] == '"':
                                            if finline[-1][-1] == '"':
                                                note = " ".join(finline[3:]).strip('"')
                                                n = Expense.parse_note(note)
                                                exps.add_expense(finline[1], Expense(finline[1], a, t, n))
                                            else:
                                                print("line %d: <no \">" % i, file=sys.stderr)
                                                print(" > %s" % finline[-1][-1], file=sys.stderr)
                                                sys.exit(1)
                                        else:
                                            print("line %d: <no \">" % i, file=sys.stderr)
                                            print(" > %s" % finline[3][0], file=sys.stderr)
                                            sys.exit(1)
                                    else:
                                        print("line %d: <<<no \">>>" % i, file=sys.stderr)
                                        print("  --> %s" % finline[-1], file=sys.stderr)
                                        sys.exit(1)
                                else:
                                    print("line %d: <<<no \">>>" % i, file=sys.stderr)
                                    print("  --> %s" % finline[3], file=sys.stderr)
                                    sys.exit(1)
                            else:
                                print("line %d: <<<no }>>>" % i, file=sys.stderr)
                                print("  --> %s" % splitline[-1], file=sys.stderr)
                                sys.exit(1)
                        else:
                            print("line %d: <<<no {>>>" % i, file=sys.stderr)
                            print("  --> %s" % splitline[-1], file=sys.stderr)
                            sys.exit(1)
                    else:
                        print("line %d: <<<too short>>>" % i, file=sys.stderr)
                        sys.exit(1)
                i += 1
        # res = json.dumps(exps.to_json(),
        #                  indent=None,
        #                  sort_keys=True,
        #                  separators=(",", ":"))
        res = json.dumps(exps.to_json(),
                         indent=2,
                         sort_keys=True)
        print(res)
    except OSError as e:
        print("Error: line %d" % i, e)
        sys.exit(e.errno)
    except ValueError as e:
        print("Error: line %d" % i, e)
        sys.exit(1)
