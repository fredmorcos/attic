# This file is part of PET.
#
# PET is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# PET is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PET.  If not, see <http://www.gnu.org/licenses/>.

from datetime import datetime
from datetime import date as d

import calendar as cal

from Expense import Expense

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
