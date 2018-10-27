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

def validate_float_param(param, name):
    if type(param) is str:
        return float(param)
    elif type(param) is float:
        return param
    elif type(param) is int:
        return float(param)
    else:
        raise ValueError("Bad type for %s value: %s (%s)" %
                         (name, amount, type(amount)))

def validate_str_param(param, name, indexed=True):
    if type(param) is None:
        return None
    elif type(param) is str:
        if len(param.strip()) == 0:
            return None
        else:
            return param.strip()
    elif type(param) is int and indexed:
        return param
    else:
        raise ValueError("Bad type for %s value: %s (%s)" %
                         (name, param, type(param)))
