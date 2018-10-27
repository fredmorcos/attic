def parse_document(lines):
    return map(_parse_line, lines, range(len(lines)))

def _parse_line(line, num):
    return _parse_line_helper(line.strip(), num)

def _parse_line_helper(line, num):
    if line.startswith("#"):
        if num == 0:
            return ("version", line.split(" ")[1])
        else:
            return None
    elif num == 0:
        return ("error", line, num,
                "expecting line 0 to start with #FIG <version>")
    elif num == 1:
        if line in ["Landscape", "Portrait"]:
            return ("orientation", line)
        else:
            return ("error", line, num,
                    "line 1 must have an orientation property")
    elif num == 2:
        if line in ["Center", "Flush Left"]:
            return ("justification", line)
        else:
            return ("error", line, num,
                    "line 2 must have a justification property")
    elif num == 3:
        if line in ["Metric", "Inches"]:
            return ("units", line)
        else:
            return ("error", line, num,
                    "line 3 must have a unitrs property")
    elif num == 4:
        if line in ["Letter", "Legal", "Ledger", "Tabloid",
                    "A", "B", "C", "D", "E",
                    "A4", "A3", "A2", "A1", "A0", "B5"]:
            return ("papersize", line)
        else:
            return ("error", line, num,
                    "line 4 must have a papersize property")
    else:
        pass
