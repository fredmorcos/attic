class TraverserFinished(Exception):
    pass

class TraverserError(Exception):
    pass

class InternalTraverserFinished(Exception):
    pass

class Traverser:
    def __init__(self, data, graph):
        self.data = data.splitlines()
        self.row = -1
        self.current_line = None
        self.graph = graph
        self.__make_line()

    def __next_line(self):
        self.row += 1
        max = len(self.data)
        if self.row >= max:
            if self.row > max:
                raise TraverserFinished()
            else:
                self.row -= 1
                self.__make_line()
                if int(self.__col_val(self.graph.clk_index)) == 0:
                    raise TraverserFinished()
                else:
                    raise InternalTraverserFinished()
        self.__make_line()

    def __make_line(self):
        self.current_line = self.data[self.row].split(',')

    def update_registered(self):
        for element in self.graph.elements:
            element.update_value(self.__col_val(element.value_index))

    def first_step(self):
        self.row = 0
        if self.row == len(self.data):
            raise TraverserFinished()
        self.__make_line()

    def advance(self):
        while int(self.__col_val(self.graph.clk_index)) == 0:
            self.__next_line()

        while int(self.__col_val(self.graph.clk_index)) == 1:
            try:
                self.__next_line()
            except InternalTraverserFinished:
                return 2

        self.row -= 2
        self.__next_line()
        self.row += 1

    def __col_val(self, col):
        return self.current_line[col].strip()
