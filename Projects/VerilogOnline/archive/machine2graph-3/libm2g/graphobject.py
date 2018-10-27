class GraphObject:
    def __init__(self, value_index, value_name):
        self.value = None
        self.changed = False
        self.value_index = value_index
        self.value_name = value_name

    def update_value(self, newval):
        if self.value != newval:
            self.value = newval
            self.changed = True

    def pre_export(self):
        val = ''
        color = ''

        if self.changed == True:
            color = 'red'
            self.changed = False
        else:
            color = 'black'

        if self.value != None:
            val = self.value

        return val, color
