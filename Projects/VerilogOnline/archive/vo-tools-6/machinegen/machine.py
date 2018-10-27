import os, json
from os import path

class Element:
    """
    This represents a simple element in a component (or machine) such
    as wires and registers.
    """
    def __init__(self, name, index, can_toggle=True):
        """
        Name is the string pattern of the element, index is the column index
        in the simulation output, can_toggle is whether it is possible to
        turn on/off tracking of this element's value changes, useful at the
        rendering stage.
        """
        self.name = name
        self.index = index
        self.can_toggle = can_toggle
        
        self.value = None
        self.changed = False

    def update(self, new_value):
        """
        This updates the value of the component and sets it as changed for
        coloring in the rendering stage if the new value is different than
        the old one.
        """
        if self.value != new_value:
            self.value = new_value
            self.changed = True

    def reset(self):
        """
        This removes the changes indicator.
        """
        self.changed = False

class Machine:
    """
    This represents a component (or a machine in case of toplevel).
    """
    def __init__(self, name, settings, toplevel=True):
        """
        Name is the name of the component (or machine), settings is the global
        settings dict and toplevel indicates whether it is a toplevel component
        or not.
        """
        self.settings = settings
        self.logger = settings['logger']

        tmp_machine_file = path.join(settings['definitions'], settings['machine'],
                                     name + settings['def_ext'])

        if os.access(tmp_machine_file, os.R_OK):
            with open(tmp_machine_file, 'r') as f: def_data = json.load(f)
        else:
            self.logger.die('Cannot open definition file %s' % tmp_machine_file)

        self.toplevel     = toplevel
        self.dirname      = path.dirname(tmp_machine_file)
        self.elements     = []
        self.ext_elements = {}

        self.title = self.get_setting(def_data, 'title', settings['machine']).strip()
        self.image = self.get_setting(def_data, 'image', '').strip()

        if self.image == '':
            self.logger.die('machine has no image defined')
        
        if toplevel:
            self.simulator  = self.get_setting(def_data, 'simulator', '').strip()
            self.machine    = self.get_setting(def_data, 'machine', '').strip()
            self.assembler  = self.get_setting(def_data, 'assembler', '').strip()
            self.debug_info = self.get_setting(def_data, 'debug_info', False)
            self.single_mem = self.get_setting(def_data, 'single_mem', False)
        else:
            if def_data.has_key('is_mem'):
                self.is_mem = def_data['is_mem']

                if self.is_mem:
                    self.mem_name = def_data['mem_name']
                    self.mem_elements = []

                    if def_data.has_key('mem_elements'):
                        tmp_mem_elements = def_data['mem_elements']
                        for name in tmp_mem_elements:
                            self.mem_elements += [[name, tmp_mem_elements[name]]]
            else:
                self.is_mem = False

        if def_data.has_key('elements'):
            tmp_elements = def_data['elements']
            for name in tmp_elements:
                tmp_props = tmp_elements[name]
                self.elements += [Element(name, tmp_props[0], tmp_props[1])]

        if def_data.has_key('external_elements'):
            tmp_ext_elements = def_data['external_elements']
            for name in tmp_ext_elements:
                tmp_element = tmp_ext_elements[name]
                self.ext_elements[name] = Machine(tmp_element, settings, toplevel=False)

    def get_setting(self, data, name, default):
        """
        Get setting with default value if setting name not found.
        """
        if data.has_key(name):
            return data[name]
        return default

    def load_values(self, values):
        """
        Puts values generated at the simulation level into their respective components.
        """
        for e in self.elements:
            e.update(values[e.index])

        for e in self.ext_elements:
            self.ext_elements[e].load_values(values)

    def load_mem(self):
        """
        Loads memory data from respective files (generated from the assembler stage)
        into the appropriate memories in the machine.
        """
        s = self.settings
        
        if (not self.toplevel) and self.is_mem:
            for e in self.mem_elements:
                if e[1] == 'PROG' and s.has_key('program_prog'):
                    with open(s['program_prog'], 'r') as f:  e += [f.readlines()]
                elif e[1] == 'DPROG' and s.has_key('program_dprog'):
                    with open(s['program_dprog'], 'r') as f: e += [f.readlines()]
                elif e[1] == 'DATA' and s.has_key('program_data'):
                    with open(s['program_data'], 'r') as f:  e += [f.readlines()]
                elif e[1] == 'DDATA' and s.has_key('program_ddata'):
                    with open(s['program_ddata'], 'r') as f: e += [f.readlines()]
        for e in self.ext_elements:
            self.ext_elements[e].load_mem()

    def mem_write(self, data):
        """
        Executes a memwrite instruction/command. Finds the appropriate memory
        element and writes to it the data.
        """
        if (not self.toplevel) and self.is_mem:
            for e in self.mem_elements:
                if e[0] == data[0]:
                    address = int(data[1], 16)
                    e[2][address] = data[2]
        for e in self.ext_elements:
            self.ext_elements[e].mem_write(data)

    def unmark_all(self):
        """
        Unmarks all element and sub-components in this component. In other words,
        resets all the 'changed' markers as to not color any components when at the
        rendering stage, this is useful for the very first render (clock cycle 0).
        """
        for element in self.elements:
            element.reset()

        for name in self.ext_elements:
            self.ext_elements[name].unmark_all()
