import json, base64, sys
from os import path
from graphobject import GraphObject

js_show_func = '''function show%(func_name)s () {
  window.open("data:text/html;base64,%(page_data)s", "%(element_name)s",
  "toolbar=no,location=no,directories=no,status=no,menubar=no");
};
'''

js_button = '<td><button type=\\"button\\" ' + \
            'onClick=\\"show%(func_name)s()\\">' + \
            'Show %(element_name)s</button></td>'

class Graph:
    def __init__(self, def_file):
        self.elements = []
        self.ext_elements = {}

        self.dirname = path.dirname(def_file)
        with open(def_file, 'r') as f: data = json.loads(f.read())

        self.title = data['title']
        self.toplevel = data['toplevel']
        self.image_file = data['image_file']

        self.postprocess = data['postprocess']
        self.pp_command = data['pp_command']

        if self.toplevel:
            self.clk_index = data['clk_index']
            self.time_index = data['time_index']
            self.verilog_file = data['verilog_file']

        if data.has_key('elements'):
            elements = data['elements']
            for name in elements:
                self.elements += [GraphObject(elements[name], name)];

        if data.has_key('external_elements'):
            ext_elements = data['external_elements']
            for name in ext_elements:
                self.ext_elements[name] = Graph(self.__join_fn(ext_elements[name]))

        if self.toplevel:
            self.verilog_file = self.__join_fn(self.verilog_file)
            self.assembler = data['assembler']
        self.image_file = self.__join_fn(self.image_file)

    def __join_fn(self, filename):
        return path.join(self.dirname, filename)

    def unmark_all(self):
        for element in self.elements:
            element.changed = False

    def export(self, page, prog_data):
        with open(page, 'r') as f: page_data = f.read()
        with open(self.image_file, 'r') as f: d = f.read().replace("%TITLE%", self.title)

        for element in self.elements:
            if (isinstance(element.value_index, str) or \
                isinstance(element.value_index, unicode)) and \
               element.value_index.startswith('__'):

                ##########################################################################
                ####                         DIRTY HACK NOTICE                        ####
                #### workaround the lack of newline support in SVG, USE TSPAN INSTEAD ####
                ##########################################################################

                mem_data = prog_data[element.value_index]
                font_size = 192
                if element.value_name == '%DATA1%':
                    x = '1500'
                elif element.value_name == '%DATA2%':
                    x = '6900'
                else:
                    sys.stderr.write('something is awfully wrong in the SVG newline hack')
                    exit(1)

                mem_data = mem_data.split('\n')
                final_mem_data = mem_data[0]
                for line in mem_data[1:]:
                     final_mem_data += '<tspan x="%(x)s" dy="%(dy)s">%(line)s</tspan>' \
                                       % {'x': x, 'dy': font_size, 'line': line}

                d = d.replace(element.value_name, final_mem_data)
                # d = d.replace(element.value_name, prog_data[element.value_index])

                ##########################################################################
                ####                       END DIRTY HACK NOTICE                      ####
                ##########################################################################
                
            else:
                val, color = element.pre_export()
                d = d.replace(element.value_name, val)

        _js_functions = ''
        _js_buttons = ''

        for elementname in self.ext_elements:
            t = self.ext_elements[elementname].title
            t_nospace = t.replace(' ', '')
            
            d = d.replace(elementname, t)
            data_dict = {'func_name':    t_nospace,
                         'page_data':    base64.b64encode(self.ext_elements[elementname].\
                                                          export(page, prog_data)),
                         'element_name': t}
            
            _js_functions += js_show_func % data_dict
            _js_buttons += js_button % data_dict

        return page_data % {'functions':  _js_functions,
                            'image_data': base64.b64encode(d),
                            'buttons':    _js_buttons}
