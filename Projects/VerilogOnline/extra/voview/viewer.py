import tkinter as tk
from tkinter import font
from tkinter.constants import ALL, BOTH, LAST, FIRST, CENTER, \
     LEFT, RIGHT, N, S, E, W, HORIZONTAL

class Viewer(tk.Toplevel):
    def __init__(self, app):
        tk.Toplevel.__init__(self)
        self.app = app
        self.machine_obj = None
        self.build_canvas_area()
        self.scale_value = 20
        self.viewers = {}

    def update_machine(self, new_machine):
        self.machine_obj = new_machine
        self.render()

    def scale_changed(self, value):
        self.scale_value = int(value)
        self.render()

    def render(self):
        if not self.machine_obj:
            return

        self.canvas.delete(ALL)

        m = self.machine_obj
        m_info = m['info']
        m_objs = m['objects']
        m_eobjs = m['extobjects']
        m_mobjs = m['metaobjects']

        self.title(m_info['title'])
        self.canvas.configure(scrollregion=
                              (0, 0,
                               m_info['size'][0] / self.scale_value,
                               m_info['size'][1] / self.scale_value))

        thickness = float(m_info['thickness']) / self.scale_value
        if thickness < 1.0: thickness = 1.0

        fontsize = int(int(m_info['fontsize']) / self.scale_value)
        if fontsize < 8: fontsize = 8

        for o in m_objs:
            self.render_object(o, thickness, fontsize)

        for k,v in m_mobjs.items():
            for o in v:
                self.render_object(o, thickness, fontsize)

        for k,v in m_eobjs.items():
            for o in v['objects']:
                self.render_object(o, thickness, fontsize, True)
            if k in self.viewers:
                self.viewers[k].update_machine(v['data'])
            else:
                self.viewers[k] = Viewer(self.app)
                self.viewers[k].update_machine(v['data'])

    def render_object(self, o, thickness, fontsize, extobj=False):
        kwargs = {}
        args = ()

        if o['type'] == 'polyline':
            kwargs['width'] = thickness

            if o['subtype'] == 'polyline':
                if 'highlight' in o: kwargs['fill'] = 'red'
                if extobj: kwargs['fill'] = 'blue'

                if o['forwardarrow'] or o['backwardarrow']:
                    kwargs['arrowshape'] = (160 / self.scale_value,
                                            200 / self.scale_value,
                                            60 / self.scale_value)
                    if o['forwardarrow'] and o['backwardarrow']:
                        kwargs['arrow'] = BOTH
                    elif o['forwardarrow']:
                        kwargs['arrow'] = LAST
                    elif o['backwardarrow']:
                        kwargs['arrow'] = FIRST

                for i,p in enumerate(o['points']):
                    args += (p[0] / self.scale_value,)
                    args += (p[1] / self.scale_value,)

                return self.canvas.create_line(*args, **kwargs)
            elif o['subtype'] == 'box' or o['subtype'] == 'arcbox':
                if 'highlight' in o: kwargs['outline'] = 'red'
                if extobj: kwargs['outline'] = 'blue'

                p = o['points']
                return self.canvas.create_rectangle(p[0][0] / self.scale_value,
                                                    p[0][1] / self.scale_value,
                                                    p[1][0] / self.scale_value,
                                                    p[1][1] / self.scale_value,
                                                    **kwargs)
        elif o['type'] == 'text':
            kwargs['text'] = o['string']
            kwargs['font'] = font.Font(family='sans-serif', size=fontsize)
            if 'highlight' in o: kwargs['fill'] = 'red'
            if extobj: kwargs['fill'] = 'blue'

            if o['subtype'] == 'center':
                kwargs['justify'] = CENTER
                kwargs['anchor'] = CENTER
            elif o['subtype'] == 'left':
                kwargs['justify'] = LEFT
                kwargs['anchor'] = W
            elif o['subtype'] == 'right':
                kwargs['justify'] = RIGHT
                kwargs['anchor'] = E

            # hack for memory text elements
            if '\n' in o['string']:
                kwargs['justify'] = LEFT
                kwargs['anchor'] = N + W

            return self.canvas.create_text(o['position'][0] / self.scale_value,
                                           o['position'][1] / self.scale_value,
                                           **kwargs)

    def build_canvas_area(self):
        self.frame = tk.Frame(self, borderwidth=1)
        self.frame.columnconfigure(0, weight=2)
        self.frame.rowconfigure(0, weight=2)

        self.canvas = tk.Canvas(self.frame, bg='#FFF')
        self.hscroll = tk.Scrollbar(self.frame, orient=tk.HORIZONTAL,
                                    command=self.canvas.xview)
        self.vscroll = tk.Scrollbar(self.frame, orient=tk.VERTICAL,
                                    command=self.canvas.yview)
        self.scale = tk.Scale(self.frame, command=self.scale_changed,
                              orient=HORIZONTAL, showvalue=0,
                              resolution=1, from_=20, to=1)
        self.scale.set(20)

        self.canvas['xscrollcommand'] = self.hscroll.set
        self.canvas['yscrollcommand'] = self.vscroll.set

        self.canvas.grid(column=0, row=0, sticky=N+S+E+W)
        self.hscroll.grid(column=0, row=1, sticky=E+W, padx=1, pady=1)
        self.vscroll.grid(column=1, row=0, sticky=N+S, padx=1, pady=1)
        self.scale.grid(column=0, columnspan=2, row=2, sticky=E+W)
        self.frame.pack(expand=True, fill=BOTH)
