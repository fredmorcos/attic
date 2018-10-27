import tkinter as tk
from tkinter.constants import BOTH, X

class ControlPanel(tk.Frame):
    def __init__(self, root, app):
        tk.Frame.__init__(self, root, borderwidth=1)
        root.title('VO Viewer')
        root.resizable(False, False)
        self.app = app
        self.build_toolbox(app)
        self.pack(expand=True, fill=BOTH)

    def build_toolbox(self, app):
        tk.Button(self, text='Previous', command=app.prev).pack(expand=True, fill=X)
        tk.Button(self, text='Next', command=app.next).pack(expand=True, fill=X)
        self.step_lab = tk.Label(self, text=app.get_step_text())
        self.step_lab.pack(expand=True, fill=X)
        tk.Button(self, text='Quit', command=app.quit).pack(expand=True, fill=X)
