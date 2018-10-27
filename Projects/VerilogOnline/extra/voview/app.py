import json, logging
from tkinter import messagebox
from subprocess import Popen, PIPE
from controlpanel import ControlPanel
from viewer import Viewer

class App:
    def __init__(self, root, machine, files, mach_dir):
        self.root = root
        self.machine = machine
        self.files = files
        self.mach_dir = mach_dir
        self.step = 0
        self.step_text = 'Step: %s'

        self.control_panel = ControlPanel(root, self)
        self.main_viewer = Viewer(self)
        self.update_all()

    def get_step_text(self):
        return self.step_text % self.step

    def do_render(self, data):
        self.main_viewer.update_machine(json.loads(data))

    def prev(self):
        if self.step == 0:
            return
        self.step -= 1
        self.update_all()

    def next(self):
        self.step += 1
        self.update_all()

    def quit(self):
        res = messagebox.askyesno(
            title='Quit',
            message='Are you sure you want to quit?',
            default=messagebox.YES)
        if res:
            self.root.quit()

    def update_all(self):
        self.control_panel.step_lab.configure(text=self.get_step_text())
        self.run_mgen()

    def run_mgen(self):
        cmdline = ['mgen', self.machine, str(self.step)]
        cmdline += self.files
        cmdline += ['--format', 'json', '-d', self.mach_dir]
        cmdline += ['-t', '2', '-e', '2', '-w', '2', '-r', '1']
        try:
            p = Popen(cmdline, stdout=PIPE, stderr=PIPE)
            p_stdout, p_stderr = p.communicate()
            p.wait()
        except OSError:
            logging.critical('error executing %s' % cmdline)
            exit(1)

        p_stdout = p_stdout.decode()
        p_stderr = p_stderr.decode()

        if p.returncode == 0:
            logging.info('success in running mgen, return code 0')
            self.do_render(p_stdout)
        elif p.returncode == 2:
            logging.warning('simulator premature finish, return code 2')
            res = messagebox.askyesno(
                title='Simulator finished',
                message='Simulator finished, restart from step 0?',
                default=messagebox.YES)
            if res:
                self.step = 0
                self.update_all()
            else:
                self.step -= 1
                self.update_all()
        else:
            logging.error('simulator error, return code %s' % p.returncode)
            logging.error('\n%s\n' % p_stderr)
            messagebox.showerror(
                title='Simulator error',
                message='Simulator error, will restart from step 0',
                default=messagebox.OK)
