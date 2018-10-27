class Logger:
    def __init__(self, settings):
        self.settings = settings

    def text(self, msg):
        self.settings['text'].write(msg + '\n')

    def info(self, msg):
        self.settings['text'].write('I %s\n' % msg)

    def error(self, msg):
        self.settings['error'].write('E %s\n' % msg)

    def die(self, msg):
        self.error(msg)
        exit(1)

    def warning(self, msg):
        self.settings['warn'].write('W %s\n' % msg)

    def output(self, msg):
        self.settings['result'].write(msg + '\n')
