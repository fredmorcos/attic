from sys import stderr

x0, y0, x1, y1 = 0, 0, 0, 0

def log_and_die(msg):
    stderr.write(msg + '\n')
    exit(1)
