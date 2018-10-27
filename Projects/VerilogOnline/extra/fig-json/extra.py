from sys import stderr

avg_radius,    num_radius    = 0, 0
avg_thickness, num_thickness = 0, 0
avg_fontsize,  num_fontsize  = 0, 0

x0, y0, x1, y1 = 0, 0, 0, 0

def log(msg): stderr.write(msg + '\n')
def die(msg): log(msg); exit(1);
