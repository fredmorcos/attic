#!/usr/bin/python

import gtk
import gtk.gdk

class line:
  def __init__(self, x1 = 0, y1 = 0, x2 = 0, y2 = 0):
    self.x1 = x1
    self.y1 = y1
    self.x2 = x2
    self.y2 = y2

class shape(object):
  def __init__(self, x = 0, y = 0, w = 0, h = 0):
    self._x = x
    self._y = y
    self.w = w
    self.h = h
    self.midx = int(x + (w / 2))
    self.midy = int(y + (h / 2))

  @property
  def x(self):
    return self._x

  @x.setter
  def x(self, val):
    self._x = val
    self.midx = int(self._x + (self.w / 2))

  @property
  def y(self):
    return self._y

  @y.setter
  def y(self, val):
    self._y = val
    self.midy = int(self._y + (self.h / 2))

lines = []
objects = [shape(10, 10, 100, 50), shape(100, 100, 100, 50),
           shape(100, 200, 150, 100)]
obj = None
moving = False
margin = 5

def da_motion(widget, event):
  global moving
  global lines

  lines = []

  if moving == True and obj != None:
    obj.x = int(event.x)
    obj.y = int(event.y)

    for o in objects:
      if o != obj:
        if o.x > obj.x - margin and o.x < obj.x + margin:
          lines += [line(o.x, o.midy, o.x, obj.midy)]
        if o.y > obj.y - margin and o.y < obj.y + margin:
          lines += [line(o.midx, o.y, o.midx, obj.y)]

    widget.queue_draw()

def da_buttonrel(widget, event):
  global moving
  global lines

  moving = False
  lines = []

  widget.queue_draw()

def da_expose(widget, event):
  cc = widget.window.cairo_create()
  cc.rectangle(0, 0, widget.get_allocation().width,
               widget.get_allocation().height)
  cc.set_source_rgb(1, 1, 1)
  cc.fill()
  cc.set_source_rgb(0.3, 0.3, 0.3)
  cc.set_line_width(2)

  for o in objects:
    cc.rectangle(o.x, o.y, o.w, o.h)
    cc.stroke()

  cc.set_source_rgb(1, 0, 0)
  for l in lines:
    cc.move_to(l.x1, l.y1)
    cc.line_to(l.x2, l.y2)
    cc.stroke()

  return False

def da_buttonpress(widget, event):
  global obj
  global moving

  for o in objects:
    if event.x > o.x and event.x < o.x + o.w and event.y > o.y and \
       event.y < o.y + o.h:
      obj = o
      moving = True
      return False
  obj = None

w = gtk.Window()
w.connect("delete-event", gtk.main_quit)
w.set_size_request(600, 600)
da = gtk.DrawingArea()
da.add_events(gtk.gdk.ALL_EVENTS_MASK)
da.connect("expose-event", da_expose)
da.connect("button-press-event", da_buttonpress)
da.connect("button-release-event", da_buttonrel)
da.connect("motion-notify-event", da_motion)
w.add(da)
w.show_all()
gtk.main()
