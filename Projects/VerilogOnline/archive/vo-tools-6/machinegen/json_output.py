import json, os
from os.path import relpath

class JSONOutput:
    def __init__(self, machine):
        self.machine  = machine
        self.settings = machine.settings
        self.logger   = self.settings['logger']

    def export(self):
        img_obj = self.load_machine_image()

        # add machine properties to it
        self.logger.info('updating image objects with machine (%s) element info' %
                         self.machine.title)

        for e in self.machine.elements:
            img_obj = self.update_image_from_element(img_obj, e)

        if not self.machine.toplevel and self.machine.is_mem:
            for e in self.machine.mem_elements:
                img_obj = self.update_image_from_element(img_obj, e)

        # add sub-machines to it
        img_obj['ext_objects'] = []

        for e in self.machine.ext_elements:
            # update submachine titles in this machine
            self.update_pattern_from_text(img_obj, e, self.machine.ext_elements[e].title)
            img_obj['ext_objects'] += [JSONOutput(self.machine.ext_elements[e]).export()]

        return img_obj

    def update_pattern_from_text(self, img, pattern, value):
        for o in img['objects']:
            if o['type'] == 'text':
                if o['string'] == pattern:
                    o['string'] = value

    def update_image_from_element(self, img, e):
        for o in img['objects']:
            o['can_toggle'] = e.can_toggle
            o['toggled'] = False

            if o['type'] == 'text':
                if o['string'] == '%TITLE%': # special case for the title
                    o['string'] = self.machine.title
                elif o['string'] == e.name:
                    o['metaname'] = o['string']
                    o['string'] = e.value
                    if e.changed: o['changed'] = True
            else:
                if o.has_key('metaname') and o['metaname'] == e.name:
                    if e.changed: o['changed'] = True
        return img

    def load_machine_image(self):
        # find machine image and load it
        image_filename = os.path.join(self.machine.dirname, self.machine.image)

        if os.access(image_filename, os.R_OK):
            with open(image_filename, 'r') as f: img_obj = json.load(f)
            self.logger.info('loaded machine image from %s' % relpath(image_filename))
        else:
            image_filename = os.path.join(self.settings['images'], self.machine.image)

            if os.access(image_filename, os.R_OK):
                with open(image_filename, 'r') as f: img_obj = json.load(f)
                self.logger.info('loaded machine image from %s' % relpath(image_filename))
            else:
                self.logger.die('cannot open image file %s' % self.machine.image)
        return img_obj
