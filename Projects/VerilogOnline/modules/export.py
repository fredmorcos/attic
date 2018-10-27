import json
import logging as log
import modules.extra as e

from modules.extra    import die
from modules.extra    import execProcess
from modules.json_out import jsonExport

def _compressJSON(data):
    out, err, rc = execProcess(['json_reformat', '-m'], 'json_compressor',
                               showOut=False, showErr=False, inData=data)
    if out: return out
    else:
        log.warning('could not execute json_reformat')
        if err: log.warning(err)

def export(machine):
    f = e.settings.format
    out = e.settings.result

    if f.startswith('js'):
        res = json.dumps(jsonExport(machine), indent=2)
        tmpJS = 'var machine = %s;'

        if   f == 'js':    out.write(tmpJS % res)
        elif f == 'jsc':   out.write(tmpJS % _compressJSON(res))
        elif f == 'json':  out.write(res)
        elif f == 'jsonc': out.write(_compressJSON(res))
        else:              die('unknown format')
