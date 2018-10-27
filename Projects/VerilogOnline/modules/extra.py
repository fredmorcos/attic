import logging as log

from json       import load as jsonLoad
from os         import R_OK as READ
from os         import access
from tempfile   import mkstemp
from subprocess import Popen
from subprocess import PIPE

# global settings object
settings = None

# lambdas to simplify some stuff
mktempf = lambda suf: mkstemp(prefix='mgen_', suffix=suf, text=True)[1]
canRead = lambda filename: access(filename, READ)

def die(msg):
    log.critical(msg)
    if settings: settings.cleanup()
    exit(1)

def readJSONFile(filename, canDie=True):
    return jsonLoad(openFileForReading(filename, canDie))

def readFile(filename, canDie=True):
    f = openFileForReading(filename, canDie)
    data = f.readall()
    f.close()
    return data

def openFileForReading(filename, canDie=True):
    if not canRead(filename):
        if canDie:
            die('cannot open file for reading: %s' % filename)
        else:
            log.warning('cannot open file for reading: %s' % filename)
            return None

    log.info('loaded file %s' % filename)
    return open(filename, 'r')

def execProcess(cmdline, name, showOut=True, showErr=True,
                canDie=False, inData=None):
    # cmdline is a list of command and command line options
    # name    is the process metaname (ie, assembler, simulator)
    # showOut denotes whether to show the process stdout
    # showErr denotes whether to show the process stderr
    # canDie  is whether to die if the process returns non-zero
    inPipe = None

    if inData:
        inData = inData.encode()
        inPipe = PIPE

    try:
        p = Popen(cmdline, stdout=PIPE, stderr=PIPE, stdin=inPipe)
        outData, errData = p.communicate(inData)
        p.wait()
    except OSError:
        if canDie: die('error executing %s' % name)
        else:      return None, None, None

    outData = outData.decode().strip()
    errData = errData.decode().strip()

    fmt = '\n== %(name)s %(type)s ==\n\n%(data)s\n== end ==\n'

    if showOut and outData != '':
        log.info(fmt % {'name': name,
                        'type': 'stdout',
                        'data': outData})

    if showErr and errData != '':
        log.info(fmt % {'name': name,
                        'type': 'stderr',
                        'data': errData})

    if canDie and p.returncode != 0:
        die('%s return errorcode %s' % (name, p.returncode))

    return outData, errData, p.returncode
