#!/usr/bin/env python3

import sys
import shutil

import logging as log

from json import load as jsonLoad
from argparse import ArgumentParser as AP

from tempfile import mkstemp

from os import environ as env
from os import path
from os import devnull
from os import fdopen
from os import remove

from subprocess import Popen as popen
from subprocess import PIPE

class Settings:
    def clean(self):
        d = self.__dict__

        # Delete all sorts of leftover temporary files from the global
        # settings dict.
        files = ["programProg",
                 "programData",
                 "programDProg",
                 "programDData",
                 "userMachineFile",
                 "simcodeFile"]

        [remove(d[i]) for f in files if f in d]

        # Flush all file standard output buffers and result file
        # descriptor caches.
        self.text.flush()
        self.error.flush()
        self.warn.flush()
        self.result.flush()

def checkAndSplitSimulationOutput(data):
    sInd = "=== start ==="
    eInd = "=== end ==="

    if sInd not in data:
        die("Start-ind not found in sim output.")

    if eInd not in data:
        die("End-ind not found in sim output.")

    if data.index(eInd) < data.index(sInd):
        die("End-ind found before start-ind in sim output.")

    return data.split(sInd)[1].split(eInd)[0].strip()

def runVeriwellSimulation():
    # sc_d = simcode_data
    # sc_f = simcode_file
    sc_d = machine["simcode"]
    sc_d = sc_d.replace("%MAX_STEPS%", str(settings.maxSteps))
    sc_d = sc_d.replace("%PROG%", "\"%s\"" % settings.programProg)

    if not machine["single_mem"]:
        sc_d = sc_d.replace("%DATA%", "\"%s\"" % settings.programData)

    sc_f = mktempf("_user_sc")

    with open(sc_f, "w") as f:
        f.write(sc_d)

    settings.simcodeFile = sc_f

    p_cmd = [machine["simulator"],
             "-l", settings.simLogFile,
             "-k", devnull, sc_f]

    data = execproc(p_cmd, showout=False, showerr=False)[0]
    return checkAndSplitSimulationOutput(data)

def runStandaloneSimulation():
    p_cmd = [machine["simulator"], "--input", settings.programProg]

    if not machine["single_mem"]:
        p_cmd += [settings.programData]

    data, err, rc = execproc(p_cmd)

    if rc != 0:
        die("Error running standalone simulation.")

    return checkAndSplitSimulationOutput(data)

def findDataProgFiles(files):
    # Try looking for the data file.
    if files[0].endswith(".d"):
        datafile = files[0]
    elif files[1].endswith(".d"):
        datafile = files[1]
    else:
        die("Cannot find data (.d) file.")

    # Try looking for the program file.
    if files[0].endswith(".p"):
        progfile = files[0]
    elif files[1].endswith(".p"):
        progfile = files[1]
    else:
        die("Cannot find program (.p) file.")

    return datafile, progfile

def execproc(cmd, showout=True, showerr=True, indata=None):
    inpipe = None

    # Check if we need to send data to the program's stdin.
    if indata:
        indata = indata.encode()
        inpipe = PIPE

    # Do the execution and wait until it is finished.
    try:
        p = popen(cmd, stdout=PIPE, stderr=PIPE, stdin=inpipe)
        outdata, errdata = p.communicate(indata)
        p.wait()
    except OSError:
        return None, None, None

    # Read subprocess stdout and stderr.
    outdata = outdata.decode().strip()
    errdata = errdata.decode().strip()

    # Format of delimiter string.
    fmt = "\n== %(name)s %(type)s ==\n\n%(data)s\n== end ==\n"

    if showout and outdata != "":
        log.info(fmt % {"name": name,
                        "type": "stdout",
                        "data": outdata})

    if showerr and errdata != "":
        log.info(fmt % {"name": name,
                        "type": "stderr",
                        "data": errdata})

    return outdata, errdata, p.returncode

def tryAssemble(program):
    assembler = machine["assembler"]
    singleMem = machine["single_mem"]
    debugInfo = machine["debug_info"]

    # Add the instruction files.
    prog = mktempf("_prog")
    p_cmd = [assembler, program, prog]
    settings.programProg = prog

    # If we have more than a single memory, then probably the first
    # mem is the instruction memory and second one is the data memory.
    if not singleMem:
        data = mktempf("_data")
        p_cmd += [data]
        settings.programData = data

    # Enable debug info for instructions and data.
    if debugInfo:
        dprog = mktempf("_dprog")
        p_cmd += [dprog]
        settings.programDProg = dprog

        if not singleMem:
            ddata = mktempf("_ddata")
            p_cmd += [ddata]
            settings.programDData = ddata

    # Return the returncode from the execution of the assembler.
    return execproc(p_cmd, showout=False)[2]

def die(msg):
    log.critical(msg)
    cleanupSettings()
    exit(1)

def mktempf(suffix):
    return mkstemp(prefix="mgen_", suffix=suffix, text=True)[1]

if __name__ == "__main__":
    global settings
    global machine

    # The bin dir contains symlinks to all relevant binaries
    # (subtools, simulators, assemblers, etc...). Here we add it to
    # the PATH so that we simply spawn using the executable name
    # without the need of constructing the full path and filename.
    fullFilename = path.dirname(__file__)
    env["PATH"] += ":" + path.join(fullFilename, "bin")

    # We set the logging format.
    fmt = "%(levelname)s-%(module)s:%(lineno)s: %(message)s"
    log.basicConfig(format=fmt, level=log.NOTSET)

    # We set some shorter names for stdout and stderr files.
    stdoutf = sys.stdout.fileno()
    stderrf = sys.stderr.fileno()

    # We start setting up our command line arguments parser.
    ap = AP(description="Online Microarchitecture Simulation Tool")
    addarg = ap.add_argument

    # We add our command line arguments descriptions and options.
    addarg("machine", type=str, metavar="MACHINE", help="Machine name to run")
    addarg("step", type=int, metavar="STEP", help="Step to generate")
    addarg("files", metavar="FILES", nargs="+", help="Program files to run")
    addarg("-m", "--max-steps", default=500, type=int, metavar="N",
           help="Maximum number of steps [%(default)s]")
    addarg("-x", "--range", default=10, type=int, metavar="N",
           help="Range of steps to cache [%(default)s]")
    addarg("-f", "--format", default="js", type=str, metavar="F",
           choices=["js", "json"],
           help="Output format (%(choices)s) [%(default)s]")
    addarg("-c", "--compress", default=False, type=bool, metavar="C",
           help="Compress output [%(default)s]")
    addarg("-d", "--mach-dir", default="machines/", type=str, metavar="D",
           help="Directory with machine files [%(default)s]")
    addarg("-t", "--text", default=stdoutf, type=int, metavar="FD",
           help="Text output file descriptor [stdout]")
    addarg("-e", "--error", default=stderrf, type=int, metavar="FD",
           help="Error output file descriptor [stderr]")
    addarg("-w", "--warn", default=stderrf, type=int, metavar="FD",
           help="Warning output file descriptor [stderr]")
    addarg("-r", "--result", default=7, type=int, metavar="FD",
           help="Result output file descriptor [%(default)s]")
    addarg("-l", "--sim-log", default=devnull, type=str, metavar="F",
           help="Dump simulation log to file [%(default)s]")

    # Parse the arguments.
    args = ap.parse_args()

    # Do some sanity checks like proper step ranges.
    if args.range > args.max_steps:
        die("bad range > max_steps")

    if args.step < 0:
        die("step should be >= 0")

    # Create a settings object, we will add properties to it
    # later. Also keep an alias "s" to it where we will need short
    # lines.
    settings = Settings()
    s = settings

    # Compose the machine's path and filename.
    machFile = path.join(args.mach_dir, args.machine, "main.machine")

    # Open and load some files then copy the command line arguments
    # into a dict for more prepared and convenient access.
    settings.machineName = args.machine
    settings.step = args.step
    settings.machFile = machFile
    settings.text = fdopen(args.text, "w")
    settings.error = fdopen(args.error, "w")
    settings.warn = fdopen(args.warn, "w")
    settings.files = args.files
    settings.simLogFile = args.sim_log
    settings.format = args.format
    settings.maxSteps = args.max_steps
    settings.range = args.range

    # Try to open the result file descriptor, if it doesn't work, just
    # use the stdout one.
    try:
        settings.result = fdopen(args.result, "w")
    except OSError:
        settings.result = settings.text

    # Here we load the machine's description file.
    try:
        with open(settings.machFile, "r") as fd:
            machine = jsonLoad(fd)
    except:
        die("Error reading machine file.")

    # We set the machine's assembler binary filename, which should be
    # linked to from the bin directory that we added to PATH above. We
    # also set some other stuff from the machine dict for shortness.
    assembler = machine["assembler"]
    singleMem = machine["single_mem"]
    debugInfo = machine["debug_info"]
    simcode = machine["simcode"]
    simulator = machine["simulator"]

    # For shortness.
    files = settings.files

    # We start trying to assemble input program files depending on a
    # number of things. First of all, if there is only a single input
    # file and the machine has only a single memory (most probably and
    # most logically an instruction + data memory), then we check the
    # cases where we either have an assembler or we dont. Otherwise,
    # in the case where there are two input files and a single memory,
    # we concatenate them together, if there are two memories, we use
    # each for program and data respectively.
    if len(settings.files) == 1:
        if singleMem:
            if not assembler:
                tmp = mktempf("_tmp")
                shutil.copyfile(settings.files[0], tmp)
                settings.programProg = tmp
                machine["debug_info"] = False
            else: # if assembler
                retcode = tryAssemble(files[0])

                if retcode == None or retcode != 0:
                    tmp = mktempf("_tmp")
                    shutil.copyfile(files[0], tmp)
                    settings.programProg = tmp
                    machine["debug_info"] = False
        else: # if not singleMem
            if not assembler:
                die("Machine does not have an assembler")
            else:
                tryAssemble(files[0])
    elif len(settings.files) == 2:
        if singleMem:
            datafile, progfile = findDataProgFiles(files)

            with open(datafile, "r") as f1, open(progfile, "r") as f2:
                data = f1.read() + f2.read()

            name = mktempf("_name")

            with open(name, "w") as f:
                f.write(data)

            settings.programProg = name
            machine["debug_info"] = False
        else: # if not singleMem
            datafile, progfile = findDataProgFiles(files)

            tmp = mktempf("_tmp")
            shutil.copyfile(progfile, tmp)
            settings.programProg = tmp

            tmp = mktempf("_tmp")
            shutil.copyfile(datafile, tmp)
            settings.programData = tmp

            machine["debug_info"] = False
    else: # more than two input files given
        die("More than two input files given.")

    # Here we start the simulation, we check whether the machine has a
    # simcode file, if it does, but there is no simulator provided, we
    # check the simcode file's extension and try to figure out what
    # simulator to use, otherwise if a simulator is given, we simply
    # use it. If there is no simcode file and no simulator, then we
    # fail, but if there is a simulator and it's neither veriwell nor
    # vhdl, then we run a standalone simulator.
    if not simcode:
        if not simulator:
            die("No simulator nor simcode (machine) provided.")
        else: # if simulator
            if simulator == "veriwell" or simulator == "vhdl":
                die("No machine for " + simulator + " simulation.")
            else:
                simData = runStandaloneSimulation()
    else: # if simcode
        if not simulator:
            if simcode.endswith(".v"):
                simData = runVeriwellSimulation()
            elif simcode.endswith(".vh"):
                pass
            else:
                die("Cannot determine simulator from simcode extension.")
        else: # if simulator
            if simulator == "veriwell":
                simData = runVeriwellSimulation()
            elif simulator == "vhdl":
                pass
            else:
                die("Unknown simulator given.")
