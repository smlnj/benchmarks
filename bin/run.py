#!/usr/bin/env python3
#
# COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# usage: run.py [ options ] ( <benchmark> | <class> ) ...
#

import argparse
import json
import os
import sys
import subprocess
import shutil
from datetime import datetime

# helper function for loading data from a JSON file
#
def load_json(path):
  try:
    with open(path, "r", encoding="utf-8") as in_strm:
      contents = json.load(in_strm)
  except json.JSONDecodeError as exn:
    print(f'error loading {path}\n')
    print('  ' + exn.msg)
    sys.exit()
  return (contents)

#========== Benchmark program and class information ==========
#
class ProgramInfo:
  def __init__(self):
    prog_info = load_json(os.path.join(progdir, "programs.json"))
    self.classes = prog_info['classes']
    self.programs = prog_info['programs']
    self._class_names = { c['name'] for c in self.classes }
    self._prog_names = { p['name'] for p in self.programs }

#json.JSONDecodeError(msg, doc, pos)

  def is_class(self, name):
    return (name in self._class_names)

  def is_program(self, name):
    return (name in self._prog_names)

  def program_names(self):
    return sorted(self._prog_names)

  def get_class(self,name):
    if (name in self._class_names):
      for cls in self.classes:
        if (name == cls['name']):
          return (cls)
    else:
      return None

  def get_program(self,name):
    if (name in self._prog_names):
      for p in self.programs:
        if (name == p['name']):
          return (p)
    else:
      return None

#========== Logging compiler messages, etc ==========
#
class LogFile:
  def __init__(self, file):
    self._name = file
    if (file == "none"):
      self._outS = None
    elif (file == "-"):
      self._outS = sys.stdout
    else:
      self._outS = open(file, "a", buffering=1)

  def __enter__(self):
    return (self)

  def __exit__(self, exc_type, exc_val, exc_tb):
    if (self._outS and (self._name != "-")):
      self._outS.close()
    return False

  def say(self, *args):
    if (self._outS):
      self._outS.write("".join(list(args)))

#========== Initialization ==========
#

# holder for command-line arguments
#
cmdln_args = argparse.Namespace()

# absolute path to `benchmarks/bin` directory
#
bindir = os.path.dirname(os.path.abspath(__file__))

# absolute path to `benchmarks` directory
#
rootdir = os.path.normpath(os.path.join(bindir, ".."))
progdir = os.path.join(rootdir, "programs")
resultdir = os.path.join(rootdir, "reports")

# default SML command
#
sml_cmd = shutil.which("sml")
sml_version = ""
sml_system = "SML/NJ" # eventually other systems, such as mlton or polyml

# timestamp suffix for output files
#
timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")

# load the benchmark info from the JSON file
#
prog_info = ProgramInfo()

# default timeout (5 minutes)
#
timeout = 5 * 60

#========== Utility functions ==========
#

# construct a list of benchmark names from a list of benchmark classes
#
def get_benchmark_list(class_list):
  # convert the list to a set classes
  cls_set = set(class_list)
  progs=[]
  for p in prog_info.programs:
    for cls in p['classes']:
      if (cls in cls_set):
        progs.append(p['name'])
        break
  return (progs)

# given a list of programs and/or benchmark classes, return a sorted list of programs
#
def parse_program_list():
  if (len(cmdln_args.programs) == 0):
    # no benchmarks is treated as all benchmarks
    return (prog_info.program_names())
  else:
    progs=set()
    classes=[]
    for name in cmdln_args.programs:
      if (prog_info.is_class(name)):
        classes.append(name)
      elif (prog_info.is_program(name)):
        progs.add(name)
      else:
        print("error: \"" + name + "\" is unknown")
        sys.exit()
    return sorted(progs.union(get_benchmark_list(classes)))

# determine the path to the sml command and get the version tag
#
def resolve_sml_cmd():
  if (cmdln_args.sml):
    sml_cmd = os.path.realpath(cmdln_args.sml)
  else:
    sml_cmd = shutil.which("sml")
  # verify that the sml command exists and is executable
#TODO
  # get the version
  process = subprocess.run([sml_cmd, "@SMLversion"], text=True, capture_output=True)
  sml_version=process.stdout.strip().removeprefix("sml ")

# remove CM files to get a fresh build (except when in single-file mode)
#
def clean_cm():
  print("TODO: clean_cm")

# concatenate the source files of a benchmark to create
# the single source file "all.sml".
#
def make_single_file(prog, includeBasis):
  print("TODO: make_single_file")

# run an sml command
#
def run_sml_cmd(*sml_args, program, input, logf=None):
  cmd = [
      sml_cmd,
      "@SMLquiet",
      f"@SMLalloc={cmdln_args.alloc}",
      "-Ccm.verbose=false",
      "-m", "../../util/sources.cm",
    ] + list(sml_args)
  if (logf):
    logf.say("## ", " ".join(cmd), "\n")
  process = subprocess.run(
    cmd,
    input=input,
    text=True,
    stdout=subprocess.PIPE,
    stderr=subprocess.STDOUT,
    check=True,
    cwd=os.path.join(progdir, program),
    timeout=timeout)
  if (logf):
    logf.say(process.stdout)

#========== Functions for command-line argument processing ==========
#

# function to add the standard arguments that control execution to a
# command argument subparser
#
def add_exec_args(subparser,name,includeNRuns,includeOutput):
  subparser.add_argument(
    "-a", "--alloc",
    nargs=1,
    default="512k",
    help="specify size of the allocation nursery (default 512k)")
  subparser.add_argument(
    "--sml",
    nargs=1,
    default=sml_cmd,
    help=f"specify the path to the SML/NJ executable (default {sml_cmd})")
  if (includeNRuns):
    subparser.add_argument(
      "-n", "--nruns",
      type=int,
      default=5,
      help="specify the number of measurements per program (default 5)")
  subparser.add_argument(
    "--single-file",
    action="store_true",
    help="compile a single-file version of the programs")
  subparser.add_argument(
    "--include-basis",
    action="store_true",
    help="include a copy of the Basis source code in the compile")
  subparser.add_argument(
    "--log",
    nargs=1,
    default=f"LOG-{timestamp}",
    help="specify the name of the logfile (none to disable)")
  subparser.add_argument(
    "-d", "--result-dir",
    nargs=1,
    default=resultdir,
    help=f"set the result directory (default: {resultdir})")
  if (includeOutput):
    subparser.add_argument(
      "-o", "--output",
      nargs=1,
      default=f"{name}-{timestamp}.json",
      help="set the name of the result JSON file")
  subparser.add_argument(
    "--progress",
    action="store_true",
    help="print progress messages to stdout")
  subparser.add_argument(
    "-C",
    action="append",
    metavar="<ctl>=<value>",
    help="specify SML/NJ compiler flags")

# add the additional programs argument to a command argument subparser
#
def add_programs_arg(subparser, help):
  subparser.add_argument(
    "programs",
    nargs="*",
    action="extend",
    help="the benchmark programs and/or classes to run")

def create_arg_parser():
  parser = argparse.ArgumentParser(
    prog="run.py",
    description="Script for running SML benchmarks")
  subparsers=parser.add_subparsers(
    title="commands",
    required=True,
    dest="cmd")
  # time execution
  run = subparsers.add_parser("run")
  add_exec_args(run, "run", True, True)
  add_programs_arg(run, "the benchmark programs and/or classes to run")
  # time execution
  gc = subparsers.add_parser("gc")
  add_exec_args(gc, "gc", False, True)
  add_programs_arg(gc, "the benchmark programs and/or classes to measure")
  # time compile times
  compile = subparsers.add_parser("compile")
  add_exec_args(compile, "compile", True, True)
  add_programs_arg(compile, "the benchmark programs and/or classes to compile")
  # check results
  check = subparsers.add_parser("check")
  add_exec_args(check, "", False, False)
  add_programs_arg(check, "the benchmark programs and/or classes to check")
  # list the benchmark programs
  list = subparsers.add_parser("list")
  list.add_argument(
    "-compact",
    action="store_true",
    help="produce a compact listing on a single line")
  list.add_argument(
    "-classes",
    action="store_true",
    help="list the benchmark classes")
  return (parser)

#==========

# standard set up for commands that run the SML/NJ compiler; returns the list of
# programs to run/compile/check
#
def sml_init():
  resolve_sml_cmd()
  clean_cm()
  return (parse_program_list())

# create the top-level dictionary for the data
#
def make_data_dict():
  return {
      "timestamp" : timestamp,
      "sml-command" : sml_cmd,
      "sml-system" : sml_system,
      "sml-options" : None,     # TODO
      "single-file" : cmdln_args.single_file,
      "include-basis" : cmdln_args.include_basis,
      "mode" : cmdln_args.cmd,
      "alloc" : cmdln_args.alloc,
      "data" : []
    }

# output the results as a JSON file
#
def output_results(results):
  try:
    os.makedirs(cmdln_args.result_dir, exist_ok=True)
    result_filename = os.path.join(cmdln_args.result_dir, cmdln_args.output)
    with open(result_filename, "w") as result_file:
      json.dump(results, result_file, indent=2)
  except Exception as e:
    print(json.dumps(results, indent=2))
    raise

#========== The 'run' command ==========
#
def do_run(logf):
  progs = sml_init()
  results = make_data_dict()
  for prog in progs:
    tmp_filename = os.path.join(progdir, prog, "".join(["results-", timestamp, ".json"]))
    logf.say("# running ", prog, "\n")
    if (cmdln_args.progress):
      print("running ", prog)
    if (cmdln_args.single_file):
      make_single_file(prog, cmdln_args.include_basis)
      run_sml_cmd(program=prog, input=f"""\
          use "all.sml";
          Timing.run (Main.name, "runs", {cmdln_args.nruns}, "{tmp_filename}", Timing.timeIt Main.doit);
        """)
    else:
      run_sml_cmd("sources.cm", program=prog, input=f"""\
          Timing.run (Main.name, "runs", {cmdln_args.nruns}, "{tmp_filename}", Timing.timeIt Main.doit);
        """)
    # input the results from the tmp file
    results["data"].append(load_json(tmp_filename))
    os.remove(tmp_filename)
  output_results(results)

#========== The 'gc' command ==========
#
def do_gc(logf):
  progs = sml_init()
  result = make_data_dict()
  print(progs)

#========== The 'compile' command ==========
#
def do_compile(logf):
  progs = sml_init()
  result = make_data_dict()
  print(progs)

#========== The 'check' command ==========
#
def do_check(logf):
  progs = sml_init()
  print(progs)

#========== The 'list' command ==========
#
def do_list(logf):
  if (cmdln_args.classes):
    # list the classes
    objs = prog_info.classes
    title = "Benchmark classes:"
  else:
    # list the programs
    objs = prog_info.programs
    title = "Benchmarks:"
  if (cmdln_args.compact):
    # list the names on a single line
    names = []
    for obj in objs:
      names.append(obj['name'])
    print (" ".join(names))
  else:
    # list the names with their descriptions
    print (title)
    for obj in objs:
      print("  " + obj['name'] + ":")
      if (obj['description'] != ""):
        print("    "+obj['description'])
      else:
        print("    <no description>")

# set up the command-line option parsing
#
create_arg_parser().parse_args(namespace=cmdln_args)

with LogFile(cmdln_args.log) as logf:
  if (cmdln_args.cmd == 'run'):
    logf.say("# run\n")
    do_run(logf)
  elif (cmdln_args.cmd == 'gc'):
    logf.say("# gc\n")
    do_gc(logf)
  elif (cmdln_args.cmd == 'compile'):
    logf.say("# compile\n")
    do_compile(logf)
  elif (cmdln_args.cmd == 'check'):
    logf.say("# check\n")
    do_check(logf)
  elif (cmdln_args.cmd == 'list'):
    logf.say("# list\n")
    do_list(logf)
