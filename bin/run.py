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
from datetime import datetime

#========== Benchmark program and class information ==========
#
class ProgramInfo:
  def __init__(self):
    try:
      prog_info_path = os.path.join(progdir, "programs.json")
      with open(prog_info_path, "r", encoding="utf-8") as info_strm:
        prog_info = json.load(info_strm)
        self.classes = prog_info['classes']
        self.programs = prog_info['programs']
        self._class_names = { c['name'] for c in self.classes }
        self._prog_names = { p['name'] for p in self.programs }
    except json.JSONDecodeError as exn:
      print(f'error loading {os.path.join(progdir, "programs.json")}\n')
      print('  ' + exn.msg)
      sys.exit()

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

#========== Initialization ==========
#

# absolute path to `benchmarks/bin` directory
#
bindir = os.path.dirname(os.path.abspath(__file__))

# absolute path to `benchmarks` directory
#
rootdir = os.path.normpath(os.path.join(bindir, ".."))
progdir = os.path.join(rootdir, "programs")
resultdir = os.path.join(rootdir, "results")

# default SML command
#
sml_cmd="sml"

# timestamp suffix for output files
#
timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")

# load the benchmark info from the JSON file
#
prog_info = ProgramInfo()

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
def parse_program_list(args):
  if (len(args.programs) == 0):
    # no benchmarks is treated as all benchmarks
    return (prog_info.program_names())
  else:
    progs=set()
    classes=[]
    for name in args.programs:
      if (prog_info.is_class(name)):
        classes.append(name)
      elif (prog_info.is_program(name)):
        progs.add(name)
      else:
        print("error: \"" + name + "\" is unknown")
        sys.exit()
    return sorted(progs.union(get_benchmark_list(classes)))

# remove CM files to get a fresh build (except when in single-file mode)
#
def clean_cm(args):
  print("TODO: clean_cm")

#========== Functions for command-line argument processing ==========
#

# function to add the standard arguments that control execution to a
# command argument subparser
#
def add_exec_args(subparser,includeNRuns):
  subparser.add_argument(
    "-alloc",
    nargs=1,
    default="512k",
    help="specify size of the allocation nursery (default 512k)")
  subparser.add_argument(
    "-sml",
    nargs=1,
    default="sml",
    help="specify the path to the SML/NJ executable")
  if (includeNRuns):
    subparser.add_argument(
      "-nruns",
      type=int,
      default=5,
      help="specify the number of measurements per program (default 5)")
  subparser.add_argument(
    "-single-file",
    action="store_true",
    help="compile a single-file version of the programs")
  subparser.add_argument(
    "-include-basis",
    action="store_true",
    help="include a copy of the Basis source code in the compile")
  subparser.add_argument(
    "-log",
    nargs=1,
    default=f"LOG-{timestamp}",
    help="specify the name of the logfile (none to disable)")
  subparser.add_argument(
    "-d", "-result-dir",
    default=resultdir,
    help=f"set the result directory (default: {resultdir})")
  subparser.add_argument(
    "-o", "-output",
    default=f"REPORT-{timestamp}.json",
    help="set the name of the result JSON file")
  subparser.add_argument(
    "-progress",
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
  add_exec_args(run, True)
  add_programs_arg(run, "the benchmark programs and/or classes to run")
  # time execution
  gc = subparsers.add_parser("gc")
  add_exec_args(gc, False)
  add_programs_arg(gc, "the benchmark programs and/or classes to measure")
  # time compile times
  compile = subparsers.add_parser("compile")
  add_exec_args(compile, True)
  add_programs_arg(compile, "the benchmark programs and/or classes to compile")
  # check results
  check = subparsers.add_parser("check")
  add_exec_args(check, False)
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

#========== The 'run' command ==========
#
def do_run(args):
  progs = parse_program_list(args)
  print(progs)

#========== The 'gc' command ==========
#
def do_gc(args):
  progs = parse_program_list(args)
  print(progs)

#========== The 'compile' command ==========
#
def do_compile(args):
  progs = parse_program_list(args)
  print(progs)

#========== The 'check' command ==========
#
def do_check(args):
  progs = parse_program_list(args)
  print(progs)

#========== The 'list' command ==========
#
def do_list(args):
  if (args.classes):
    # list the classes
    objs = prog_info.classes
    title = "Benchmark classes:"
  else:
    # list the programs
    objs = prog_info.programs
    title = "Benchmarks:"
  if (args.compact):
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
args = create_arg_parser().parse_args()

# postprocess the arguments
#
if (args.cmd == 'run'):
  print("# run")
  do_run(args)
elif (args.cmd == 'gc'):
  print("# gc")
  do_gc(args)
elif (args.cmd == 'compile'):
  print("# compile")
  do_compile(args)
elif (args.cmd == 'check'):
  print("# check")
  do_check(args)
elif (args.cmd == 'list'):
  print("# list")
  do_list(args)
