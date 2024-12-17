#!/bin/sh
#
# COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
# All rights reserved.
#
# usage: run.sh [ options ] ( benchmark | class ) ...
#

# get the path of the benchmark root directory
here=$(pwd)
bindir=$(dirname "$0")
cd "$bindir/.."
root=$(pwd)
programsdir="$root/programs"
outdir="$root/reports"
cd $here

# sanity check
if [ ! -d $root/programs ] ; then
  echo "$cmd: missing programs at $root"
  exit 1
fi

# time stamp for output files
timestamp=$(date +"%F-%H-%M-%S")
host=$(hostname -s)

cmd=$0
smlcmd=sml
single_file=no
mode="execution"  # can also be "compile" and "gc"
run_time=yes
logfile="LOG-$timestamp"
outfile="REPORT-$timestamp.json"
nruns=5
allocsz="512k"
programs=""
verbose=yes

say() {
  if [ x"$verbose" = xno ] ; then
    echo $@
  fi
}

usage() {
  echo "usage: $cmd [ options ] ( <benchmark> | <class> ) ..."
  echo "  options:"
  echo "    -h,-help         print this message and exit"
  echo "    -alloc <sz>      specify size of the allocation nursery"
  echo "    -sml <path>      specify the path to the SML/NJ executable"
  echo "    -single-file     measure single-file version of programs"
  echo "    -compile-time    measure and report compile time"
  echo "    -gc-stats        measure and report allocation and GC counts"
  echo "    -nruns <n>       specify the number of runs per program"
  echo "    -outfile <file>  specify the output file"
  echo "    -log <file>      specify the log file"
  exit $1
}

# measure execution time for a benchmark
measure_execution() {
  prog=$1
  progdir="$programsdir/$prog"
  if [ x"$single_file"=xyes ] ; then
    $bindir/make-single-file.sh -quiet $prog
    cd $progdir
    $smlcmd @SMLquiet @SMLalloc=$allocsz -m ../../util/sources.cm <<EOF 1>> $here/$logfile 2>&1
      use "all.sml";
      Timing.run (Main.name, "runs", $nruns, "$outdir/$outfile", Timing.timeIt Main.doit);
EOF
    rm -f all.sml
  else
    cd $progdir
    $smlcmd @SMLquiet @SMLalloc=$allocsz -m ../../util/sources.cm sources.cm <<EOF 1>> $here/$logfile 2>&1
      Timing.run (Main.name, "runs", $nruns, "$outdir/$outfile", Timing.timeIt Main.doit);
EOF
  fi
  cd $here
}

# measure compile time for a benchmark
measure_compile() {
  exit 1
}

# measure allocation/GC stats for a benchmark
measure_gc_stats() {
  exit 1
}

#
# Process command line arguments
#
while [ "$#" != "0" ]; do
  arg=$1
  case "$arg" in
    -alloc)
      shift
      if [ "$#" != 0 ] ; then
        allocsz="$1"; shift
      else
        usage 1
      fi
    ;;
    -compile-time) shift; mode="compile" ;;
    -gc_stats) shift; mode="gc" ;;
    -h|-help) usage 0 ;;
    -log)
      shift
      if [ "$#" != 0 ] ; then
        logfile="$1"; shift
      else
        usage 1
      fi
    ;;
    -nruns)
      shift;
      if [ "$#" != 0 ] ; then
        nruns="$1"; shift
      else
        usage 1
      fi
    ;;
    -o)
      shift
      if [ "$#" != 0 ] ; then
        outfile="$1"; shift
      else
        usage 1
      fi
    ;;
    -single-file) shift; single_file="yes" ;;
    -sml)
      shift
      if [ "$#" != 0 ] ; then
        smlcmd="$1"; shift
      else
        usage 1
      fi
    ;;
    -*) echo "$cmd: unknown option '$arg'"; usage 1 ;;
    *)
      shift
      if [ -d "$programsdir/$arg" ] ; then
        programs="$programs $arg"
      else
# handle potential benchmark class
usage 1
      fi
      ;;
  esac
done

if [ x"$programs" = x ] ; then
  usage 1
fi

# do the measurements (depending on mode)
#
case $mode in
  execution)
    for p in $programs ; do
      say "***** $p"
      measure_execution $p
    done
  ;;
  compile)
    for p in $programs ; do
      say "***** $p"
      measure_compile $p
    done
  ;;
  gc)
    for p in $programs ; do
      say "***** $p"
      measure_gc_stats $p
    done
  ;;
esac
