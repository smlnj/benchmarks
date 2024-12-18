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
mode="execution"  # can also be "compile", "gc", and "test"
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
  echo "    -check           check benchmark outputs"
  echo "    -nruns <n>       specify the number of runs per program"
  echo "    -outfile <file>  specify the output file"
  echo "    -log <file>      specify the log file (specify none to disable log)"
  exit $1
}

# measure execution time for a benchmark
measure_execution() {
  prog=$1
  progdir="$programsdir/$prog"
  if [ x"$single_file"=xyes ] ; then
    $bindir/make-single-file.sh -quiet $prog
    cd $progdir
    $smlcmd @SMLquiet @SMLalloc=$allocsz -m ../../util/sources.cm <<EOF 1>> $logfile 2>&1
      use "all.sml";
      Timing.run (Main.name, "runs", $nruns, "$outfile", Timing.timeIt Main.doit);
EOF
    rm -f all.sml
  else
    cd $progdir
    $smlcmd @SMLquiet @SMLalloc=$allocsz -m ../../util/sources.cm sources.cm <<EOF 1>> $logfile 2>&1
      Timing.run (Main.name, "runs", $nruns, "$outfile", Timing.timeIt Main.doit);
EOF
  fi
  cd $here
}

# measure compile time for a benchmark
measure_compile() {
  prog=$1
  progdir="$programsdir/$prog"
  echo "{ \"program\" : \"$1\"," >> $outfile
  echo "  \"compile\" : [" >> $outfile
  if [ x"$single_file"=xyes ] ; then
    $bindir/make-single-file.sh -quiet $prog
    cd $progdir
    for (( i = 0 ; $i < $nruns ; ++i )) ; do
      $smlcmd @SMLquiet @SMLalloc=$allocsz -m ../../util/sources.cm <<EOF 1>> $logfile 2>&1
        Timing.runOnce ("$outfile", Timing.timeUse "all.sml");
EOF
      if (( $i == $nruns-1 )) ; then
        echo "" >> $outfile
      else
        echo "," >> $outfile
      fi
    done
  else
    cd $progdir
    for (( i = 0 ; $i < $nruns ; ++i )) ; do
      rm -rf $root/util/.cm .cm
      $smlcmd @SMLquiet @SMLalloc=$allocsz -m ../../util/sources.cm <<EOF 1>> $logfile 2>&1
        Timing.runOnce ("$outfile", Timing.timeMake "sources.cm");
EOF
      if (( $i == $nruns-1 )) ; then
        echo "" >> $outfile
      else
        echo "," >> $outfile
      fi
    done
  fi
  echo "] }" >> $outfile
  exit 1
}

# measure allocation/GC stats for a benchmark
measure_gc_stats() {
  exit 1
}

# check the program's output
check_program() {
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
    -check) shift; mode="check" ;;
    -compile-time) shift; mode="compile" ;;
    -gc_stats) shift; mode="gc" ;;
    -h|-help) usage 0 ;;
    -log)
      shift
      if [ "$#" != 0 ] ; then
        if [ x"$1" = xnone ] ; then
          logfile="/dev/null"
        else
          logfile="$1"
        fi
        shift
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
        c_progs=$($bindir/list-programs.sh -compact $arg)
        if [ "$?" != 0 ] ; then
          usage 1
        fi
        programs="$programs $c_progs"
      fi
      ;;
  esac
done

if [ x"$programs" = x ] ; then
  usage 1
fi

# set log file
case $logfile in
  /*) ;; # absolute path
  *) logfile="$here/$logfile" ;;
esac

# set output file
case $outfile in
  /*) ;; # absolute path
  *) outfile="$outdir/$outfile" ;;
esac

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
  check)
    for p in $programs ; do
      say "***** $p"
      check_program $p
    done
  ;;
esac
