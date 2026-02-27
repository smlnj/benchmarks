#!/bin/sh
#
# COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# usage: run.sh [ options ] ( benchmark | class ) ...
#

# get the path of the benchmark root directory
here=$(pwd)
bindir=$(dirname "$0")
cd "$bindir/.." || exit 1
root=$(pwd)
programsdir="$root/programs"
outdir="$root/reports"
cd "$here" || exit 1

# sanity check
if [ ! -d "$root/programs" ] ; then
  echo "$cmd: missing programs at $root"
  exit 1
fi

# time stamp and file suffix for output files
timestamp=$(date +"%FT%H:%M:%S")
fileid=$(echo "$timestamp" | tr "T:" "--")

host=$(hostname -s)
cmd=$0
smlcmd=sml
single_file=no
include_basis_opt=""
mode="execution"  # can also be "compile", "gc", and "test"
run_time=yes
logfile="LOG-$fileid"
outfile="REPORT-$fileid.json"
nruns=5
allocsz="1024k"
programs=""
progress=no
controls=""
mlton=no # for future MLton benchmarking support

say() {
  if [ x"$progress" = xyes ] ; then
    echo "$*"
  fi
  echo "$*" >> $logfile
}

usage() {
  echo "usage: $cmd [ options ] ( <benchmark> | <class> ) ..."
  echo "  options:"
  echo "    -h,-help         print this message and exit"
  echo "    -alloc <sz>      specify size of the allocation nursery"
  echo "    -sml <path>      specify the path to the SML/NJ executable"
  echo "    -single-file     measure single-file version of programs"
  echo "    -include-basis   include the Basis source code in a single-file"
  echo "    -compile-time    measure and report compile time"
  echo "    -gc-stats        measure and report allocation and GC counts"
  echo "    -check           check benchmark outputs"
  echo "    -nruns <n>       specify the number of runs per program (default 5)"
  echo "    -o <file>        specify the output file"
  echo "    -log <file>      specify the log file (specify none to disable log)"
  echo "    -progress        print progress messages to stdout"
  echo "    -C<ctl>=<value>  specify SML/NJ compiler flags"
  exit "$1"
}

# remove CM files to get a fresh build (except when in single-file mode)
clean_cm() {
  if [ x"$single_file" = xno ] ; then
    echo "# removing CM files from $1" >> $logfile
    find "$1" \( -name .cm -exec rm -rf {} \; -prune \)
  fi
}

# run the SML command with the benchmark utilities included
run_sml() {
  echo "$smlcmd @SMLquiet @SMLalloc=$allocsz $controls -m ../../util/sources.cm $*" >> $logfile
  $smlcmd @SMLquiet "@SMLalloc=$allocsz" $controls -m ../../util/sources.cm "$@"
}

# measure execution time for a benchmark
measure_execution() {
  prog="$1"
  progdir="$programsdir/$prog"
  if [ x"$single_file" = xyes ] ; then
    "$bindir/make-single-file.sh" -quiet $include_basis_opt "$prog"
    cd "$progdir" || exit 1
    run_sml <<EOF 1>> "$logfile" 2>&1
      use "all.sml";
      Timing.run (Main.name, "runs", $nruns, "$outfile", Timing.timeIt Main.doit);
EOF
    rm -f all.sml
  else
    cd "$progdir" || exit 1
    run_sml sources.cm <<EOF 1>> "$logfile" 2>&1
      Timing.run (Main.name, "runs", $nruns, "$outfile", Timing.timeIt Main.doit);
EOF
  fi
  cd "$here" || exit 1
}

# measure compile time for a benchmark
measure_compile() {
  prog="$1"
  progdir="$programsdir/$prog"
  echo "{ \"program\" : \"$1\"," >> "$outfile"
  echo "  \"compile\" : [" >> "$outfile"
  if [ x"$single_file" = xyes ] ; then
    "$bindir/make-single-file.sh" -quiet $include_basis_opt "$prog"
    cd "$progdir" || exit 1
    for i in $(seq "$nruns"); do
      run_sml <<EOF 1>> "$logfile" 2>&1
        Timing.runOnce ("$outfile", Timing.timeUse "all.sml");
EOF
      if [ "$i" -eq "$nruns" ] ; then
        echo "" >> "$outfile"
      else
        echo "," >> "$outfile"
      fi
    done
  else
    cd "$progdir" || exit 1
    for i in $(seq "$nruns") ; do
      rm -rf "$root/util/.cm" .cm
      run_sml <<EOF 1>> "$logfile" 2>&1
        Timing.runOnce ("$outfile", Timing.timeMake "sources.cm");
EOF
      if [ "$i" -eq "$nruns" ]; then
        echo "" >> "$outfile"
      else
        echo "," >> "$outfile"
      fi
    done
  fi
  echo "] }" >> "$outfile"
}

# measure allocation/GC stats for a benchmark
measure_gc_stats() {
  prog="$1"
  progdir="$programsdir/$prog"
  echo "{ \"program\" : \"$1\"," >> "$outfile"
  if [ x"$single_file" = xyes ] ; then
    "$bindir/make-single-file.sh" -quiet $include_basis_opt "$prog"
    cd "$progdir" || exit 1
    run_sml <<EOF 1>> "$logfile" 2>&1
      use "all.sml";
      Timing.runOnce ("$outfile", Timing.gcStats Main.doit);
EOF
  else
    cd "$progdir" || exit 1
    run_sml <<EOF 1>> "$logfile" 2>&1
      CM.make "sources.cm";
      Timing.runOnce ("$outfile", Timing.gcStats Main.doit);
EOF
  fi
  echo "}" >> "$outfile"
}

# check the program's output
check_program() {
# TODO: use the Timing.testIt function to generate output and then
# compare it with the reference
  exit 1
}

#
# Process command line arguments
#
while [ "$#" != "0" ]; do
  arg=$1
  shift
  case "$arg" in
    -alloc)
      if [ "$#" != 0 ] ; then
        allocsz="$1"; shift
      else
        usage 1
      fi
    ;;
    -check) mode="check" ;;
    -compile-time) mode="compile" ;;
    -gc-stats) mode="gc" ;;
    -h|-help) usage 0 ;;
    -log)
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
      if [ "$#" != 0 ] ; then
        nruns="$1"; shift
      else
        usage 1
      fi
    ;;
    -o)
      if [ "$#" != 0 ] ; then
        outfile="$1"; shift
      else
        usage 1
      fi
    ;;
    -progress) progress="yes" ;;
    -single-file) single_file="yes" ;;
    -include-basis) include_basis_opt="$arg" ;;
    -sml)
      if [ "$#" != 0 ] ; then
        smlcmd="$1"; shift
      else
        usage 1
      fi
    ;;
    -C*)
      if [ x"$controls" = x ] ; then
        controls="$arg"
      else
        controls="$controls $arg"
      fi
    ;;
    -*) echo "$cmd: unknown option '$arg'"; usage 1 ;;
    *)
      if [ -d "$programsdir/$arg" ] ; then
        programs="$programs $arg"
      else
        c_progs=$("$bindir/list-programs.sh" -compact "$arg")
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

# get the version information for the SML command
#
if [ x"$mlton" = xno ] ; then
  smlvers=$($smlcmd @SMLversion)
  case $smlvers in
    sml*) # old-style version string
      smlvers=$(echo "$smlvers" | sed -e 's/sml //')
      ;;
    *) ;;
  esac
  smlsys="SML/NJ"
else
  smlsys="MLton"
  echo "mlton not supported yet"
  exit 1
fi

# ouput meta data to report file
{
    echo "{ \"timestamp\" : \"${timestamp}\","
    echo "  \"sml-command\" : \"${smlcmd}\","
    echo "  \"sml-system\" : \"${smlsys}\","
    echo "  \"sml-version\" : \"${smlvers}\","
    if [ x"$controls" = x ] ; then
      echo "  \"sml-options\" : null,"
    else
      echo "  \"sml-options\" : \"$controls\","
    fi
    if [ x"$single_file" = xyes ] ; then
      echo "  \"single-file\" : true,"
      if [ x"$include_basis_opt" = x ] ; then
        echo "  \"include-basis\" : false,"
      else
        echo "  \"include-basis\" : true,"
      fi
    else
      echo "  \"single-file\" : false,"
    fi
    echo "  \"mode\" : \"${mode}\","
    echo "  \"alloc\" : \"${allocsz}\","
    echo "  \"data\" : ["
} > "$outfile"

# do the measurements (depending on mode)
#
clean_cm util
first=yes
case $mode in
  execution)
    for p in $programs ; do
      say "# $p"
      if [ x"$first" = xyes ]; then
        first=no
      else
        echo "," >> $outfile
      fi
      clean_cm "$programsdir/$p"
      measure_execution "$p"
    done
  ;;
  compile)
    for p in $programs ; do
      say "# $p"
      if [ x"$first" = xyes ]; then
        first=no
      else
        echo "," >> "$outfile"
      fi
      clean_cm "$programsdir/$p"
      measure_compile "$p"
    done
  ;;
  gc)
    for p in $programs ; do
      say "# $p"
      if [ x"$first" = xyes ]; then
        first=no
      else
        echo "," >> "$outfile"
      fi
      clean_cm "$programsdir/$p"
      measure_gc_stats "$p"
    done
  ;;
  check)
    for p in $programs ; do
      say "# $p"
      if [ x"$first" = xyes ]; then
        first=no
      else
        echo "," >> "$outfile"
      fi
      clean_cm "$programsdir/$p"
      check_program "$p"
    done
  ;;
esac

echo "]}" >> "$outfile"
