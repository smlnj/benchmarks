#!/bin/sh
#
# COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
# All rights reserved.
#
# usage: make-single-file.sh [options] <benchmark> ...
#
# create the `all.sml` version of a benchmark
#

cmd="make-single-file.sh"
mlton=no
quiet=no
include_basis=no

# get the path of the benchmark root directory
here=$(pwd)
bindir=$(dirname "$0")
cd "$bindir/.."
root=$(pwd)
cd $here

# sanity check
if [ ! -d $root/programs ] ; then
  echo "$cmd: missing programs at $root"
  exit 1
fi

say() {
  if [ x"$quiet" = xno ] ; then
    echo "$@"
  fi
}

usage() {
  echo "usage: $cmd [ options ] <benchmark> ..."
  echo "  options:"
  echo "    -h,-help        print this message and exit"
  echo "    -mlton          create a file that can be compiled by MLton"
  echo "    -quiet          run in quiet mode"
  echo "    -include-basis  include the Basis source code in a single-file"
  exit $1
}

# copy srcDir src dst
copy() {
  srcDir="$1"
  src="$2"
  dst="$3"
  echo "(******************** $src ********************)" >> "$dst"
  cat "$srcDir/$src" >> "$dst"
}

# mkall bmark
mkall () {
  bmark="$1"
  bmarkDir="$root/programs/$bmark"
  if [ ! -d "$bmarkDir" ] ; then
    echo "$cmd: missing benchmark directory '$bmarkDir'"
    exit 1
  fi
  say "***** $bmark"
  if [ x"$mlton" = xyes ] ; then
    out="$bmarkDir/all-mlton.sml"
  else
    out="$bmarkDir/all.sml"
  fi
  echo "(* $out -- all sources for $bmark *)" > $out
  if [ x"$mlton" != xyes ] ; then
    # SML/NJ allows signatures in local, which is an extension, but
    # MLton follows the Definition and does not
    echo "local" >> $out
  fi

  copy "$root/util" bmark.sig "$out"

  if [ -f $bmarkDir/FILES ] ; then
    # copy the listed source files in FILES
    for srcFile in $(cat $bmarkDir/FILES) ; do
      if [ x"$include_basis" = xyes ] ; then
        copy "$bmarkDir" "$srcFile" "$out"
      else
        # filter out basis modules
        case $srcFile in
          ../common/*) ;;
          *) copy "$bmarkDir" "$srcFile" "$out" ;;
        esac
      fi
    done
  fi

  if [ x"$mlton" = xyes ] ; then
    cat "$bmarkDir/main.sml" >> $out
  else
    echo "in" >> $out
    cat "$bmarkDir/main.sml" >> $out
    echo "end; (* local *)" >> $out
  fi

  if [ x"$mlton" = xyes ] ; then
    echo "val _ = (case CommandLine.arguments()" >> $out
    echo "         of \"-test\"::_ => Main.testit TextIO.stdOut" >> $out
    echo "          | _ => Main.doit()" >> $out
    echo "        (* end case *));" >> $out
  fi
}

#
# Process command line arguments
#
while [ "$#" != "0" ]; do
  arg=$1
  case "$arg" in
    -mlton) shift; mlton=yes ;;
    -quiet) shift; quiet=yes ;;
    -include-basis) shift; include_basis=yes ;;
    -h|-help) usage 0 ;;
    -*) echo "$cmd: unknown option '$arg'"; usage 1 ;;
    *) break ;;
  esac
done

bmarks=$@
if [ x"$bmarks" = x ] ; then
  usage 1
fi

for b in $bmarks ; do
  mkall $b
done

exit 0
