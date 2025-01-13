#!/bin/sh
#
# COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
# All rights reserved.
#
# usage: cloc.sh [ options ] <benchmark> ...
#

cmd="cloc.sh"
outfile=""
outformat=""

# get the path of the benchmark root directory
here=$(pwd)
bindir=$(dirname "$0")
cd "$bindir/.."
root=$(pwd)
cd $here

usage() {
  echo "usage: $cmd [ options ] <benchmark> ..."
  echo "  options:"
  echo "    -h,-help      print this message and exit"
  echo "    -json         generate output in JSON format"
  echo "    -md           generate output in Markdown table format"
  echo "    -o <file>     direct output to a file instead out stdout"
  exit $1
}

#
# Process command line arguments
#
while [ "$#" != "0" ]; do
  arg=$1;
  case "$arg" in
    -h|-help) usage 0 ;;
    -json) shift; outformat="-json" ;;
    -md) shift; outformat="-md" ;;
    -o)
      shift
      if [ "$#" != 0 ] ; then
        outfile="$1"; shift
      else
        usage 1
      fi
    ;;
    -*) echo "$cmd: unknown option '$arg'"; usage 1 ;;
    *) break ;;
  esac
done

bmarks=$@
if [ x"$bmarks" = x ] ; then
  usage 1
fi

# create the files to be counted in a temporary directory to make
# cleanup easier
tmpdir=$$tmp
mkdir $tmpdir

for b in $bmarks ; do
  bmarkdir=programs/$b
  dst="$tmpdir/$b"
  # check the validity of the benchmark
  if [ ! -d "$bmarkdir" ] ; then
    rm -rf $tmpdir
    echo "$cmd: '$bmarkdir' does not exist"
    echo 1
  fi
  # create a single file from the benchmark sources
  if [ -r "$bmarkdir/FILES" ] ; then
    for f in $(cat "$bmarkdir/FILES") main.sml ; do
      cat $bmarkdir/$f >> $dst
    done
  else
    cp $bmarkdir/main.sml $dst
  fi
  echo "$b" >> $tmpdir/FILES
done

# command-line arguments to the cloc command
clocargs="--hide-rate --by-file --skip-uniqueness --list-file=FILES --quiet"
if [ x"$outformat" != x ] ; then
  clocargs="$clocargs $outformat"
fi
if [ x"$outfile" != x ] ; then
  case $outfile in
    /*) ;;
    *) outfile="../$outfile"
  esac
  clocargs="$clocargs --report-file=$outfile"
fi

# now we can count the lines
cd $tmpdir
cloc --force-lang="Standard ML" $clocargs

cd ..
rm -rf $tmpdir

exit 0
