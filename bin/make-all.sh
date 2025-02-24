#!/bin/sh

here=$(pwd)
bindir=$(dirname "$0")
cd "$bindir/.."
rootdir=$(pwd)
progs=$($bindir/list-programs.sh -compact $arg)
outdir="$rootdir/single-file"

mkdir -p "$outdir"
mkdir -p "$outdir/DATA"

for prog in $progs; do
  $bindir/make-single-file.sh -quiet $prog
  mv "$rootdir/programs/$prog/all.sml" "$outdir/$prog.sml"
  if [[ -d "$rootdir/programs/$prog/DATA" ]]; then
    cp -r "$rootdir/programs/$prog/DATA/"* "$outdir/DATA"
  fi
done
