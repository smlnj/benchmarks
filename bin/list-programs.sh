#!/bin/sh
#
# COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
# All rights reserved.
#
# usage: list-programs.sh [ options ] <class> ...
#   options:
#       -h,-help        print this message and exit
#       -compact        only print the benchmark names
#
# This script lists the benchmarks in various classes.  With no arguments, it
# lists each class and the programs contained in that class
#
# TODO: make this data driven; i.e., define a JSON file with benchmark
# descriptions and tags and extract the info from it

cmd="list-programs.sh"
compact=no
all_programs=no

CLASSES="FP LIST ARRAY BIGNUM FUNCTOR CLASSIC SMLNJ MANTICORE MLTON LARCENY"

usage() {
  echo "usage: $cmd [ options ] <class> ..."
  echo "  options:"
  echo "    -h,-help      print this message and exit"
  echo "    -compact      generate sorted list of programs"
  echo "  classes: $CLASSES"
  exit $1
}

# display <class> <programs> <description>
display() {
  echo "$1: $3"
  echo "  $2"
}

# sort_items ...
sort_items() {
  tmpfile=tmp$$
  for f in $@ ; do
    echo $f >> $tmpfile
  done
  sort -u $tmpfile
  rm $tmpfile
}

ALL="\
  black-scholes \
  count-graphs \
  cml-sieve \
  fft \
  knuth-bendix \
  lexgen \
  life \
  logic \
  mandelbrot \
  mandelbrot-rat \
  mazefun \
  mc-ray \
  minimax \
  mlyacc \
  nbody \
  nucleic \
  pingpong \
  plclub-ray \
  ratio-regions \
  sat \
  stream-sieve \
  simple \
  smith-nf \
  tsp \
  twenty-four \
  vliw \
  "
BROKEN="\
  barnes-hut \
  boyer \
  delta-blue \
  dlx \
  "

FP="barnes-hut black-scholes fft mandelbrot mc-ray nbody nucleic plclub-ray smith-nf tsp"
FP_DESC="programs that make significant use of floating-point arithmetic"

LIST="count-graphs life minimax"
LIST_DESC="programs that make significant use of lists"

ARRAY="fft minimax simple smith-nf"
ARRAY_DESC="programs that make significant use of arrays or vectors"

BIGNUM="smith-nf"
BIGNUM_DESC="programs that make significant use of the IntInf.int type"

CML="cml-sieve"
CML_DESC="multi-threaded message-passing programs"

FUNCTOR="barnes-hut dlx mlyacc simple"
FUNCTOR_DESC="programs that are assembled from functor applications"

CLASSIC="lexgen life mlyacc simple vliw"
CLASSIC_DESC="programs from the original SML/NJ benchmarking"

SMLNJ="$CLASSIC boyer fft knuth-bendix logic mandelbrot ray tsp"
SMLNJ_DESC="programs from the previous SML/NJ bencmark suite"

MANTICORE="cml-sieve minimax"
MANTICORE_DESC="programs from the Manticore benchmark suite"

MLTON="dlx ratio-regions smith-nf"
MLTON_DESC="programs from the MLton bencmark suite"

LARCENY="mazefun"
LARCENY_DESC="programs from the Larceny bencmark suite"

SCHEME="$LARCENY sat stream-sieve"
SCHEME_DESC="programs ported from Scheme and Racket (includes the Larceny bencmarks)"

classes=""

#
# Process command line arguments
#
while [ "$#" != "0" ]; do
  arg=$1
  case "$arg" in
    -compact) shift; compact=yes ;;
    -h|-help) usage 0 ;;
    -*) echo "$cmd: unknown option '$arg'"; usage 1 ;;
    *)
      shift
      case "$arg" in
        ALL|all) all_programs=yes ;;
        FP|fp) classes="$classes FP" ;;
        LIST|list) classes="$classes LIST" ;;
        ARRAY|array) classes="$classes ARRAY" ;;
        BIGNUM|bignum) classes="$classes BIGNUM" ;;
        CML|cml) classes="$classes CML" ;;
        FUNCTOR|functor) classes="$classes FUNCTOR" ;;
        CLASSIC|classic) classes="$classes CLASSIC" ;;
        SMLNJ|smlnj) classes="$classes SMLNJ" ;;
        MANTICORE|manticore) classes="$classes MANTICORE" ;;
        MLTON|mlton) classes="$classes MLTON" ;;
        LARCENY|larceny) classes="$classes LARCENY" ;;
        SCHEME|scheme) classes="$classes SCHEME" ;;
        *) echo "$cmd: unknown benchmark class '$arg'"
          exit 1
          ;;
      esac ;;
  esac
done

if [ x"$classes" = x ] ; then
  all_programs=yes
fi

if [ x"$all_programs" = xyes ] ; then
  if [ x"$compact" = xyes ] ; then
    echo $ALL
  else
    programs=$(echo $ALL)
    display "all" "$programs" "all programs"
  fi
else
  if [ x"$compact" = xyes ] ; then
    programs=""
    for c in $classes ; do
      c_progs=$(eval echo "$"$c)
      programs="$programs $c_progs"
    done
    programs=$(sort_items $programs)
    echo $programs
  else
    for c in $classes ; do
      programs=$(eval echo "$"$c)
      desc=$(eval echo "$"$c"_DESC")
      display "$c" "$programs" "$desc"
    done
  fi
fi

exit 0
