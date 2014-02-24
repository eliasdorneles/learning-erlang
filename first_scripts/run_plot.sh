#!/bin/bash

if [ x$3 == "x" ]; then
    echo "Usage: `basename $0` NUM_PROCS_START NUM_PROCS_END [NPROCS_STEP]"
    exit 1
fi

set -e

nprocs_start=$1
nprocs_end=$2
nprocs_step=${3:-1000}

tmpfile=`mktemp`
erl -boot start_clean -noshell -s procreation plot \
    $nprocs_start $nprocs_end $nprocs_step -s init stop > $tmpfile

gnuplot -e """
set xlabel 'Number of processes';
set ylabel 'microseconds';
plot '$tmpfile' using 1:2 title 'CPU time' with linespoints, \
    '$tmpfile' using 1:3 title 'Ellapsed time' with linespoints;
pause -1;
"""
