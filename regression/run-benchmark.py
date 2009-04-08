#!/usr/bin/env python
"""
This is the benchmark driver program. It allows us to run the benchmarks
in the Ex-SML project.
"""
from __future__ import with_statement

import sys
import os
import timeit

from optparse import OptionParser

class NoSuchBench(Exception):
    """Exception used for nonexistant benchmarks"""
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return repr(self.value)

# Counts from benchmarks from MLton
## The benchmark counts is a name, a number of iterations for the fast run
## and a number of iterations for the slow run. Some of these are spensive to
## run, so take it easy and wait it out.
bench_counts = { "barnes-hut" : (4096, 1024),
                 "boyer" : (3000, 1000),
                 "checksum" : (1500, 150),
                 "count-graphs" : (3, 1),
                 "DLXSimulator" : (50, 15),
                 "fft" : (256, 128),
                 "fib" : (6, 1),
                 "flat-array" : (6000, 1200),
                 "hamlet" : (100, 10),
                 "imp-for" : (1000, 300),
                 "knuth-bendix" : (500, 100),
                 "lexgen" : (300, 50),
                 "life" : (6, 2),
                 "logic" : (40, 7),
                 "mandelbrot" : (2, 1),
                 "matrix-multiply" : (20, 20),
                 "md5" : (30, 10),
                 "merge" : (4000, 1000),
                 "mlyacc" : (500, 150),
                 "model-elimination" : (0, 0),
                 "mpuz" : (20, 5),
                 "nucleic" : (500, 150),
                 "output1" : (3, 3),
                 "peek" : (1000, 100),
                 "psdes-random" : (3, 1),
                 "ratio-regions" : (1024, 512),
                 "ray" : (100, 30),
                 "raytrace" : (10, 3),
                 "simple" : (100, 20),
                 "smith-normal-form" : (6, 1),
                 "tailfib" : (200, 60),
                 "tak" : (4, 2),
                 "tensor" : (3, 1),
                 "tsp" : (4, 1),
                 "tyan" : (80, 13),
                 "vector-concat" : (10, 2),
                 "vector-rev" : (20, 20),
                 "vliw" : (150, 30),
                 "wc-input1" : (4000, 1000),
                 "wc-scanStream" : (6000, 2000),
                 "zebrapig" : (15, 3),
                 "zern" : (2000, 500) }

def smlify(benchmark):
    """Add .sml to the end of a benchmark name (the name is a string)"""
    return benchmark + '.sml'

def mosml_compile(options, benchmark):
    """Compile a benchmark for Moscow ML"""
    mosml_command = 'mosmlc -orthodox -standalone -toplevel'

    sys_str = ' '.join([mosml_command, '-o', 'benchmark', benchmark])
    command = "os.system('%s')" % sys_str

    t = timeit.Timer(stmt=command, setup='import os')
    try:
        return t.timeit(number=1)
    except:
        t.print_exc()

def batch_benchmark(options, benchmark):
    """
    Build a batch benchmark.

    Some SML systems can do batch-compilation as you would expect in a UNIX system
    That is, they know to execute 'val _ = Main.doit n' for some n if it is in the
    file. Other systems, like PolyML and SML/NJ do not understand how to do this
    properly.

    This function batches a benchmark, ie, adds the call to Main.doit in the end of
    it and returns the name of the batched benchmark.
    """
    program = '/'.join([options.benchmarkdir, smlify(benchmark)])

    with open('benchmark.sml', 'w') as out:
        with open(program, 'r') as in_file:
            contents = in_file.read()
        out.write(contents)
        (slow, fast) = bench_counts[benchmark]
        if options.runs is not None:
            num = options.runs
        elif options.slow:
            num = slow
        else:
            num = fast

        out.write("val _ = Main.doit(%d)\n" % num)

    return 'benchmark.sml' # Generalize this

def run_benchmark(f):
    """Runs a benchmark executable in the file f"""
    command = "os.system('%s')" % (''.join(['./', f]))
    t = timeit.Timer(stmt=command, setup='import os')
    try:
        return t.timeit(number=1)
    except:
        t.print_exc()

def parse_options():
    """Parse options for the benchmark driver via Pythons option parser"""
    parser = OptionParser(usage="Usage: %prog [options]")
    parser.add_option('--benchmarkdir', dest="benchmarkdir", type='string',
                      help="Directory of benchmarks",
                      default="benchmarks")
    parser.add_option('--slow', dest="slow",
                      default=False, action='store_true',
                      help="Run the large (slow) benchmark.")
    parser.add_option("-b", '--benchmark', dest='run_only',
                      default=[], action='append',
                      help="Run this benchmark. Default: Run all")
    parser.add_option('--runs', dest='runs',
                      default=None, action='store', type='int',
                      help="Explicitly specify a number of runs")

    return parser.parse_args()

def main(options, args):
    """Main runner, run benchmarks"""
    #for benchmark in bench_counts.keys():
    if options.run_only == []:
        benchmarks_to_run = sorted(bench_counts.keys())
    else:
        # Process them and bail if the benchmark doesn't exist
        for b in options.run_only:
            if not bench_counts.has_key(b):
                raise NoSuchBench(b)

        benchmarks_to_run = options.run_only

    for benchmark in benchmarks_to_run:
        print "Running %s" % benchmark
        benchmark_name = batch_benchmark(options, benchmark)
        compile_time = mosml_compile(options, benchmark_name)
        print "Compilation time: %s" % compile_time
        run_time = run_benchmark('benchmark')
        print "Run time: %s" % run_time

if __name__ == '__main__':
    try:
        sys.exit(main(*parse_options()))
    except NoSuchBench, nb:
        print "Non existant benchmark: %s" % nb

