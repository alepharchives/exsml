#!/usr/bin/env python
"""
This is the benchmark driver program. It allows us to run the benchmarks
in the Ex-SML project.
"""
from __future__ import with_statement

import sys
import os
from optparse import OptionParser

# Counts from benchmarks from MLton
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

def smlify(test):
    return test + '.sml'

def mosml_compile(options, benchmark):

    mosml_command = 'mosmlc -orthodox -standalone -toplevel'

    sys_str = ' '.join([mosml_command, '-o', 'benchmark', benchmark])
    os.system(sys_str)

def batch_benchmark(options, benchmark):
    program = '/'.join([options.benchmarkdir, smlify(benchmark)])

    with open('benchmark.sml', 'w') as out:
        with open(program, 'r') as in_file:
            contents = in_file.read()
        out.write(contents)
        (fast, slow) = bench_counts[benchmark]
        out.write("val _ = Main.doit(%d)\n" % fast)

def run_benchmark(f):
    os.system(''.join(['./', f]))

def parse_options():
    """Parse options for the benchmark driver via Pythons option parser"""
    parser = OptionParser(usage="Usage: %prog [options]")
    parser.add_option('--benchmarkdir', dest="benchmarkdir", type='string',
                      help="Directory of benchmarks",
                      default="benchmarks")

    return parser.parse_args()

def main(options, args):
    """Main runner"""
    #for benchmark in bench_counts.keys():
    for benchmark in ['zebrapig']:
        print "Running %s" % benchmark
        batch_benchmark(options, benchmark)
        mosml_compile(options, 'benchmark.sml')
        run_benchmark('benchmark')

if __name__ == '__main__':
    sys.exit(main(*parse_options()))
