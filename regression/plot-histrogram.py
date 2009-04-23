#!/usr/bin/env python

"""
This is the Benchmark plotter program. It works by plotting against two tags in the
database
"""
from __future__ import with_statement

import sys
import os
import timeit
import datetime

import simplejson

from optparse import OptionParser

def read_database(db_file):
    """
    Read in the database from the db_file. On error, return a new
    empty database.
    """
    try:
        with open(db_file, 'r') as f:
            db = simplejson.load(f)

        return db
    except IOError, e:
        return {}

def parse_options():
    """Parse options for the benchmark driver via Pythons option parser"""
    parser = OptionParser(usage="Usage: %prog [options] tag1 tag2")
    parser.add_option('-o', action='store', type='string',
                      default='benchmark.dat', dest='outfile',
                      help="The file in which to produce the output")
    parser.add_option('--dbfile', action='store', type='string',
                      default='runs.json', dest='db_file',
                      help="The database from which to pull data")

    return parser.parse_args()

def initialize_entries(tag1, tag2):
    """Initialize the histogram database"""
    return [('Benchmark', tag1, tag2)]

def update_histogram(es, a, b, benchmark):
    """Append an entry to the histrogram database"""
    es.append((benchmark, a, b))

def write_histogram(entries, out):
    """Write histogram to a file"""
    with open(out, 'w') as f:
        for (b, r_a, r_b) in entries:
            f.write("%s\t%s\t%s\n", (b, r_a, r_b))

def main(options, args):
    """Main runner, run benchmarks"""
    db = read_database(options.db_file)
    run_a = db[args[0]]
    run_b = db[args[1]]

    entries = initialize_entries(args[0], args[1])

    for benchmark in run_a.keys():
        runtime_a = run_a[benchmark]['run_time']
        runtime_b = run_b[benchmark]['run_time']

        update_histogram(entries, runtime_a, runtime_b, benchmark)

    write_histogram(entries, options.outfile)

if __name__ == '__main__':
    try:
        sys.exit(main(*parse_options()))
    except RuntimeError, e:
        print "Runtime error: %s" % e

