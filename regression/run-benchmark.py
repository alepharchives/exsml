"""
This is the benchmark driver program. It allows us to run the benchmarks
in the Ex-SML project.
"""

from optparse import OptionParser


def parse_options(args):
    """Parse options for the benchmark driver via Pythons option parser"""
    parser = OptionParser(usage="Usage: %prog [options]")
    parser.add_option('--benchmarkdir', dest="benchmarkdir", type='string',
                      help="Directory of benchmarks",
                      default="bencmarks")

    return parser.parse_args()

def main(options, args):
    """Main runner"""
    pass


if __name__ == '__main__':
    sys.exit(main(*parse_options()))
