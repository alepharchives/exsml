#
set title "Ex-SML Benchmark run"
set auto x
set yrange [0:300]
set style data histogram
set style histogram cluster gap 1
set style fill solid border -1
set boxwidth 0.9
set xtic rotate by -45
#set bmargin 10 
plot 'benchmark.dat' using 2:xtic(1) ti col, '' u 3 ti col
#
