set title "GPStraces (100 requests in total)"
set style data linespoints
set xlabel "Simultaneous requests"
set xtics (1,2,4,8,16,32)
set ylabel "Time (ms)"
# set yrange [0:400000]
plot 'results_puma_100.txt' using 1:2 title 'POST (puma)', \
     'results_puma_100.txt' using 1:3 title 'GET (puma)', \
     'results_webrick_100.txt' using 1:2 title 'POST (webrick)', \
     'results_webrick_100.txt' using 1:3 title 'GET (webrick)'
