set title "GPStraces (1000 requests in total)"
set style data linespoints
set xlabel "Simultaneous requests"
set xtics (1,2,4,8,16,32)
set ylabel "Time (ms)"
set yrange [0:400000]
plot 'results_puma.txt' using 1:2 title 'POST (puma)', \
     'results_puma.txt' using 1:3 title 'GET (puma)', \
     'results_webrick.txt' using 1:2 title 'POST (webrick)', \
     'results_webrick.txt' using 1:3 title 'GET (webrick)'
