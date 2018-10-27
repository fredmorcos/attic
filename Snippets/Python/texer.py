#!/usr/bin/python

# Frederic-Gerald Morcos
# Public Domain

maxi = 17 # numbering of tests

outf = open('report-extra.tex', 'w')

i = 1
while i <= maxi:
	data = open('test/' + str(i) + '-results').readlines()

	outf.write('\\begin{newpage}\n')
	outf.write('\\textbf{Test ' + str(i) + '\\\\\\\\}\n')
	outf.write('\\includegraphics[scale=0.5]{images/' + str(i) + '-net.pdf}\\\\\n')
	outf.write('\\includegraphics[scale=0.5]{images/' + str(i) + '-run.pdf}\\\\\n')
	outf.write('\\includegraphics[scale=0.5]{images/' + str(i) + '-perf.pdf}\\\\\n')
	outf.write('\\lstset{basicstyle=\\ttfamily\\small}\n')
	outf.write('\\begin{lstlisting}\n')
	for l in data:
		outf.write(l)
	outf.write('\\end{lstlisting}\n')
	outf.write('\\end{newpage}\n')

	i += 1

outf.close()

