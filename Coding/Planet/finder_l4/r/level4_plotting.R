library(plotly)

plot_ly(l4in, name = "Level 4 F", x = ~index, y = ~f, mode = 'lines+markers', type = 'scatter', text = ~paste(f)) %>%
  add_trace(name = "Level 4 T", x = ~index, y = ~t, text = ~paste(t)) %>%
  add_trace(name = "Level 4 S", x = ~index, y = ~s, text = ~paste(s))

plot_ly(l1_2, name = "beginnerSecond0", x = ~index, y = ~beginnerSecond0, mode = 'lines+markers', type = 'scatter', text = ~paste(beginnerSecond0)) %>%
  add_trace(name = "beginnerSecond1", x = ~index, y = ~beginnerSecond1, text = ~paste(beginnerSecond1)) %>%
  add_trace(name = "beginnerSecond2", x = ~index, y = ~beginnerSecond2, text = ~paste(beginnerSecond2)) %>%
  add_trace(name = "beginnerSecond3", x = ~index, y = ~beginnerSecond3, text = ~paste(beginnerSecond3)) %>%
  add_trace(name = "beginnerSecond4", x = ~index, y = ~beginnerSecond4, text = ~paste(beginnerSecond4))
