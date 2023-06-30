library(yahoofinancer)
library(dplyr)
library(tseries)

ticker <- Ticker$new('vct.l')
his <- ticker$get_history(start = "2000-01-01", end = Sys.Date(), interval = '3mo')[, c(1,6)]
his$PercentageChange <- c(NA, diff(his$close)/his$close[-length(his$close)]) * 100
his <- na.omit(his)

acf_result <- acf(his$PercentageChange, lag.max = 40)