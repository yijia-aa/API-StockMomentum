library(yahoofinancer)
library(dplyr)
library(tseries)
library(lubridate)
library(zoo)

# Store your portfolio of stocks
all_tickers <- c("AAL.L", "ANTO.L", "CAPD.L", "CEY.L", "EDV.L", "FXPO.L",
"FRES.L", "GLEN.L", "HILS.L", "HOC.L", "KMR.L", "PDL.L", "RIO.L", "ZTF.L",
"CRDA.L", "ELM.L", "JMAT.L", "RHIM.L", "SYNT.L", "TET.L", "VCT.L", "BP.L",
"CNE.L", "DEC.L", "ENOG.L", "ENQ.L", "HBR.L", "HTG.L", "PFC.L",
"PHAR.L", "PODP.L", "SHEL.L", "TLW.L", "WG.L")

n <- length(all_tickers)

# Initialise a dataframe
tickers_momentum <- data.frame(ticker = all_tickers, annualised_sd = c(1:n), momentum_annual = c(1:n),
half_annualised_sd = c(1:n), momentum_half = c(1:n), long_momentum_number = c(1:n), tendency_to_trend = c(1:n))

# Set parameters
end <- Sys.Date() %m-% months(1) # End by the previous month
start <- Sys.Date() %m-% months(37) # Start from 3 years ago
periods <- 52 # 52 weeks in one year

# number of weeks for historic rolling mean percentage change
width <- 4

# Calculated annualised standard deviation for weekly percentage change over three years period
for (i in 1:n){
    ticker <- Ticker$new(all_tickers[i])
    his <- ticker$get_history(start = start, end = end, interval = '1wk')[, c(1,6)]
    his$PercentageChange <- c(NA, diff(his$close)/his$close[-length(his$close)]) * 100
    his <- na.omit(his)
    weekly_sd <- sd(his$PercentageChange)
    tickers_momentum$annualised_sd[i] <- weekly_sd * sqrt(periods)
    tickers_momentum$half_annualised_sd[i] <- weekly_sd * sqrt(periods/2)

    # For a particular stock, how long is its trend?
    his$rolling_means <- rollapply(his$PercentageChange, width = width, FUN = mean, align = "left", fill = NA)
    his <- na.omit(his)

    # Find length of momentum periods
    run_lengths <- rle(as.numeric(his$rolling_means) > 0)$lengths
    mean <- mean(run_lengths)
    sd <- sd(run_lengths)
    outliers <- run_lengths[run_lengths>mean+1.5*sd]
    tickers_momentum$long_momentum_number[i] <- length(outliers)
    if (tickers_momentum$long_momentum_number[i] > 4) {
        tickers_momentum$tendency_to_trend[i] <- "Yes"
    } else if (tickers_momentum$long_momentum_number[i] < 3) {
       tickers_momentum$tendency_to_trend[i] <- "No"
    } else {
       tickers_momentum$tendency_to_trend[i] <- "Maybe"
    }
}

# Take the recent 1-year and half-year data
end <- Sys.Date() %m-% months(1)
start_half <- Sys.Date() %m-% months(7)
start_annual <- Sys.Date() %m-% months(13)

# Calculate half-year and one-year momentum
for (i in 1:n){
    ticker <- Ticker$new(all_tickers[i])
    his_half <- ticker$get_history(start = start_half, end = end, interval = '1wk')[, c(1,6)]
    his_annual <- ticker$get_history(start = start_annual, end = end, interval = '1wk')[, c(1,6)]

    his_half$PercentageChange <- c(NA, diff(his_half$close)/his_half$close[-length(his_half$close)]) * 100
    his_annual$PercentageChange <- c(NA, diff(his_annual$close)/his_annual$close[-length(his_annual$close)]) * 100

    his_half <- na.omit(his_half)
    his_annual <- na.omit(his_annual)

    mean_half <- mean(his_half$PercentageChange)
    mean_annual <- mean(his_annual$PercentageChange)
    tickers_momentum$momentum_half[i] <- mean_half * periods / 2
    tickers_momentum$momentum_annual[i] <- mean_annual * periods
}


# Lastly, order the dataframe according to long_momentum_number
ordered_momentum <- tickers_momentum[order(tickers_momentum$long_momentum_number, decreasing = TRUE), ]

print(ordered_momentum)

# NB: better to visualise in R