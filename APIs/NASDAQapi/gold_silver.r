library(httr)
library(jsonlite)
library(ggplot2)
library(cowplot)
library(xts)
library(yahoofinancer)
library(lubridate)
library(dplyr)

# Set parameters
# Specify the window size for the rolling correlation
window_size <- 252
# window_size 30 corresponds to n = 252 (yearly data)
# window_size 252 corresponds to n = 1260 (5-year)
n <- 1260



# Innitialisation - get data
apikey <- "8PV_YrgJQAiuD8nFXXWP"

# Gold data
url <- paste0("https://data.nasdaq.com/api/v3/datasets/LBMA/GOLD.json?api_key=", URLencode(apikey))
raw_result <- GET(url)
content <- content(raw_result, as = "text")
content_from_json <- fromJSON(content)
gold_data <- content_from_json$dataset$data
all_columns <- content_from_json$dataset$column_names
colnames(gold_data) <- all_columns
selected_columns <- all_columns[c(1, 2, 4)] # get 1030am gold data in USD & GBP
gold_data <- gold_data[, selected_columns]

# Silver data
url <- paste0("https://data.nasdaq.com/api/v3/datasets/LBMA/SILVER.json?api_key=", URLencode(apikey))
raw_result <- GET(url)
content <- content(raw_result, as = "text")
content_from_json <- fromJSON(content)
silver_data <- content_from_json$dataset$data
all_columns <- content_from_json$dataset$column_names
colnames(silver_data) <- all_columns # 12 noon (UK time)
selected_columns <- all_columns[1:3]
silver_data <- silver_data[, selected_columns]

# Convert data to data frame format
gold_data <- as.data.frame(gold_data)
silver_data <- as.data.frame(silver_data)

# Slice dataframe and in USD
recent_gold <- gold_data[1:n, ]
recent_silver <- silver_data[1:n, ]
gold_in_USD <- recent_gold[, c(1, 2)]
silver_in_USD <- recent_silver[, c(1, 2)]
dates <- as.Date(recent_gold$Date)
gold_in_USD[,c(1)] <- dates
silver_in_USD[,c(1)] <- dates

# Slice dataframe and in GBP
gold_in_GBP <- recent_gold[, c(1, 3)]
silver_in_GBP <- recent_silver[, c(1, 3)]
dates <- as.Date(recent_gold$Date)
gold_in_GBP[,c(1)] <- dates
silver_in_GBP[,c(1)] <- dates


# Make the plot 
par(bg = "black")

# Create the gold plot
plot(gold_in_USD$Date, gold_in_USD$`USD (AM)`, type = "l", col = "yellow", xlab = "Date", ylab = "USD", main = "Gold Prices")

# Combine the plots
par(new = TRUE)
plot.new()

# Create the silver plot
plot(silver_in_USD$Date, silver_in_USD$USD, type = "l", col = "white", xlab = "Date", ylab = "USD", main = "Silver Prices")

plot.window(xlim = c(1, 2), ylim = c(1, 2), xlab = "Date", ylab = "USD")
legend("topleft", c("Gold Prices", "Silver Prices"), col = c("yellow", "white"), lty = 1, bty = "n")





# Now get gold-silver ratio
par(bg = "white")
gold_in_USD$`USD (AM)` <- as.numeric(gold_in_USD$`USD (AM)`)
silver_in_USD$USD <- as.numeric(silver_in_USD$USD)
ratio <- gold_in_USD$`USD (AM)` / silver_in_USD$USD
date_ratio <- data.frame(Date = gold_in_USD$Date, Ratio = ratio)

# Make the plot
ggplot(date_ratio, aes(x = Date, y = Ratio)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  # This plot is made for n = 252; if n = 1250 is used, better change the label to
  # scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  xlab("Date") +
  ylab("Ratio") +
  ggtitle("Gold-Silver Price Ratio")





# Find correlation
# Create a data frame with the gold and silver prices
correlation_data <- data.frame(Date = gold_in_USD$Date, Gold = as.numeric(gold_in_USD$`USD (AM)`), Silver = as.numeric(silver_in_USD$USD))

# Convert 'Date' column to Date format
correlation_data$Date <- as.Date(correlation_data$Date)

# Create the xts object
xts_data <- xts(x = correlation_data[, c("Gold", "Silver")], order.by = correlation_data$Date)

# Define the rolling correlation function
rolling_corr <- function(x) {
  cor(x[, "Gold"], x[, "Silver"], use = "pairwise.complete.obs")
}

# Calculate the rolling correlation using rollapply
rolling_correlation <- rollapply(xts_data, width = window_size, FUN = rolling_corr, by.column = FALSE, align = "right", fill = NA)

# Create the plot
main_title <- paste0("Rolling Correlation: Gold-Silver Prices ", n, " days")
plot(rolling_correlation, xlab = "Date", ylab = "Rolling Correlation", main = main_title)

# CEY.L - gold, EDV.L - gold, FRES.L - silver, HOC.L - gold & silver
# Set the date range
start_date <- Sys.Date() %m-% years(1) # Date of five years before
end_date <- Sys.Date()

# Percentage changes for gold, silver, and FT100
gold_in_GBP$`GBP (AM)` <- as.numeric(gold_in_GBP$`GBP (AM)`)
silver_in_GBP$GBP <- as.numeric(silver_in_GBP$GBP)
gold_in_GBP$PercentageChange <- c(NA, diff(gold_in_GBP$`GBP (AM)`)/gold_in_GBP$`GBP (AM)`[-length(gold_in_GBP$`GBP (AM)`)]) * 100
silver_in_GBP$PercentageChange <- c(NA, diff(silver_in_GBP$GBP)/silver_in_GBP$GBP[-length(silver_in_GBP$GBP)]) * 100





# Set the date range
start_date <- Sys.Date() %m-% years(5) # Date of one year before
end_date <- Sys.Date()

# Get all the stock prices
cey <- Ticker$new('CEY.L')
cey_stock <- cey$get_history(start = start_date, end = end_date, interval = '1d')

# Get the close price only
cey_close_price <- cey_stock[,c(1, 6)]

# Calculate percentage change
cey_close_price$PercentageChange <- c(NA, diff(cey_close_price$close)/cey_close_price$close[-length(cey_close_price$close)]) * 100

# Make sure date is in correct format
cey_close_price$date <- as.Date(cey_close_price$date)

# Append gold/siver prices and FTSE100 price into the same dataframe
cey_close_price$Gold <- c(gold_in_GBP$PercentageChange[match(cey_close_price$date, gold_in_GBP$Date)])

# Omit NA rows
cey_close_price <- na.omit(cey_close_price)

# # Plot CEY.L prices and Gold prices in a timeseries plot (Daily)
# ggplot(cey_close_price, aes(x = date, y = PercentageChange)) +
#   geom_line(aes(color = "CEY"), show.legend = TRUE) +
#   scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
#   xlab("Date") +
#   ylab("Daily Percentage Change") +
#   ggtitle("Daily percentage change of CEY.L stock prices vs Gold prices") +
#   # Add the other geom line
#   geom_line(aes(y = Gold, color = "Gold"), show.legend = TRUE) +
#   labs(color = "Legend", fill = "Legend") +
#   scale_color_manual(values = c("CEY" = "blue", "Gold" = "red")) +
#   scale_fill_manual(values = c("CEY" = "blue", "Gold" = "red"))


# Get weekly percentage change
# For gold_in_GBP
gold_weekly_price_GBP <- gold_in_GBP %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date) %>%
  mutate(week_start = floor_date(Date, "week")) %>%
  group_by(week_start) %>%
  summarize(weekly_gold_price = last(`GBP (AM)`),
  weekly_percentage_change = (last(`GBP (AM)`) / first(`GBP (AM)`) - 1) * 100)
gold_weekly_price_GBP$week_start <- as.Date(gold_weekly_price_GBP$week_start)

# For silver_in_GBP
silver_weekly_price_GBP <- silver_in_GBP %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date) %>%
  mutate(week_start = floor_date(Date, "week")) %>%
  group_by(week_start) %>%
  summarize(weekly_gold_price = last(GBP),
  weekly_percentage_change = (last(GBP) / first(GBP) - 1) * 100)
silver_weekly_price_GBP$week_start <- as.Date(silver_weekly_price_GBP$week_start)

# CEY weekly percentage change
cey_weekly_price <- cey_close_price %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(week_start = floor_date(date, "week")) %>%
  group_by(week_start) %>%
  summarize(weekly_cey_price = last(close),
  weekly_percentage_change = (last(close) / first(close) - 1) * 100)
cey_weekly_price$week_start <- as.Date(cey_weekly_price$week_start)


# Append Gold weekly price percentage change to cey_weekly_price
cey_weekly_price$Gold <- gold_weekly_price_GBP$weekly_percentage_change[match(cey_weekly_price$week_start, gold_weekly_price_GBP$week_start)]

# Plot CEY.L prices and Gold prices in a timeseries plot (Weekly)
ggplot(cey_weekly_price, aes(x = week_start, y = weekly_percentage_change)) +
  geom_line(aes(color = "CEY"), show.legend = TRUE) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  xlab("Date") +
  ylab("Weekly Percentage Change") +
  ggtitle("Weekly percentage change of CEY.L stock prices vs Gold prices") +
  # Add the other geom line
  geom_line(aes(y = Gold, color = "Gold"), show.legend = TRUE) +
  labs(color = "Legend", fill = "Legend") +
  scale_color_manual(values = c("CEY" = "blue", "Gold" = "red")) +
  scale_fill_manual(values = c("CEY" = "blue", "Gold" = "red"))

# Use weekly data for regression
cey_formula <- cey_weekly_price$weekly_percentage_change ~ 0 + cey_weekly_price$Gold
cey_model <- lm(cey_formula, cey_weekly_price)

# Find r-squared etc
cey_summary <- summary(cey_model) # r-squared ~ 0.21

# # Try a shift of one week
# cey_weekly_price <- cey_weekly_price %>%
#   mutate(Gold = lag(Gold)) # Use lag to move Gold column down by one entry
#   # mutate(Gold = lead(Gold)) # Use lead to move Gold column up by one entry
# cey_weekly_price <- na.omit(cey_weekly_price)
# cey_model <- lm(cey_formula, cey_weekly_price)
# cey_summary <- summary(cey_model) # r-squared ~ 0.0008


# Can do the similar things for other stocks


""" # nolint
# Failed linear regression using Daily data
# Find regression between mining companies and gold/silver prices

# Set the date range
start_date <- Sys.Date() %m-% years(1) # Date of one year before
end_date <- Sys.Date()

# Find stock prices for different companies in the recent year
ftse100 <- Ticker$new('^FTSE')
ftse100_stock <- ftse100$get_history(start = start_date, end = end_date, interval = '1d')
ftse100_close_price <- ftse100_stock[, c(1, 6)]
ftse100_close_price$date <- as.Date(ftse100_close_price$date)

# Percentage changes for gold, silver, and FT100
gold_in_GBP$`GBP (AM)` <- as.numeric(gold_in_GBP$`GBP (AM)`)
silver_in_GBP$GBP <- as.numeric(silver_in_GBP$GBP)
ftse100_close_price$close <- as.numeric(ftse100_close_price$close)
gold_in_GBP$PercentageChange <- c(NA, diff(gold_in_GBP$`GBP (AM)`)/gold_in_GBP$`GBP (AM)`[-length(gold_in_GBP$`GBP (AM)`)]) * 100
silver_in_GBP$PercentageChange <- c(NA, diff(silver_in_GBP$GBP)/silver_in_GBP$GBP[-length(silver_in_GBP$GBP)]) * 100
ftse100_close_price$PercentageChange <- c(NA, diff(ftse100_close_price$close)/ftse100_close_price$close[-length(ftse100_close_price$close)]) * 100

# Get all the stock prices
cey <- Ticker$new('cey.L')
cey_stock <- cey$get_history(start = start_date, end = end_date, interval = '1d')
cey_close_price <- cey_stock[,c(1, 6)]

# Regression for cey
# Calculate the percentage change
cey_close_price$PercentageChange <- c(NA, diff(cey_close_price$close)/cey_close_price$close[-length(cey_close_price$close)]) * 100

# Make sure date is in correct format
cey_close_price$date <- as.Date(cey_close_price$date)
# gold_in_GBP$date <- as.Date(gold_in_GBP$date)


# Append gold/siver prices and FTSE100 price into the same dataframe
cey_close_price$Gold <- c(gold_in_GBP$PercentageChange[match(cey_close_price$date, gold_in_GBP$Date)])
cey_close_price$Silver <- c(silver_in_GBP$PercentageChange[match(cey_close_price$date, silver_in_GBP$Date)])
cey_close_price$FTSE100 <- c(ftse100_close_price$PercentageChange[match(cey_close_price$date, ftse100_close_price$date)])

# Omit NA rows
cey_close_price <- na.omit(cey_close_price)

# Build a regression model
# cey_formula <- cey_close_price$PercentageChange ~ 0 + cey_close_price$Gold +  cey_close_price$Silver + cey_close_price$FTSE100
cey_formula <- cey_close_price$PercentageChange ~ 0 + cey_close_price$Gold
# Dependent: stock price; Independent: gold price & FTSE100 price
cey_model <- lm(cey_formula, cey_close_price)

# Get the summary stats
cey_summary <- summary(cey_model)

# Extract the regression statistics
multiple_r <- sqrt(cey_summary$r.squared)
r_squared <- cey_summary$r.squared
adjusted_r_squared <- cey_summary$adj.r.squared
standard_error <- sqrt(diag(cey_summary$cov.unscaled))
num_observations <- cey_summary$df[2]

cey_summary_df <- data.frame(Multiple_R = multiple_r,
                         R_Squared = r_squared,
                         Adjusted_R_Squared = adjusted_r_squared,
                         Standard_Error = standard_error,
                         Observations = num_observations)

# Regression for EDV
edv <- Ticker$new('EDV.L')
edv_stock <- edv$get_history(start = start_date, end = end_date, interval = '1d')
edv_close_price <- edv_stock[, c(1, 6)]
edv_close_price$PercentageChange <- c(NA, diff(edv_close_price$close)/edv_close_price$close[-length(edv_close_price$close)]) * 100
edv_close_price$date <- as.Date(edv_close_price$date)
edv_close_price$Gold <- c(gold_in_GBP$PercentageChange[match(edv_close_price$date, gold_in_GBP$Date)])
edv_close_price$Silver <- c(silver_in_GBP$PercentageChange[match(edv_close_price$date, silver_in_GBP$Date)])
edv_close_price$FTSE100 <- c(ftse100_close_price$PercentageChange[match(edv_close_price$date, ftse100_close_price$date)])
edv_close_price <- na.omit(edv_close_price)
edv_formula <- edv_close_price$PercentageChange ~ 0 + edv_close_price$Gold + edv_close_price$Silver + edv_close_price$FTSE100
edv_model <- lm(edv_formula, edv_close_price)
edv_coef_all <- coef(edv_model)

edv_summary <- summary(edv_model)

multiple_r <- sqrt(edv_summary$r.squared)
r_squared <- edv_summary$r.squared
adjusted_r_squared <- edv_summary$adj.r.squared
standard_error <- sqrt(diag(edv_summary$cov.unscaled))
num_observations <- edv_summary$df[2]

edv_summary_df <- data.frame(Multiple_R = multiple_r,
                         R_Squared = r_squared,
                         Adjusted_R_Squared = adjusted_r_squared,
                         Standard_Error = standard_error,
                         Observations = num_observations)

# Regression for FRES
fres <- Ticker$new('FRES.L')
fres_stock <- fres$get_history(start = start_date, end = end_date, interval = '1d')
fres_close_price <- fres_stock[, c(1, 6)]
fres_close_price$PercentageChange <- c(NA, diff(fres_close_price$close)/fres_close_price$close[-length(fres_close_price$close)]) * 100
fres_close_price$date <- as.Date(fres_close_price$date)
fres_close_price$Gold <- c(gold_in_GBP$PercentageChange[match(fres_close_price$date, gold_in_GBP$Date)])
fres_close_price$Silver <- c(silver_in_GBP$PercentageChange[match(fres_close_price$date, silver_in_GBP$Date)])
fres_close_price$FTSE100 <- c(ftse100_close_price$PercentageChange[match(fres_close_price$date, ftse100_close_price$date)])
fres_close_price <- na.omit(fres_close_price)
fres_formula <- fres_close_price$PercentageChange ~ 0 + fres_close_price$Gold + fres_close_price$Silver + fres_close_price$FTSE100
fres_model <- lm(fres_formula, fres_close_price)
fres_coef_all <- coef(fres_model)

fres_summary <- summary(fres_model)

multiple_r <- sqrt(fres_summary$r.squared)
r_squared <- fres_summary$r.squared
adjusted_r_squared <- fres_summary$adj.r.squared
standard_error <- sqrt(diag(fres_summary$cov.unscaled))
num_observations <- fres_summary$df[2]

fres_summary_df <- data.frame(Multiple_R = multiple_r,
                         R_Squared = r_squared,
                         Adjusted_R_Squared = adjusted_r_squared,
                         Standard_Error = standard_error,
                         Observations = num_observations)


# Regression for HOC
hoc <- Ticker$new('HOC.L')
hoc_stock <- hoc$get_history(start = start_date, end = end_date, interval = '1d')
hoc_close_price <- hoc_stock[, c(1, 6)]
hoc_close_price$PercentageChange <- c(NA, diff(hoc_close_price$close)/hoc_close_price$close[-length(hoc_close_price$close)]) * 100
hoc_close_price$date <- as.Date(hoc_close_price$date)
hoc_close_price$Gold <- c(gold_in_GBP$PercentageChange[match(hoc_close_price$date, gold_in_GBP$Date)])
hoc_close_price$Silver <- c(silver_in_GBP$PercentageChange[match(hoc_close_price$date, silver_in_GBP$Date)])
hoc_close_price$FTSE100 <- c(ftse100_close_price$PercentageChange[match(hoc_close_price$date, ftse100_close_price$date)])
hoc_close_price <- na.omit(hoc_close_price)
hoc_formula <- hoc_close_price$PercentageChange ~ 0 + hoc_close_price$Gold + hoc_close_price$Silver + hoc_close_price$FTSE100
hoc_model <- lm(hoc_formula, hoc_close_price)
hoc_coef_all <- coef(hoc_model)

hoc_summary <- summary(hoc_model)

multiple_r <- sqrt(hoc_summary$r.squared)
r_squared <- hoc_summary$r.squared
adjusted_r_squared <- hoc_summary$adj.r.squared
standard_error <- sqrt(diag(hoc_summary$cov.unscaled))
num_observations <- hoc_summary$df[2]

hoc_summary_df <- data.frame(Multiple_R = multiple_r,
                         R_Squared = r_squared,
                         Adjusted_R_Squared = adjusted_r_squared,
                         Standard_Error = standard_error,
                         Observations = num_observations)

"""