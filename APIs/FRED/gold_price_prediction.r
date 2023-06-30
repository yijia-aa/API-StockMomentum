library(httr)

library(jsonlite)
library(ggplot2)
library(lubridate)
library(dplyr)

# Load gold prices from NASDAQ
n <- 1260
apikey_nasdaq <- "8PV_YrgJQAiuD8nFXXWP"
url <- paste0("https://data.nasdaq.com/api/v3/datasets/LBMA/GOLD.json?api_key=", URLencode(apikey_nasdaq))
content_from_json <- fromJSON(content(GET(url), as = "text"))
gold_data <- content_from_json$dataset$data
all_columns <- content_from_json$dataset$column_names
colnames(gold_data) <- all_columns
selected_columns <- all_columns[c(1, 3)] # get 3pm gold data in USD
gold_data <- as.data.frame(gold_data[, selected_columns])

# Get gold weekly prices
gold_weekly_price <- gold_data %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date) %>%
  mutate(week_start = floor_date(Date, "week")) %>%
  group_by(week_start) %>%
  mutate(`USD (PM)` = as.numeric(`USD (PM)`)) %>%
  summarize(weekly_gold_price = last(`USD (PM)`))
gold_weekly_price <- na.omit(gold_weekly_price)
gold_weekly_price$week_start <- as.Date(gold_weekly_price$week_start)

# Get factors from FRED
code <- c("DTWEXBGS", "DTWEXAFEGS", "DTWEXEMEGS", "BAMLH0A0HYM2",
"BAMLC0A0CM", "BAA10Y", "VIXCLS", "GVZCLS", "T10YIE")

n <- length(factor_codes)


# Load the title and units
url_info <- paste0("https://api.stlouisfed.org/fred/series?series_id=", code[i], "&api_key=", apikey, "&file_type=json")
info_content <- fromJSON(content(GET(url_info), as = "text"))
title <- info_content$seriess$title
units <- info_content$seriess$units

summary_table <- data.frame(variable = character(),
                            r_squared = numeric(),
                            f_statistic = numeric(),
                            stringsAsFactors = FALSE)
# For each of the above factors, run a single regression
apikey_fred <- "8efadd61dc7028ae9148d6c7a307f018"
for (i in 1:n){
    url <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=", code[i],"&api_key=", apikey_fred, "&file_type=json")
    content_from_json <- fromJSON(content(GET(url), as = "text"))
    data <- data.frame(date = content_from_json$observations$date, value = content_from_json$observations$value)
    # Get data weekly value
    data_weekly <- data %>%
        mutate(date = as.Date(date)) %>%
        arrange(date) %>%
        mutate(week_start = floor_date(date, "week")) %>%
        group_by(week_start) %>%
        summarize(weekly_value = last(value))
    data_weekly <- na.omit(data_weekly)
    data_weekly$week_start <- as.Date(data_weekly$week_start)

    data_weekly$gold <- c(gold_weekly_price$weekly_gold_price[match(data_weekly$week_start, gold_weekly_price$week_start)])

    formula <- data_weekly$gold ~ 0 + data_weekly$weekly_value
    model <- lm(formula, data_weekly)
    summary <- summary(model)
    r_squared <- summary$r.squared
    f_statistic <- summary$fstatistic[1]
    summary_table$variable[i] <- code[i]

    # Not working
    summary_table$r_squared[i] <- r_squared
    summary_table$f_statistic[i] <-  f_statistic
}
