library(httr)
library(jsonlite)

apikey <- "A545DBA5-6EC4-47E8-B435-341F020F2B5C"
url <- paste0("https://apps.bea.gov/api/data/?&UserID=", URLencode(apikey),
              "&method=GetData&DataSetName=NIPA&TableName=T10105&Frequency=A&Year=ALL&ResultFormat=json")
raw_result <- GET(url)
content <- content(raw_result, as = "text")
content_from_json <- fromJSON(content)

data <- content_from_json[[1]][["Results"]][["Data"]]


# Extract the lines with LineNumber equal to 1
lines <- data[data$LineNumber == 1, c("TimePeriod", "DataValue")]


# Print the lines
print(lines)