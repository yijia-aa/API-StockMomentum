library(httr)
library(jsonlite)

url <- paste0("tablebuilder.singstat.gov.sg/api/table/metadata/M891111")
# append resource id in the end

raw_result <- httr::GET(url)
content <- httr::content(raw_result, as = 'text')

content_from_json <- jsonlite::fromJSON(content)
# head(content_from_json)

record_data <- content_from_json[["Data"]][["records"]]
attributes(record_data)

rows <- record_data[["row"]]
# head(record_data)
