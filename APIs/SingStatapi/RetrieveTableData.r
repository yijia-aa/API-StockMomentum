library(httr)
library(jsonlite)

key <- "M891301"
url <- paste0("tablebuilder.singstat.gov.sg/api/table/tabledata/", key)
# append resource id in the end

content <- httr::content(raw_result, as = "text")
content_from_json <- jsonlite::fromJSON(content)
record_data <- content_from_json[["Data"]]