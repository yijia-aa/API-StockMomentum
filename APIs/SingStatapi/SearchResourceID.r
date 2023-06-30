library(httr)
library(jsonlite)

url <- paste0("https://tablebuilder.singstat.gov.sg/api/table/resourceid?keyword=energy&searchOption=title")
# keyword; searchOption: include “all”, “title” or “variable”.

raw_result <- httr::GET(url)
content <- httr::content(raw_result, as = 'text')

content_from_json <- jsonlite::fromJSON(content)
# head(content_from_json)

record_data <- content_from_json[["Data"]][["records"]]
# head(record_data)

record_data["id"]["title"]