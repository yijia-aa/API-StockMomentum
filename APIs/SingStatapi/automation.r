library(httr)
library(jsonlite)

keyword <- "energy"
searchOption <- "title"
# keyword; searchOption: include “all”, “title” or “variable”.
url <- paste0("https://tablebuilder.singstat.gov.sg/api/table/resourceid?keyword=", keyword, "&searchOption=", searchOption)


raw_result <- httr::GET(url)
content <- httr::content(raw_result, as = "text")
content_from_json <- jsonlite::fromJSON(content)
record_data <- content_from_json[["Data"]][["records"]]
list_of_ids <- record_data["id"]

for (id in list_of_ids)
{
    id <- str(id)
    url <- paste0("tablebuilder.singstat.gov.sg/api/table/tabledata/", id)
    raw_result <- httr::GET(url)
    content <- httr::content(raw_result, as = "text")
    content_from_json <- jsonlite::fromJSON(content)

    record_data <- content_from_json[["Data"]]
    rows <- record_data[["row"]]
    indices <- which(grepl("crude oil", rows$rowText, ignore.case = TRUE))   
    for (index in indices)
    {
        colume <- rows[[3]]
        plot(column)
    }
}