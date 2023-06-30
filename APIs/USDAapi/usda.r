library(httr)
library(jsonlite)
library(readxl)

# URL for token generation
token_url <- "https://usda.library.cornell.edu/user_token"

# JSON payload with email and password
payload <- list(auth = list(email = "chenyijia212@gmail.com", password = "CyJ000011111!")) 

# Send POST request to obtain the token
response <- POST(token_url, body = toJSON(payload), add_headers("Content-Type" = "application/json")) 

# Extract the token from the response
# token <- content(response[["request"]][["headers"]][["Authorization"]]) 
# not working

token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOjE0MzAxfQ.vItwIC_x1RIDA5q3abzUG36-OGVcAkLIH1ioC46ST9E"

# API endpoint URL
api_url <- "https://usda.library.cornell.edu/api/v1/release/findByIdentifier/wasde?latest=false&start_date=2022-01-01&end_date=2023-06-26"

# Send GET request with the authorization token
response <- GET(api_url, add_headers("accept" = "application/json", "Authorization" = paste("Bearer", token)))

# Extract and print the response content
content <- content(response)
# print(content)

xls_link <- content[[1]][["files"]][[3]]
# 1 - pdf, 2 - txt, 3 - xls, 4 - xml

# Create a temporary file path to save the XLS file
temp_file <- tempfile(fileext = ".xls")

# Download the XLS file using authentication token
GET(xls_link, add_headers("Authorization" = paste("Bearer", token)), write_disk(temp_file))

# Read the XLS file and store each sheet as a data frame
xls_data <- read_excel(temp_file, sheet = 2, col_names = TRUE)

# Print the data frames
for (i in 1:length(xls_data)) {
  print(xls_data[[i]])
}


""" # nolint
in terminal:

curl -X POST "https://usda.library.cornell.edu/user_token" -d '{"auth": {"email":"chenyijia212@gmail.com","password":"CyJ000011111!"}}' -H "Content-Type: application/json"

curl 'https://usda.library.cornell.edu/api/v1/publication/findByIdentifier/wasde' \
  -H 'accept: application/json' \
  -H 'Authorization: Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOjE0MzAxfQ.vItwIC_x1RIDA5q3abzUG36-OGVcAkLIH1ioC46ST9E'
"""