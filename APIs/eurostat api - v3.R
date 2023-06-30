#httr is to get the GET function and jsonlite is to get the fromJSON so that I can retrieve the data in the right format.
install.packages("httr")
install.packages("jsonlite")
install.packages("tidyr")

#load libraries (httr for GET, jsonlite for fromJSON and tidyr for complete!)
library(httr)
library(jsonlite)
library(tidyr)
#this imports the raw data and makes sure it is readable in JSON
#data is EU total from 01/2024
res = GET("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/NRG_TE_GASM?format=JSON&lang=en&freq=M&siec=G3000&partner=TOTAL&unit=MIO_M3&geo=EU27_2020&time=2014-01&time=2014-02&time=2014-03&time=2014-04&time=2014-05&time=2014-06&time=2014-07&time=2014-08&time=2014-09&time=2014-10&time=2014-11&time=2014-12&time=2015-01&time=2015-02&time=2015-03&time=2015-04&time=2015-05&time=2015-06&time=2015-07&time=2015-08&time=2015-09&time=2015-10&time=2015-11&time=2015-12&time=2016-01&time=2016-02&time=2016-03&time=2016-04&time=2016-05&time=2016-06&time=2016-07&time=2016-08&time=2016-09&time=2016-10&time=2016-11&time=2016-12&time=2017-01&time=2017-02&time=2017-03&time=2017-04&time=2017-05&time=2017-06&time=2017-07&time=2017-08&time=2017-09&time=2017-10&time=2017-11&time=2017-12&time=2018-01&time=2018-02&time=2018-03&time=2018-04&time=2018-05&time=2018-06&time=2018-07&time=2018-08&time=2018-09&time=2018-10&time=2018-11&time=2018-12&time=2019-01&time=2019-02&time=2019-03&time=2019-04&time=2019-05&time=2019-06&time=2019-07&time=2019-08&time=2019-09&time=2019-10&time=2019-11&time=2019-12&time=2020-01&time=2020-02&time=2020-03&time=2020-04&time=2020-05&time=2020-06&time=2020-07&time=2020-08&time=2020-09&time=2020-10&time=2020-11&time=2020-12&time=2021-01&time=2021-02&time=2021-03&time=2021-04&time=2021-05&time=2021-06&time=2021-07&time=2021-08&time=2021-09&time=2021-10&time=2021-11&time=2021-12&time=2022-01&time=2022-02&time=2022-03&time=2022-04&time=2022-05&time=2022-06&time=2022-07&time=2022-08&time=2022-09&time=2022-10&time=2022-11&time=2022-12&time=2023-01&time=2023-02&time=2023-03&time=2023-04&time=2023-05")
rawToChar(res$content)
data = fromJSON(rawToChar(res$content))

# Create a sample data frame with missing indices

df1 <- data[["dimension"]][["time"]][["category"]][["label"]]
df2 <- data[["value"]]

end <- length(df1)-1
indices <- 0:end
matched_values <- df2[match(indices, names(df2))]


df3 <- cbind(df1, matched_values)

print(df3)


# Complete the data frame with missing indices and fill with 0
df_complete <- complete(df3, index = seq(min(index), max(index)), fill = list(value = 0))

# View the complete data frame
df_complete


