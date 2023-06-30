library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(scales)
library(cowplot)

# Store all codes and titles of the graphs
code <- c("PAYEMS", "UNRATE","DGASUSGULF","DCOILBRENTEU","DCOILWTICO","DHHNGSP",
"DDFUELUSGULF","DEXBZUS","DEXCHUS","DEXJPUS","DEXSFUS","DTWEXBGS","DTWEXAFEGS",
"DEXUSAL","DEXUSEU","DEXUSUK","BAMLEMCBPIOAS","BAMLHE00EHYIOAS","BAMLC0A0CM",
"BAMLEMCLLCRPIUSOAS","BAMLH0A0HYM2","T10YIE","T10Y2Y","AAA10Y",
"JPNASSETS","ECBASSETSW","WALCL","CP0000EZ19M086NEST","CLVMEURSCAB1GQEA19",
"CP0000CHM086NEST","CLVMNACSAB1GQCH","CP0000GBM086NEST","UKNGDP","CPILFESL",
"CPIAUCSL","CP0000USM086NEST","INDPRO","PPIACO","GDPC1","RSXFS","BOPGSTB","M2V",
"T10Y3M","DGS10","DJIA","NASDAQCOM","SP500","GFDEGDQ188S","VIXCLS","WSHOMCB",
"TCU","DFEDTARU","DFEDTARL","GGGDTPARA188N","RAILFRTCARLOADSD11","UMCSENT","SOFR")

n <- length(code)

# Your API key
apikey <-  "8efadd61dc7028ae9148d6c7a307f018"

# Create a pdf, specify width and height
pdf("output.pdf", width = 10, height = 4)

# A special case for PAYEMS
url <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=PAYEMS&api_key=", apikey, "&file_type=json")
content_from_json <- fromJSON(content(GET(url), as = "text"))
data <- data.frame(date = content_from_json$observations$date, value = content_from_json$observations$value)
data$date <- as.Date(data$date)
data$value <- as.numeric(data$value)
data$change <- c(NA, diff(data$value))
data <- na.omit(data)

# Make the plot 
p <- ggplot(data, aes(x = date, y = change)) + 
    geom_line(color = "blue") +
    scale_x_date(date_labels = "%Y", date_breaks = "10 year") +
    scale_y_continuous(breaks = seq(min(data$change), max(data$change), by = 4000)) +
    xlab("Date") +
    ylab("Change, Thousands of Persons") +
    ggtitle("All Employees, Total Nonfarm, Thousands of Persons, Seasonally Adjusted (PAYEMS)") +
    geom_hline(yintercept = 0, color = "black")

# Print it on PDF
print(p)

# Loop the rest of the codes
for (i in 2:n){
    url <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=", code[i],"&api_key=", apikey, "&file_type=json")
    content_from_json <- fromJSON(content(GET(url), as = "text"))
    data <- data.frame(date = content_from_json$observations$date, value = content_from_json$observations$value)
    data <- na.omit(data) 
    data$date <- as.Date(data$date)
    data$value <- as.numeric(data$value)
    data <- data %>% arrange(date)

    # Load the title and units
    url_info <- paste0("https://api.stlouisfed.org/fred/series?series_id=", code[i], "&api_key=", apikey, "&file_type=json")
    info_content <- fromJSON(content(GET(url_info), as = "text"))
    title <- info_content$seriess$title
    units <- info_content$seriess$units

    # # Take log of the value for stocks
    # if (code[i] %in% c("DJIA", "NASDAQCOM", "SP500")){
    #     data$value <- ifelse(data$value > 0, log10(data$value), NA)
    # }
    # # Want axis to be original values

    p <- ggplot(data, aes(x = date, y = value)) + 
        geom_line(color = ifelse(code[i] %in% c("CPILFESL", "DFEDTARU"), "red", "blue")) +
        # The plots go together need to have different colours. See result PDF.
        scale_x_date(date_labels = "%Y", date_breaks = "10 year") +
        scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        xlab("Date") +
        ylab(units) +
        ggtitle(title)

    if (code[i] %in% c("DJIA", "NASDAQCOM", "SP500")){
        p <- p + scale_y_log10()
    }

    print(p)

    # Some plots need to be plot together on a third plot
    if (code[i] %in% c("CPIAUCSL", "DFEDTARL")){
        url <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=", code[i-1],"&api_key=", apikey, "&file_type=json")
        content_from_json <- fromJSON(content(GET(url), as = "text"))
        data <- data.frame(date = content_from_json$observations$date, value = content_from_json$observations$value)
        data <- na.omit(data) 
        data$date <- as.Date(data$date)
        data$value <- as.numeric(data$value)
        data <- data %>% arrange(date)
        
        combined_plot <- p +
            geom_line(data = data, aes(y = value, color = "red")) +
            ggtitle("Combined plot")

        print(combined_plot)
    }
}

dev.off()