library(httr)
library(rvest)
library(tidyverse)
library(DBI)
library(RMariaDB)

# Function to retrieve data and add timestamp
fetch_air_quality_data <- function(base_url, main_table_url) {
  # Capture the system time at the start of the scraping
  scrape_time <- Sys.time()
  
  res <- GET(base_url)
  html <- content(res, as = "text")
  csrf_token <- read_html(html) %>%
    html_node("input[name='__RequestVerificationToken']") %>%
    html_attr("value")
  
  res_post <- POST(
    url = main_table_url,
    body = list("__RequestVerificationToken" = csrf_token),
    encode = "form"
  )
  
  data <- read_html(content(res_post, as = "text")) %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  
  # Add the scrape time as a new column
  data <- data %>% mutate(Scrape_Time = scrape_time)
  
  return(data)
}

# List of websites
locations <- list(
  HCAB = list(
    base_url = "https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB",
    main_table_url = "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Copenhagen/HCAB"
  ),
  Anholt = list(
    base_url = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/ANHO",
    main_table_url = "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Rural/ANHO"
  ),
  AARH3 = list(
    base_url = "https://envs2.au.dk/Luftdata/Presentation/table/Aarhus/AARH3",
    main_table_url = "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Aarhus/AARH3"
  ),
  RISOE = list(
    base_url = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/RISOE",
    main_table_url = "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Rural/RISOE"
  )
)

# Retrieve data for each location
data_list <- lapply(locations, function(urls) fetch_air_quality_data(urls$base_url, urls$main_table_url))

# Combine the data from the list into a single dataframe
combined_data <- bind_rows(
  lapply(names(data_list), function(station) {
    data_list[[station]] %>%
      mutate(Station = station)
  })
)

combined_data[,2:8] <- lapply(combined_data[,2:8], function(x) {
  as.numeric(gsub(",", ".", x))
})

combined_data$`Målt (starttid)` <- trimws(combined_data$`Målt (starttid)`)
combined_data$`Målt (starttid)` <- as.POSIXct(combined_data$`Målt (starttid)`, format = "%d-%m-%Y %H:%M", tz = "UTC")

unique(combined_data$Station)


##################### Pushing it to SQL
con <- dbConnect(MariaDB(),
                 db = "air_quality_data",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "cFryGv#3174")

### Push the current data
dbWriteTable(con, "air_quality", combined_data, append=TRUE, row.names=FALSE)


##### Pushing the new data
existing_data <- dbGetQuery(con, "SELECT `Målt (starttid)`, Station FROM air_quality")
existing_data$`Målt (starttid)` <- as.POSIXct(existing_data$`Målt (starttid)`, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Identify new data by anti-joining with existing data (excluding Scrape_Time)
new_data <- anti_join(combined_data, existing_data, by = c("Målt (starttid)", "Station"))

# Inserting new data, if there is any
if (nrow(new_data) > 0) {
  dbWriteTable(con, "air_quality", new_data, append = TRUE, row.names = FALSE)
  print("New data added.")
} else {
  print("No new data to insert.")
}

dbDisconnect(con)
