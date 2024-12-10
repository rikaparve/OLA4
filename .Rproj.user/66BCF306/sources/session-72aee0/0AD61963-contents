library(httr)
library(rvest)
library(tidyverse)

# Function to retrieve data
fetch_air_quality_data <- function(base_url, main_table_url) {
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

##################### Pushing it to SQL
library(DBI)
library(RMariaDB)

con_ubuntu <- dbConnect(MariaDB(),
                 db = "air_quality_data",
                 host = "51.20.185.161",
                 port = 3306,
                 user = "redrika",
                 password = "")

print(dbListTables(con_ubuntu))

### Push the current data
dbWriteTable(con_ubuntu, "air_quality", combined_data, append=TRUE, row.names=FALSE)

##### Pushing new data in OLA5, opgave 2
