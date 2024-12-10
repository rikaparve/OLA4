############## Opgave 1: Bilbasen
library(httr)
library(rvest)
library(tidyverse)
library(XML)
library(skimr)

# First page
page1 <- "https://www.bilbasen.dk/brugt/bil/hyundai?fuel=3&includeengroscvr=true&includeleasing=false"
rawres <- GET(url=page1)
rawres$status_code
rawcontent <- httr::content(rawres,as="text")

# transform text to html-nodes
page <- read_html(rawcontent)

# extract car-elements from the start page
# Formodning om at kun bilerne er stored som attr = article
carlist <- page %>% html_elements("article")

# tag list
tag_price <- ".Listing_price__6B3kE"
tag_prop <- ".Listing_properties___ptWv"
tag_mm <- "[class^='Listing_makeModel']"
tag_det <- "[class^='Listing_details']"
tag_detitem <- "[class^='ListingDetails_listItem']"
tag_desc <- "[class^='Listing_description']"
tag_loc <- "[class^='Listing_location']"

# Set the base URL (up to the page parameter part)
base_url <- "https://www.bilbasen.dk/brugt/bil/hyundai?fuel=3&includeengroscvr=true&includeleasing=false&page="

# Initialize the column names for the dataframe
cn <- c("price", "property", "model", "detailitems", "description", "location", "link", "carid", "scrapedate")
bil_df <- as.data.frame(matrix(data=NA, nrow=0, ncol=9))
colnames(bil_df) <- cn

# Loop through each page number
for (page_num in 1) {
  # Update the URL with the current page number
  page_url <- paste0(page1)
  
  # Fetch the page content
  rawres <- GET(url = page_url)
  
  # Check if the request was successful
  if (rawres$status_code != 200) {
    print(paste("Failed to fetch page:", page_num))
    next
  }
  
  # Parse the HTML content
  rawcontent <- httr::content(rawres, as = "text")
  page <- read_html(rawcontent)
  
  # Extract car elements
  carlist <- page %>% html_elements("article")
  
  # Extract information for each car
  for (car in carlist) {
    tryCatch({
      price <- car %>% html_element(tag_price) %>% html_text()
      prop <- car %>% html_element(tag_prop) %>% html_text()
      makemodel <- car %>% html_element(tag_mm) %>% html_text()
      items <- car %>% html_elements(tag_detitem) %>% html_text() %>% paste0(collapse = "_")
      description <- car %>% html_elements(tag_desc) %>% html_text()
      location <- car %>% html_elements(tag_loc) %>% html_text()
      link <- car %>% html_element("a") %>% html_attr("href")
      carid <- link %>% str_extract("[0-9]{7}")
      
      # Create a temporary data frame with the extracted information
      temp_df <- data.frame(price, prop, makemodel, items, description, location, link, carid, Sys.time())
      bil_df <- rbind(bil_df, temp_df)
      
      # Random sleep to prevent being blocked - per line
      random_sleep <- sample(2:5, 1, replace = TRUE)
      Sys.sleep(random_sleep)
      result <- paste0("System slept for ", random_sleep, " seconds on page ", page_num)
      print(result)
      
    }, error = function(cond) {
      print(cond)
      message(conditionMessage(cond))
    })
  }
  # Random sleep to prevent being blocked - per page
  Sys.sleep(1)
}

# Tilføj første side
# Rens til at sikre unike biler
bil_df <- bil_df[!duplicated(bil_df$carid), ]
length(unique(bil_df$carid))

##################################################
####################### Create a new clean dataframe

# Make the price column numeric
clean_price <- as.numeric(gsub("[^0-9]", "", bil_df$price))

# extract make, model and number of doors from makemodel
result_make <- str_match(bil_df$makemodel, "^\\s*(\\S+)" )
make <- result_make[,2]
result_model <- str_match(bil_df$makemodel, "^\\s*\\S+\\s+(\\S+\\d|\\S+\\s+\\d)" )
model <- result_model[,2]
result_doors <- str_match(bil_df$makemodel, "(\\d)[dD]$" )
doors <- result_doors[,2]


### Define the pattern to capture each part for three underscores
pattern_items3 <- "^([^_]+)_([0-9.]+) km_([0-9.]+) km"

# Define the pattern for two underscores
pattern_items2 <- "^([0-9.]+) km_([0-9.]+) km"

# Initialize empty columns
age <- NA
Km <- NA
rækkevide <- NA

# Loop through each row to apply the correct pattern
for (i in 1:nrow(bil_df)) {
  item <- bil_df$items[i]
  
  if (str_count(item, "_") == 3) {
    # Apply three-underscore pattern
    result <- str_match(item, pattern_items3)
    age[i] <- result[2]
    Km[i] <- result[3]
    rækkevide[i] <- result[4]
  } else if (str_count(item, "_") == 2) {
    # Apply two-underscore pattern
    result <- str_match(item, pattern_items2)
    age[i] <- NA          # No age info available
    Km[i] <- result[2]    # Extract kilometers
    rækkevide[i] <- result[3]  # Extract rækkevide
  }
}


#### Calculate the car age in months
# current month in number
now <- 2024*12+11

# Function to calculate age in months
age_in_months <- function(age) {
  # Split the date string into month and year
  car_date <- str_split(age, "/", simplify = TRUE)
  car_month <- as.numeric(car_date[1])
  car_year <- as.numeric(car_date[2])
  ageM <- now-(car_year*12+car_month)
  return(ageM)
}

age_months <- sapply(age, age_in_months)

###### Separate the two info in the column location
pattern_location <- "^([^,]+),\\s*([A-Za-zæøåÆØÅ -]+)$"

# Apply the pattern to extract the components
result_locations <- str_match(bil_df$location, pattern_location)

# Extract the components into separate variables
By <- result_locations[, 2]   # Everything before the comma (City)
Region <- result_locations[, 3] # Everything after the comma (Region)

#### Dealer ID

biler_dealer_ID <- sapply(bil_renset_df$Link, function(Link) {
  match_row <- forhandler_renset_df[forhandler_renset_df$Link == Link, ]
  if (nrow(match_row) > 0) {
    return(match_row$WW_Dealer_ID)
  } else {
    return(NA)  # Return NA if no match is found
  }
})

#Put all the cleaned data in a new dataframe
bil_renset_df <- data.frame(Car_ID=bil_df$carid, Price=clean_price, Make=make, Model=model,
                            Doors=doors, First_registration=age, Age_in_months=age_months,
                            Km=as.numeric(Km)*1000, Reach=rækkevide,
                            Description=bil_df$description, Gear=as.character("NA"), Fuel_type="Electric", Horsepower=NA,
                            Dealer_ID=biler_dealer_ID, Link=bil_df$link, Scrapetime=bil_df$Sys.time..)

row.names(bil_renset_df) <- NULL
bil_renset_df <- bil_renset_df[!is.na(forhandler_id),]
skim(bil_renset_df)
