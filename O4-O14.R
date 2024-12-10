library(httr)
library(rvest)
library(tidyverse)
library(XML)

##### Webscrap cars from Germany

DE_biler <- "https://www.autoscout24.com/lst/hyundai?atype=C&custtype=D&cy=D&damaged_listing=exclude&desc=0&fuel=E&ocs_listing=include&powertype=kw&search_id=q4o563gu3i&sort=standard&source=listpage_pagination&ustate=N%2CU"
rawres <- GET(url=DE_biler)
rawres$status_code
rawcontent <- httr::content(rawres,as="text")

# transform text to html-nodes
DE_page <- read_html(rawcontent)

# extract car-elements from the start page
DE_carlist <- DE_page %>% html_elements("article.cldt-summary-full-item")

#Tags
DE_tag_price <- ".PriceAndSeals_wrapper__BMNaJ"
DE_tag_prop <-"span.ListItem_version__5EWfi"
DE_tag_mm <- "[class^='ListItem_header__J6xlG ListItem_header_new_design__Rvyv_']"
DE_tag_detitem <- ".VehicleDetailTable_container__XhfV1"
DE_tag_seller <- ".SellerInfo_wrapper__XttVo .SellerInfo_name__nR9JH"
DE_tag_location <- ".SellerInfo_wrapper__XttVo .SellerInfo_address__leRMu"
DE_tag_carid <-  "a.ListItem_title__ndA4s.ListItem_title_new_design__QIU2b.Link_link__Ajn7I"

# Baseurl for links
baseurl = "https://www.autoscout24.de"

# Initiate the dataframe
DE_biler_df <- as.data.frame(matrix(data=NA, nrow=0, ncol=9))

### Loop to gather the info
for (i in 2:20) {
  # Update the URL with the current page number
  page_url <- paste0("https://www.autoscout24.com/lst/hyundai?atype=C&custtype=D&cy=D&damaged_listing=exclude&desc=0&fuel=E&ocs_listing=include&page=",
                     i,
                     "&powertype=kw&search_id=q4o563gu3i&sort=standard&source=listpage_pagination&ustate=N%2CU")
  
  # Fetch the page content
  rawres <- GET(url = page_url)
  
  # Check if the request was successful
  if (rawres$status_code != 200) {
    print(paste("Failed to fetch page", i))
    next
  }
  
  # Parse the HTML content
  rawcontent <- httr::content(rawres, as = "text")
  DE_page <- read_html(rawcontent)
  
  # Extract car elements
  DE_carlist <- DE_page %>% html_elements("article.cldt-summary-full-item")
  
  # Extract information for each car
  for (car in DE_carlist) {
    tryCatch({
      bilmodel = car %>% html_element(DE_tag_mm) %>% html_text()
      property = car %>% html_element(DE_tag_prop) %>% html_text()
      carid = car %>% html_node(DE_tag_carid) %>% html_attr("href")
      price = car %>% html_node(DE_tag_price) %>% html_text()
      items <- car %>% html_elements(DE_tag_detitem) %>% html_nodes("span") %>% html_text(trim=T) %>% paste0(collapse = "_")
      dealer = car %>% html_node(DE_tag_seller) %>% html_text()
      location = car %>% html_node(DE_tag_location) %>% html_text()
      link = paste0(baseurl, carid)
      
      # Create a temporary data frame with the extracted information
      temp_df <- data.frame(bilmodel, property, carid, price, items, dealer, location, link,  Sys.time(),stringsAsFactors = FALSE)
      DE_biler_df <- rbind(DE_biler_df, temp_df)
      
      # Random sleep to prevent being blocked - per line
      random_sleep <- sample(2:4, 1, replace = TRUE)
      Sys.sleep(random_sleep)
      result <- paste0("System slept for ", random_sleep, " seconds on page ", i)
      print(result)
      
    }, error = function(cond) {
      print(cond)
      message(conditionMessage(cond))
    })
  }
  # Random sleep to prevent being blocked - per page
  Sys.sleep(1)
}

# Remove duplicates to ensure unique cars
DE_biler_df <- DE_biler_df[!duplicated(DE_biler_df$link),]




###################################### Clean the data
#price
DE_clean_price <- round(as.numeric(gsub("[^0-9]", "", DE_biler_df$price))*7.45, digit=0)

#makemodel
DE_pattern_model <- "^(\\w+)\\s+(\\w+(?:\\s\\d)?)\\s+(.+)?"

DE_result_model <- str_match(DE_biler_df$bilmodel, DE_pattern_model)
DE_make <- DE_result_model[,2]
DE_model <- DE_result_model[,3]
DE_rest <- DE_result_model[,4]

#items
DE_pattern_items <- "^([^_]+)_([^_]+)_([^_]+)_([^_]+)_(.+)$"
DE_result_items <- str_match(DE_biler_df$items, DE_pattern_items)

DE_Km = DE_result_items[, 2]
Gear = DE_result_items[, 3]
Registration = DE_result_items[, 4]
Fuel_Type = DE_result_items[, 5]
Horsepower = DE_result_items[, 6]

#Km
DE_Km <- as.numeric(gsub("[^0-9]", "", DE_Km))

#Registration
# current month in number
now <- 2024*12+11

# Function to calculate age in months
DE_age_in_months <- function(Registration) {
  # Split the date string into month and year
  car_date <- str_split(Registration, "/", simplify = TRUE)
  car_month <- as.numeric(car_date[1])
  car_year <- as.numeric(car_date[2])
  ageM <- now-(car_year*12+car_month)
  return(ageM)
}

DE_age_months <- sapply(Registration, age_in_months)

## Get the postal code and city
pattern_DE_location <- "DE-(\\d+)\\s*(.*)"

result_DE_location <- str_match(DE_biler_df$location, pattern_DE_location)
DE_postcode <- result_DE_location[,2]  # Extracted postal code
DE_city<- result_DE_location[, 3]   # Text after the postal code

#### Create a dealer ID for the German list
DE_unique <- unique(DE_biler_df$dealer)
DE_forhandler_ID_df <- data.frame(WW_Dealer_ID=c(1:153), Dealer=DE_unique)


###### Samle renset dataframe
DE_biler_renset_df <- data.frame(Car_ID=c(1:nrow(DE_biler_df)), Price=DE_clean_price, Make=DE_make, Model=DE_model,
                                  Doors=NA, First_registration=Registration, Age_in_months=DE_age_months,
                                  Km=as.numeric(DE_Km), Reach=NA, Description=DE_rest, Gear=Gear, Fuel_type=Fuel_Type, Horsepower=Horsepower,
                                 Dealer_ID=DE_dealer_ID, Link=DE_biler_df$link, Scrapetime=DE_biler_df$Sys.time..)

DE_forhandler_df <- data.frame(Dealer_ID=DE_dealer_ID, Dealer=DE_biler_df$dealer, Street=NA,
                               Post_code=DE_postcode, City=DE_city, Country="Germany",
                               CVR_no=NA, Scrapetime=DE_biler_df$Sys.time..)

DE_dealer_ID <- sapply(DE_forhandler_df$Dealer, function(Dealer) {
  match_row <- DE_forhandler_ID_df[DE_forhandler_ID_df$Dealer == Dealer, ]
  if (nrow(match_row) > 0) {
    return(match_row$WW_Dealer_ID)
  } else {
    return(NA)  # Return NA if no match is found
  }
})

skim(DE_biler_renset_df)
skim(DE_forhandler_df)
