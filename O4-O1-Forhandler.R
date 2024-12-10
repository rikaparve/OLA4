################### Tilføj forhandler##########################################
tag_forhandler <- "[class^='bas-MuiTypography-root bas-MuiTypography-h3']"
tag_forhandler2 <- "[class^='bas-MuiTypography-root bas-MuiVipSectionComponent-sectionHeader bas-MuiTypography-h3']"
tag_id <- "[class^='bas-MuiSellerInfoComponent-sellerInfo']"
tag_addr <- "[class^='bas-MuiSellerInfoComponent-sellerLink bas-MuiSellerInfoComponent-addressWrapper']"
tag_cvr <- ".bas-MuiSellerInfoComponent-cvr"

# Initialize the column names for the dataframe
cn_forhandler <- c("forhandler", "forhandler_id", "address", "CVR_no", "link", "Sys.time")
forhandler_df <- as.data.frame(matrix(data=NA, nrow=0, ncol=6))
colnames(forhandler_df) <- cn_forhandler


# Loop through each row
for (i in 1:nrow(bil_df)) {
  # Update the URL with the current page number
  page_url <- paste0(bil_df$link[i])
  
  # Fetch the page content
  rawres <- GET(url = page_url)
  
  # Check if the request was successful
  if (rawres$status_code != 200) {
    print(paste("Failed to fetch link:", page_url))
    next
  }
  
  # Parse the HTML content
  rawcontent <- httr::content(rawres, as = "text")
  page <- read_html(rawcontent)
  
  # Extract car elements
  forhandlerlist <- page %>% html_elements("article")
  
  # Extract information for each car
  for (forhandler in forhandlerlist) {
    tryCatch({
      name <- tryCatch({
        forhandler %>%
          html_element(tag_forhandler) %>%
          html_text()
      }, error = function(e) NA)
      
      if (is.na(name) || name == "") {
        name <- tryCatch({
          # Hent alle forekomster af tag_forhandler2
          all_names <- forhandler %>%
            html_elements(tag_forhandler2) %>%
            html_text()
          
          #Vælg den anden forekomst, hvis der er mindst to
          if (length(all_names) >= 6) {
            all_names[6]
          } else {
            NA  # Returnér NA, hvis der ikke er to forekomster
          }
        }, error = function(e) NA)
      }
      address <- forhandler %>% html_element(tag_addr) %>% html_text(trim=T)
      id <- forhandler %>% html_element(tag_id) %>% html_node("a") %>% html_attr("href")
      CVR <- forhandler %>% html_element(tag_cvr) %>% html_text(trim=T)
      car_ID=bil_df$carid[i]
      
      # Create a temporary data frame with the extracted information
      temp_df <- data.frame(name, id, address, CVR, page_url, Sys.time(),stringsAsFactors = FALSE)
      forhandler_df <- rbind(forhandler_df, temp_df)
      
      # Random sleep to prevent being blocked - per line
      random_sleep <- sample(2:4, 1, replace = TRUE)
      Sys.sleep(random_sleep)
      result <- paste0("System slept for ", random_sleep, " seconds on car ", page_url)
      print(result)
      
    }, error = function(cond) {
      print(cond)
      message(conditionMessage(cond))
    })
  }
  # Random sleep to prevent being blocked - per page
  Sys.sleep(1)
}

######################## Clean the forhandler dataframe and create a new clean one
# Extract ID from the link
pattern_id <- "(\\d+)$"
forhandler_id <- str_match(forhandler_df$id, pattern_id)[,2]

##### Divide address into 3 columns
pattern_address <- "^(.*), (\\d{4}) (.+)$"
result_address <- str_match(forhandler_df$address, pattern_address)

# Extract the components into separate variables
street <- result_address[,2]
postcode <- result_address[,3]
city <- result_address[,4]

#### CVR number
result_CVR <- str_match(forhandler_df$CVR, pattern_id)[,2]

### New clean dataframe
forhandler_renset_df <- data.frame(Dealer_ID=forhandler_id, Dealer=forhandler_df$name, Street=street,
                                   Post_code=postcode, City=city, Country="Denmark", 
                                   CVR_no=result_CVR, Scrapetime=forhandler_df$Sys.time..)

forhandler_renset_df <- forhandler_renset_df[!is.na(forhandler_id),]
skim(forhandler_renset_df)
