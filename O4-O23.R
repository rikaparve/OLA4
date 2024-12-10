library(tidyverse)
library(diffdf)

# Scrapetime is different for both, so I remove to ensure we look for real changes
bil_renset_df2 <- bil_renset_df[,-16]
new_bil_renset_df2 <- new_bil_renset_df[,-16]

# Check if they are identical
identical(bil_renset_df2, new_bil_renset_df2)
# FALSE

# Identify rows that are removed or added
lv_removed_rows <- bil_renset_df$Car_ID %in% new_bil_renset_df$Car_ID
removed_rows <- bil_renset_df[!lv_removed_rows,]

lv_added_rows <- new_bil_renset_df$Car_ID %in% bil_renset_df$Car_ID
added_rows <- new_bil_renset_df[!lv_added_rows,]

# Find the rows with changes in them

# Initialize an empty data frame with the same structure as new_bil_renset_df2
different_car_df <- new_bil_renset_df2[0, ]

# Loop through each row in bil_renset_df2
for (i in 1:nrow(bil_renset_df2)) {
  # Get the current Car_ID
  car_id <- bil_renset_df2$Car_ID[i]
  
  # Find the matching row in new_bil_renset_df2
  match_index <- which(new_bil_renset_df2$Car_ID == car_id)
  
  # If a match is found
  if (length(match_index) == 1) {
    # Compare the corresponding rows (excluding Car_ID column)
    row_original <- bil_renset_df2[i, -which(names(bil_renset_df2) == "Car_ID")]
    row_new <- new_bil_renset_df2[match_index, ]
    
    # Check if the rows are identical
    if (!identical(as.character(unlist(row_original)), as.character(unlist(row_new[-which(names(row_new) == "Car_ID")])))) {
      # If rows are not identical, append the row from new_bil_renset_df2
      different_car_df <- rbind(different_car_df, row_new)
    }
  }
}

#### Prepare the new and adjusted data for SQL
add_cars_df <- data.frame(Car_ID=added_rows$Car_ID, Make=added_rows$Make, Model=added_rows$Model,
                      Doors=added_rows$Doors, First_registration=added_rows$First_registration,
                      Km=added_rows$Km, Reach=added_rows$Reach,
                      Gear=added_rows$Gear, Fuel_type=added_rows$Fuel_type, Horsepower=added_rows$Horsepower,
                      Dealer_ID=added_rows$Dealer_ID, Link=added_rows$Link)

changes_df <- rbind(added_rows[,-16], different_car_df)

changes_df <- data.frame(variant_ID=(nrow(allcars_df)+1):(nrow(allcars_df)+5),
                             Car_ID=changes_df$Car_ID, Price=changes_df$Price,
                             Age_in_months=changes_df$Age_in_months, 
                             Sales_text=changes_df$Description, sold=FALSE, 
                             latest=TRUE, timestamp=Sys.time())

remove_df <- data.frame(variant_ID=(nrow(allcars_df)+6):(nrow(allcars_df)+10),
                        Car_ID=removed_rows$Car_ID, Price=removed_rows$Price,
                         Age_in_months=removed_rows$Age_in_months, 
                         Sales_text=removed_rows$Description, sold=TRUE, 
                         latest=TRUE, timestamp=Sys.time())

add_car_variant_df <- rbind(changes_df, remove_df)


#### Establish connection
library(DBI)
library(RMariaDB)

con <- dbConnect(MariaDB(),
                 db = "bilbasen",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "")

dbWriteTable(con, "cars", add_cars_df, append = TRUE, row.names = FALSE)
dbWriteTable(con, "car_variant_data", add_car_variant_df, append = TRUE, row.names = FALSE)


