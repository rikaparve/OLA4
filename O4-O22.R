###### Bilbasen og tyske biler til SQL
# Put the DK and DE cars in one dataframe
allcars_df <- rbind(bil_renset_df, DE_biler_renset_df)
skim(allcars_df)
summary(allcars_df)

# Put all DK and DE dealers in one dataframe and remove all duplicates
alldealers_df <- rbind(forhandler_renset_df, DE_forhandler_df)
alldealers_df <- alldealers_df[!duplicated(alldealers_df$Dealer_ID), ]
row.names(alldealers_df) <- NULL

skim(alldealers_df)

# Establish a connection
library(DBI)
library(RMariaDB)

con <- dbConnect(MariaDB(),
                 db = "bilbasen",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "")

sql_bilbasen <- dbReadTable(con,"dealers")

#INSERT INTO table_name [colnames, ...]
#SELECT colnames, ...
#FROM table_name

# Insert data into the `dealers` table
if (nrow(alldealers_df) > 0) {
  dbWriteTable(con, "dealers", alldealers_df[, c("Dealer_ID", "Dealer", "Street", "Post_code", "City", "Country", "CVR_no")], 
               row.names = FALSE, append = TRUE)
}

# Prepare the cars data for insertion
cars_df <- data.frame(Car_ID=allcars_df$Car_ID, Make=allcars_df$Make, Model=allcars_df$Model,
                       Doors=allcars_df$Doors, First_registration=allcars_df$First_registration,
                       Km=allcars_df$Km, Reach=allcars_df$Reach,
                       Gear=allcars_df$Gear, Fuel_type=allcars_df$Fuel_type, Horsepower=allcars_df$Horsepower,
                       Dealer_ID=allcars_df$Dealer_ID, Link=allcars_df$Link)

# Insert data into the `cars` table
if (nrow(cars_df) > 0) {
  dbWriteTable(con, "cars", cars_df, row.names = FALSE, append = TRUE)
}

# Prepare the car_variant_data for insertion
car_variant_df <- data.frame(variant_ID=nrow(allcars_df):1, Car_ID=allcars_df$Car_ID, Price=allcars_df$Price, Age_in_months=allcars_df$Age_in_months, Sales_text=allcars_df$Description, sold=FALSE, latest=TRUE, timestamp=allcars_df$Scrapetime)

# Insert data into the `car_variant_data` table
if (nrow(car_variant_df) > 0) {
  dbWriteTable(con, "car_variant_data", car_variant_df, row.names = FALSE, append = TRUE)
}

# Close the connection
dbDisconnect(con)

