### Opgave 1.3 – Hente nye data - simuleret ###

new_bil_renset_df <- bil_renset_df
colnames(new_bil_renset_df)

# ny scrapedate
new_bil_renset_df$Scrapetime <- new_bil_renset_df$Scrapetime + 24 * 60 * 60

# fjern 5 tilfældige rækker
new_bil_renset_df <- new_bil_renset_df[-sample(nrow(new_bil_renset_df), 5), ]

# ændre prisen på 3 rækker
str(new_bil_renset_df$Price)
new_bil_renset_df$Price[c(20, 40, 60)] <- as.numeric(c(200000, 180000, 250000))

# 2 rækker med nye biler
nye_rækker <- data.frame(
  Car_ID=c(1234567, 7654321),
  Price = as.numeric(c(150000, 200000)),
  Make = "Hyundai",
  Model = c("IONIQ","KONA"),
  Doors = "5",
  First_registration = c("7/2022","1/2024"),
  Age_in_months=c((now-(2022*12+7)),(now-(2024*12+1))),
  Km = c(52000, 5000),
  Reach = c(570, 450),
  Description = c("Fin bil i god stand, kun 1 ejer", "Meget velholdt bil med lav kilometerstand"),
  Gear="Automatic",
  Fuel_type="Electric",
  Horsepower=NA,
  Dealer_ID=c(7396, 10114),
  Link = c("https://www.example.com/car1234567","https://www.example.com/car7654321"),
  Scrapetime = Sys.time()
)

new_bil_renset_df <- rbind(new_bil_renset_df, nye_rækker)
