library('data.table')
library('tidyverse')
library('shiny')
library('plotly')

samoloty <- fread("~/PDU/Pliki/Project/dataverse_files/plane-data.csv", fill = TRUE)
przewoznicy <- fread("~/PDU/Pliki/Project/dataverse_files/carriers.csv")
przewoznicy[1309, Description := "US Airways Inc."]
przewoznicy[643, Description := "America West Airlines Inc."]
loty_00 <- fread("~/PDU/Pliki/Project/dataverse_files/2000.csv.bz2")
file_names <- paste("~/PDU/Pliki/Project/dataverse_files/", 1987:2008, ".csv.bz2", sep = '')

anulowanie <- function(file){ # zlicza odrazu wszystkie na dany rok
  result <- fread(file)
  result <- result[Cancelled == 1, .(N_anul_lotow = .N), by = UniqueCarrier]
  result <- merge(result, przewoznicy, by.x = "UniqueCarrier", by.y = "Code")
}

anulowanie_data <- function(file){ # każdy anulowany lot na dni
  result <- fread(file)
  result <- result[Cancelled == 1, 
                   -c("DepTime", "ArrTime", "ActualElapsedTime", "AirTime", "TaxiIn", "TaxiOut", "ArrDelay", 
                      "DepDelay", "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay")]
  result <-  result[, data := as.Date(paste(Year, Month, DayofMonth, sep = "-"))]
  result <- result[,-c("Year", "Month", "DayofMonth")]
  result <- merge(result, przewoznicy, by.x = "UniqueCarrier", by.y = "Code")
}


anul_list <- lapply(file_names, anulowanie)

# *----------------------------------------------------------------------------------------
anul_rama <- rbindlist(anul_list)
anul_rama_lacznie <- anul_rama[, sum(N_anul_lotow), by = Description] # ramka z wszystkich lat
setnames(anul_rama_lacznie, "V1", "Liczba_anul")
anul_rama_lacznie[6, Description := "America West Airlines Inc."]
anul_rama_lacznie[13, Description := "US Airways Inc."]

wykres <- plot_ly(
  data = anul_rama_lacznie,
  labels = ~Description,
  values = ~Liczba_anul,
  type = 'pie'
)

write.table(anul_rama_lacznie, "anul_lacznie.csv", row.names = FALSE)
# *----------------------------------------------------------------------------------------


# *----------------------------------------------------------------------------------------
anul_data_list <- lapply(file_names, anulowanie_data)
anul_data_rama <- rbindlist(anul_data_list)
anul_data_rama <- anul_data_rama[, .(N_anul_lotow = .N), by = .(Description, data)]
write.table(anul_data_rama, "anul_data.csv", row.names = FALSE)


ggplot(anul_data_rama) +
  geom_smooth(mapping = aes(x = data, y = N_anul_lotow, color = Description), se = FALSE)
# *----------------------------------------------------------------------------------------