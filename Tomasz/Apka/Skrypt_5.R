library('data.table')
library('tidyverse')
library('shiny')
library('plotly')

#           Najbardziej popularne polaczenia w zaleznosci od linii.
przewoznicy <- fread("~/PDU/Pliki/Project/dataverse_files/carriers.csv")
przewoznicy[1309, Description := "US Airways Inc."]
przewoznicy[643, Description := "America West Airlines Inc."]
lotniska <- fread("~/PDU/Pliki/Project/dataverse_files/airports.csv")
loty_00 <- fread("~/PDU/Pliki/Project/dataverse_files/2000.csv.bz2")
loty_00 <- loty_00[Origin == 'ORD',]
loty_00 <- loty_00[Dest == '']
files_names <- paste("~/PDU/Pliki/Project/dataverse_files/", 1987:2008, ".csv.bz2", sep = '')


loty_00[, miasto_skad := lotniska[loty_00, on = .(iata = Origin), city]]
loty_00[, miasto_dokad := lotniska[loty_00, on = .(iata = Dest), city]]
loty_00 <- merge(loty_00, przewoznicy, by.x = "UniqueCarrier", by.y = "Code")
loty_00 <- loty_00[, c("miasto_skad","miasto_dokad","Description")]
loty_00[, polaczenie := paste(miasto_skad, miasto_dokad, sep = ":")]
loty_00 <- loty_00[, -c("miasto_skad", "miasto_dokad")]


polaczenia <- function(file){
  data <- fread(file)
  data[, miasto_skad := lotniska[data, on = .(iata = Origin), city]]
  data[, stan_skad := lotniska[data, on = .(iata = Origin), state]]
  data[, miasto_dokad := lotniska[data, on = .(iata = Dest), city]]
  data[, stan_dokad := lotniska[data, on = .(iata = Dest), state]]
  data <- merge(data, przewoznicy, by.x = "UniqueCarrier", by.y = "Code")
  data <- data[, c("miasto_skad","miasto_dokad","stan_skad","stan_dokad","Description", "Distance")]
  data[, miasto_skad := paste(miasto_skad, "(", stan_skad, ")", sep = '')]
  data[, miasto_dokad := paste(miasto_dokad, "(", stan_dokad, ")", sep = '')]
  data[, polaczenie := paste(miasto_skad, miasto_dokad, sep = " - ")]
  data <- data[, -c("miasto_skad", "miasto_dokad", "stan_skad", "stan_dokad")]
  data <- data[, .(ilosc_lotow = .N), by = c("Description", "polaczenie","Distance")]
  data
}

list_rames <- lapply(files_names, polaczenia)
polaczenia_rama <- rbindlist(list_rames)
polaczenia_rama <- polaczenia_rama[, .(ilosc_lotow = sum(ilosc_lotow)), 
                                   by = c("Description", "polaczenie", "Distance")]
write.table(polaczenia_rama, "polaczenia_rama.csv", row.names = FALSE)

polaczenia_rama <- polaczenia_rama[order(polaczenia_rama$ilosc_lotow, decreasing = TRUE),]
wykres <- plot_ly(data = polaczenia_rama[Description == "US Airways Inc.",],
                  labels = ~polaczenie,
                  values = ~ilosc_lotow,
                  type = 'pie')


#             Mapa cieplna popularności danych linii lotniczych w stanach
