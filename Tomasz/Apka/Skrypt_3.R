library('data.table')
library('tidyverse')
library('shiny')

samoloty <- fread("~/PDU/Pliki/Project/dataverse_files/plane-data.csv", fill = TRUE)
przewoznicy <- fread("~/PDU/Pliki/Project/dataverse_files/carriers.csv")
loty_97 <- fread("~/PDU/Pliki/Project/dataverse_files/1997.csv.bz2")
loty_06 <- fread("~/PDU/Pliki/Project/dataverse_files/2006.csv.bz2")
loty_87 <- fread("~/PDU/Pliki/Project/dataverse_files/1987.csv.bz2")
loty_01 <- fread("~/PDU/Pliki/Project/dataverse_files/2001.csv.bz2")

loty_01 <- loty_01[,c(5:8, 12:16)]

loty_87 <- loty_87[Cancelled == 1 & TailNum != "UNKNOW", 
                   -c("DepTime", "ArrTime", "ActualElapsedTime", "AirTime", "TaxiIn", "TaxiOut", "ArrDelay", 
                      "DepDelay", "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay")]
loty_97 <- merge(loty_97, przewoznicy, by.x = "UniqueCarrier", by.y = "Code")
loty_97 <- merge(loty_97, samoloty[,-"status"], by.x = "TailNum", by.y = "tailnum")
loty_97 <- loty_97[, data := paste(Year, Month, DayofMonth, sep = "-")]
loty_97 <- loty_97[,-c("Year", "Month", "DayofMonth")]

odczytanko <- function(file){
  ramka <- fread(file)
  ramka <- ramka[Cancelled == 1 & TailNum != "UNKNOW", 
                 -c("DepTime", "ArrTime", "ActualElapsedTime", "AirTime", "TaxiIn",
                    "TaxiOut", "ArrDelay", "DepDelay", "CarrierDelay", "WeatherDelay",
                    "NASDelay", "SecurityDelay", "LateAircraftDelay")]
  ramka <- merge(ramka, przewoznicy, by.x = "UniqueCarrier", by.y = "Code")
  ramka <- merge(ramka, samoloty[,-"status"], by.x = "TailNum", by.y = "tailnum")
  ramka <- ramka[, data := paste(Year, Month, DayofMonth, sep = "-")]
  ramka <- ramka[,-c("Year", "Month", "DayofMonth")]
}


lista_plikow <- paste("~/PDU/Pliki/Project/dataverse_files/", 1987:2008, ".csv.bz2", sep = '')
lista_danych <- lapply(lista_plikow, odczytanko)
lista_danych <- lista_danych[-c(1:8)]

rama <- rbindlist(lista_danych)
