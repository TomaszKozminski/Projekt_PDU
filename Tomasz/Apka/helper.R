przyloty_odloty <- function(file){
  data <- fread(file)
  data[Cancelled == 0,]
  data[, data := as.Date(paste(Year, Month, DayofMonth, sep = "-"))]
  data <- data[, c("data", "Origin", "Dest")]
  data <- data[order(data),]
  data
}

ramka <- function(data_start = "2007-01-01", data_konc = "2007-01-01", loty){
  loty <- loty[data >= data_start,]
  loty <- loty[data <= data_konc,]
  skad <- loty[, .(Liczba_odlotow = .N), by = Origin]
  dokad <- loty[data >= data_start & data <= data_konc, .(Liczba_przylotow = .N), by = Dest]
  skad <- merge(skad, dokad, by.x = "Origin", by.y = "Dest")
  skad <- merge(lotniska[,c("iata", "airport", "state")], skad, by.x = "iata", by.y = "Origin")
  skad[, roznica := Liczba_przylotow - Liczba_odlotow]
  skad
}

kasowanie <- function(rameczka, stan){
  rameczka <- rameczka[rameczka$state == stan,]
  rameczka <- na.omit(rameczka)
  rameczka
}