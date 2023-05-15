loty_08 <- fread('~/PDU/Pliki/Project/dataverse_files/2008.csv.bz2')
loty_08 <- loty_08[Cancelled == 0,]
lotniska <- read.csv('~/PDU/Pliki/Project/dataverse_files/airports.csv')
samoloty <- read.csv('~/PDU/Pliki/Project/dataverse_files/plane-data.csv')
przewoznicy <- read.csv('~/PDU/Pliki/Project/dataverse_files/carriers.csv')
file_names <- paste("~/PDU/Pliki/Project/dataverse_files/", 1987:2008, ".csv.bz2", sep = '')

weekendy <- loty_08 %>% filter(DayOfWeek == 6 | DayOfWeek == 7)
tydzien <- loty_08 %>% filter(DayOfWeek != 6 & DayOfWeek != 7)

siema <- loty_08 %>% mutate(data = as.Date(sprintf("%d-%02d-%02d",
                                           loty_08$Year,
                                           loty_08$Month,
                                           loty_08$DayofMonth)))
siema <- siema %>% select(Origin, Dest, data)
write.csv(siema, file = 'przyloty_odloty.csv', row.names = FALSE)

skad <- siema %>% count(Origin, name = "Liczba_odlotow")
dokad <- siema %>% count(Dest, name = "Liczba_przylotow")

obchodzi <- left_join(skad, dokad, by = join_by(Origin == Dest))
  
obchodzi <- right_join(select(lotniska, iata, airport, city, state), siema,
                      by = join_by(iata == Origin))
obchodzi <- obchodzi %>% mutate(roznica = obchodzi$Liczba_przylotow-obchodzi$Liczba_odlotow)

write.csv(siema, file = 'przyloty_odloty.csv', row.names = FALSE)

barplot(height = obchodzi[obchodzi$state == 'TX',]$`Liczba odlotów`,
        las = 2,
        names.arg = obchodzi[obchodzi$state == 'TX',]$airport,
        ylab = "Liczba odlotów",
        xlab = "Lotniska",
        main = "Liczba odlotów z danych lotnisk")

names(table(obchodzi$state))


przyloty_odloty <- function(file){
  data <- fread(file)
  data[Cancelled == 0,]
  data[, data := as.Date(paste(Year, Month, DayofMonth, sep = "-"))]
  data[,c("data", "Origin", "Dest")]
  data
}
