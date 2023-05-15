library('data.table')

# KOMPUTER Z NASA POTRZEBNY!!!
tworzenie_ramki <- function(){
  file_names <- paste("~/PDU/Pliki/Project/dataverse_files/", 1987:2008, ".csv.bz2", sep = '')
  
  data_list <- lapply(file_names, fread)
  
  all_data <- rbindlist(data_list)
}


przewoznicy <- fread("~/PDU/Pliki/Project/dataverse_files/carriers.csv")
przewoznicy[1309, Description := "US Airways Inc."]
przewoznicy[643, Description := "America West Airlines Inc."]


file_names <- paste("~/PDU/Pliki/Project/dataverse_files/", 1987:2008, ".csv.bz2", sep = '')

anulowanie <- function(file){
  data <- fread(file)
  result <- data[Cancelled == 1, .(N_anul_lotow = .N), by = UniqueCarrier]
  result <- merge(result, przewoznicy, by.x = "UniqueCarrier", by.y = "Code")
}
anul_list <- lapply(file_names, anulowanie)
 
lapply(seq_along(anul_list), function(i){
  write.table(anul_list[[i]], paste0("tabela", i,".csv"), sep=",", row.names = FALSE)
})
