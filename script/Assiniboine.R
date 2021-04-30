### Assiniboine Delta Aquifer hydrometric data (including baseflow and quickflow) and climate data ###
# Written by: Ethan McTavish

# Library's -----
library(weathercan)
library(tidyhydat)
library(EcoHydRology)
library(tidyverse)
library(here)
library(rgdal)
library(leaflet)

# read in climate and hydrometric stations -----
stationsADAhy <- read_csv(here("data","stationsADAhy.csv"))
stationsADAcl <- read_csv(here("data","stationsADAcl.csv"))

# import hydrometric data by station -----
error <- NULL
hy_stn_list <- list()
for (i in 1:nrow(stationsADAhy)) {
  
  skip_to_next <- FALSE
  
  tryCatch({nam <- paste(stationsADAhy[i,2])
  
  hy_stn_list[[i]] <- assign(nam, hy_monthly_flows(station_number = stationsADAhy[i,2]))
  
  }, error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { error[i] <- i
  next }     
}

# clean/modify the hydrometric data -----
# you only need to remove NULL if there are empty df in the list

#convert STATION_NUMBER column to a vector
stationsADAhy_vec <- stationsADAhy[["STATION_NUMBER"]]

# combine list into one dataframe by STATION_NUMBER
ADA_hy <- hy_stn_list %>%
  set_names(stationsADAhy_vec) %>%
  bind_rows(.id = "STATION_NUMBER") 

# writing hydrometric .csv files -----

# writing multiple .csv files
for (i in 1:length(hy_stn_list)) {
  
  write_csv(hy_stn_list[[i]], paste("data/ADAhy/", stationsADAhy[i,2] ,  ".csv"))
  
}

file.exists("data/ADAhy")

# write one big .csv file
write_csv(ADA_hy, "data/ADAhy/ADA_hy_data.csv")

# add baseflow and quickflow -----

ADA_hy_clean <- ADA_hy %>%
  filter(Sum_stat == "MEAN") %>%
  select(-Date_occurred) %>%
  drop_na(c(Value))

ADA_bf_sep <- BaseflowSeparation(ADA_hy_clean$Value, filter_parameter = 0.925, passes = 3)

ADA_bf_bind <- cbind(ADA_hy_clean, ADA_bf_sep)

ADA_hy_flow <- rename(ADA_bf_bind, c(Streamflow = Value, Baseflow = bt, Quickflow = qft)) # a file with monthly streamflow, quickflow and baseflow

# write .csv
write_csv(ADA_hy_flow, "data/ADAhy/ADA_hy_flow.csv")


# import climate data by station -----
error <- NULL
cl_stn_list <- list()
for (i in 1:nrow(stationsADAcl)) {
  
  skip_to_next <- FALSE
  
  tryCatch({nam <- paste(stationsADAcl[i,4])
  
  cl_stn_list[[i]] <- assign(nam, weather_dl(station_ids = stationsADAcl[i,4], interval = "day"))
  
  }, error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { error[i] <- i
  next }     
}

# clean/modify the climate data -----

#convert Station_id column to a vector
statNumberVecCl <- stationsADAcl[["station_id"]]

# combine list into 1 dataframe by STATION_NUMBER
ADA_cl_Stn <- cl_stn_list %>%
  set_names(statNumberVecCl) %>%
  bind_rows(.id = "Station_id")

# writing climate .csv files -----

# writing multiple .csv files
for (i in 1:length(cl_stn_list)) {
  
  write_csv(cl_stn_list[[i]], paste("data/ADAcl/", stationsADAcl[i,4] ,  ".csv"))
  
}

# write one big .csv file

write_csv(ADA_cl_Stn, "data/ADAcl/ADA_Cl_data.csv")

