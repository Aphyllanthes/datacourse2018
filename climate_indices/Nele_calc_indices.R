library(tidyverse)
library(readr)

# This script runs all the functions for the indices and calculates all indices for all stations whithin a loop, with a data.frame as result. 
## The data path to the BW_climate-data ant the path to the functions for the climate_indices has to be set.

# ----- data path-----
path <- "/Users/Nele/Documents/Uni/Master/Data/BigData/BW_Climate_1977_2016.txt"
BW_C <- read_tsv(path)

#path to climate indices:
path_ci <- "/Users/Nele/Desktop/BackupNele-branch/"



#amount.of.indices <- 10
station.ids <- unique(BW_C$id)
number.of.stations <- length(station.ids)
#indices <- c("Fd", "ETR", "GSL", "HWDI", "Tn90", "R10", "CDD", "R5d", "SDII", "R95T")

avg.result.frame <- data.frame(Station_IDs = station.ids, Fd = NA, ETR = NA, GSL = NA, HWDI = NA,
                               Tn90 = NA, R10 = NA, CDD = NA, R5d = NA, SDII = NA, R95T = NA)

rate.of.change.frame <- data.frame(Station_IDs = station.ids, Fd = NA, ETR = NA, GSL = NA, HWDI = NA,
                             Tn90 = NA, R10 = NA, CDD = NA, R5d = NA, SDII = NA, R95T = NA)

# --------- Load all Indice-functions ------------------
source(paste0(path_ci, "CDD.R"))
source(paste0(path_ci, "ETR.R"))

source(paste0("climate_indices/Fd.R"))

source(paste0(path_ci, "R10.R"))
source(paste0(path_ci, "R5d.R"))
source(paste0(path_ci, "R95T.R"))
source(paste0(path_ci, "SDII.R"))
source(paste0(path_ci, "Tn90.R"))

source(paste0( "climate_indices/GSL_Nele.R"))
source(paste0( "climate_indices/HWDI_Nele.R"))

# ----- Loop to calculate the ids for all the stations: -----
for(i in 1:number.of.stations){
  station_id <- station.ids[i]

id <- tibble(id = station_id)


# ----- running functions ----- 
#CDD

vCDD <- CDD(BW_C, station_id)
avg.result.frame$CDD[i] <- vCDD[[1]][1]
rate.of.change.frame$CDD[i] <- vCDD[[2]][1]

#ETR
vETR <- ETR(BW_C, station_id)
avg.result.frame$ETR[i] <- vETR[[1]][1]
rate.of.change.frame$ETR[i] <- vETR[[2]][1]

#Fd
vFd <- fun_frostdays(BW_C, station_id)
avg.result.frame$Fd[i] <- vFd[[1]][1]
rate.of.change.frame$Fd[i] <- vFd[[2]][1]

#R10
vR10 <- R10(BW_C, station_id)
avg.result.frame$R10[i] <- vR10[[1]][1]
rate.of.change.frame$R10[i] <- vR10[[2]][1]

#R5d
vR5d <- calc_R5d(BW_C, station_id)
avg.result.frame$R5d[i] <- vR5d[[1]][1]
rate.of.change.frame$R5d[i] <- vR5d[[2]][1]

#R95T
vR95T <-R95T(BW_C, station_id, output = "numbers")
avg.result.frame$R95T[i] <- vR95T[[1]][1]
rate.of.change.frame$R95T[i] <- vR95T[[2]][1]

#SDII
vSDII <- SDII(BW_C, station_id)
avg.result.frame$SDII[i] <-  vSDII[[1]][1]
rate.of.change.frame$SDII[i] <- vSDII[[2]][1]

#Tn90
vTn90 <- Tn90(BW_C, station_id)
avg.result.frame$Tn90[i] <- vTn90[[1]][1]
rate.of.change.frame$Tn90[i] <- vTn90[[2]][1]

#GSL
vGSL <- GSL(BW_C, station_id)
avg.result.frame$GSL[i] <- vGSL[[1]][1]
rate.of.change.frame$GSL[i]<-vGSL[[2]][1]

#HWDI
vHWDI <- HWDI(BW_C, station_id)
avg.result.frame$HWDI[i] <- vHWDI[[1]][1]
rate.of.change.frame$HWDI[i] <- vHWDI[[2]][1]

# ----- Combine the indices -----
#avgs <- tibble(avg_Fd, avg_ETR, avg_GSL, avg_HWDI,
   #            avg_Tn90, avg_R10, avg_CDD, avg_R5d,
  #             avg_SDII, avg_R95T)

#ratios <- tibble(ratio_Fd, ratio_ETR, ratio_GSL, ratio_HWDI,
   #              ratio_Tn90, ratio_R10, ratio_CDD, ratio_R5d,
    #             ratio_SDII, ratio_R95T)

}

#write.csv2(avg.result.frame, "average_all_stations.csv")
#write.csv2(rate.of.change.frame, "rate_of_change_all_stations.csv")

names(avg.result.frame)[2:11] <- paste0("avg_", names(avg.result.frame)[2:11] )
names(rate.of.change.frame)[2:11] <- paste0("RoC_", names(rate.of.change.frame)[2:11] )

result.frame <- cbind(avg.result.frame, rate.of.change.frame[,-1])
write.csv2(result.frame, "indices_all_stations.csv")

