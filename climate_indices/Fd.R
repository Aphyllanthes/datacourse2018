# Function for the number of frost days
# Raymond Asimhi, Sara Doumi, Anna Dermann

#To use this function;
#Import your data
#Scroll down to the last line of the code
#Replace data_name with the name of your imported data
#Change the station_id to ID number of your specific station
#Run the code, your output will be 2 values; the first is the mean frost days 
#and the second is the rate of change


library(tidyverse)
library(lubridate)

# Our function for frost days(Fd)

fun_frostdays <- function(bw, station_id){
  
# 1) filter the data for the specific station ####
station <- bw %>% filter(id == station_id)


# 2) Quality control ####

station$year <- year(station$date)

# count number of NA-values
station3 <- station %>%
  group_by(year) %>% mutate(na.n = sum(is.na(TNK)))

# delete all years with more than 60 NA-values
station4 <- station3 %>%
  filter(na.n < 60 ) %>%
  group_by(year) %>% mutate(na.n = sum(is.na(TNK))) %>% 
  ungroup()

# 4) Number of frost days ####

station2 <- station4 %>%
  filter(station4$TNK < 0 ) %>%
  group_by(year) %>% count()
 

# 5) Average annual values ####
mean <- mean(station2$n)  #112.0769 days

# 6) delta second half/first half ####
sec.half <- station2  %>% filter(year >="1997",year <="2016")
fr.half <- station2 %>% filter(year >= "1977",year < "1997")

sec.mean <- if(length(sec.half$year>10)){mean(sec.half$n)} #111.95

fr.mean <- if(length(fr.half$year>10)){mean(fr.half$n)} #112.2105

delta <- (sec.mean/fr.mean) * 100

output<-list(mean, delta)

return(output)

}

#fun_frostdays(data_name, station_id)
