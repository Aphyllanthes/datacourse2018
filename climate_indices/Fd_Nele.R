# Our function for frost days(Fd)

fun_frostdays <- function(bw, station_id){
  
# 1) filter the data for the specific station ####
merk <- bw %>% filter(id == station_id)

#Rename columns
names(merk)[3] <- "pp.h"
names(merk)[4] <- "pp.f"
names(merk)[5] <- "snowd"
names(merk)[6] <- "min.t"
names(merk)[7] <- "mean.t"
names(merk)[8] <- "max.t"
names(merk)[9] <- "rel.h"


# 2) Quality control ####

merk$year <- year(merk$date)

# count number of NA-values
merk3 <- merk %>%
  group_by(year) %>% mutate(na.n = sum(is.na(min.t)))

# delete all years with more than 60 NA-values
merk4 <- merk3 %>%
  filter(na.n < 60 ) %>%
  group_by(year) %>% mutate(na.n = sum(is.na(min.t))) %>% 
  ungroup()

# 4) Number of frost days ####

merk2 <- merk4 %>%
  filter(merk4$min.t < 0 ) %>%
  group_by(year) %>% count()
 

# 5) Average annual values ####
mean <- mean(merk2$n)  #112.0769 days

# 6) delta second half/first half ####
sec.half <- merk2  %>% filter(year >="1997",year <="2016")
fr.half <- merk2 %>% filter(year >= "1977",year < "1997")

sec.mean <- if(length(sec.half$year>10)){mean(sec.half$n)} #111.95

fr.mean <- if(length(fr.half$year>10)){mean(fr.half$n)} #112.2105

delta <- (sec.mean/fr.mean) * 100

output<-list(mean, delta)

return(output)

}

# Now replace data_name with the name of your data and insert your station ID to run the function.
# The first output in the list is the frost days mean(Fd), the second value is the rate of change.

#fun_frostdays(data_name,2814)
