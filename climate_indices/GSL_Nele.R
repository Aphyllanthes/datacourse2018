# Calculates Growing seasons length index (GSL) with 2 vectors: date (yyyy-mm-dd) and temp (daily mean temperature)
# Calculation is adjusted with Zhang et al., 2011, WIREs Clim Change 2011. doi: 10.1002/wcc.147
# Version 1.0, michael.stoelzle@hydro.uni-freiburg.de

GSL <- function(data, id) {# date, temp
  
  station <- subset(data[which(data$id == station_id),])
  temp <- station$TMK
  date <- station$date
  
    stopifnot(inherits(date, 'Date'), is.numeric(temp))
    stopifnot(require(dplyr))
    stopifnot(require(zoo))

    # helper functions
      mnt <- function(x) rollapply(x, 6, FUN="min", fill=NA, align="right")
      mxt <- function(x) rollapply(x, 6, FUN="max", fill=NA, align="right")
    
    # function code  
      df<- tibble(date, temp)
      df$year <- format(date, "%Y") %>% as.numeric()
    
    # check years have more than 60 values
      df <- df %>% group_by(year) %>% mutate(nas = sum(is.na(temp))) %>% 
              filter(nas < 60) %>% select(-nas) %>% ungroup()
    
    #set periods
      df$period <- "1977_1996"
      df$period[df$year %in% 1997:2016] <- "1997_2016"
      period_len <-  df %>% count(period) %>% 
                      mutate(test10y = n>=3652) %>% # at least two leap years
                      summarise(test=any(test10y)) %>% as.logical()
       
    # calculate index  
      df2 <- df %>% arrange(date) %>% group_by(year) %>% 
                mutate(start = mnt(temp), end = mxt(temp)) 
   
      sf <- df2 %>% filter(start > 5) %>%  slice(1) %>% ungroup()
      ef <- df2 %>% filter(end < 5, format(date, "%j") %>% as.numeric()>=182) %>%  slice(1) %>% ungroup() # small adjustment, doy > 1-july
      af <- bind_rows(sf,ef) %>% arrange(date) %>% 
              group_by(year) %>% mutate(slen = date - lag(date)) %>% 
              filter(!is.na(slen)) %>% select(year, slen, period)
   
   avg <- mean(af$slen) %>% as.numeric()
   avgp <- af %>% group_by(period) %>% summarise(avgp = mean(slen)) %>% select(avgp) %>% unlist() %>% as.numeric()
   ratio <- NA
   if(period_len) ratio <-  avgp[2] / avgp[1] * 100

return(list(average = avg, ratio = ratio))
}
