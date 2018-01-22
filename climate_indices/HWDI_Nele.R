#------------------------------------------------------------------------------------------------------------
#Topic 4: Big data - DWD Stations with 40 years "Climate Data Baden-Württemberg"
#
#Indicator HWDI: heat wave duration index
#
#J. Siegismund, Januar 2018
#------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------
#function
#------------------------------------------------------------------------------------------------------------

#function to calculate the mean heat wave duration index (HWDI) for the 1977 to 2016 period and the ratio for the 1977 to 1996 period and the 1997 to 2016 period.
#HWDI: maximum period > 5 consecutive days with T(max) > 5 deg. C above the 1961-1990 daily T(max) normal
#function uses a set period to calculate T(max) normal. starting with the first year using a set period length
#yearly HWDIs are only calculated for years without missing values
#mean HWDI and the ratio are just calculated if each period has at least 10 values
#function input:
#a data frame of the DWD data (see above how to load in data)
#the station ID
#(optional) length of the period to calculate T(max) normal. pre set value is 20 years
  
HWDI <- function(data, station_id, period = 20){
    library(dplyr)
    
  #cutting down data frame to only have needed information
    station <- subset(data, id == station_id)
    needed <- data.frame(station$date, station$TXK)
    colnames(needed) <- c("date", "t_max")
      
  #creating sub frame to calculate daily mean t max for comparative period
    df <- needed
    period_start <- needed$date[1]
    period_stop <- period_start + years(period) - days(1)
    df_period <- subset(df, (date >= period_start) & (date <= period_stop))
    df_period$month_day <- format(df_period$date, "%m-%d")

  #calculating daily mean t max for comparative period
    df_mean <- aggregate(t_max ~ month_day, data = df_period, FUN = function(x){mean(x, na.rm = TRUE)})
    colnames(df_mean) <- c("month_day", "t_mean")

  #creating vector with occurring years. needed to run loop
    year_start <- as.integer(format(needed$date[1], "%Y")) + 0
    year_stop <- as.integer(format(needed$date[length(needed$date)], "%Y")) + 0 
    years <- as.character(seq(from = year_start, to = year_stop, by = 1))

  #calculating HWDI for each year
    periods <- rep(NA, length(years))
    
    for (l in 1:length(years)){
   
    #creating sub frame for single years
      year = years[l]
      y <- subset(df, (date >= as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d")) & (date <= as.Date(paste0(year, "-12-31"), format = "%Y-%m-%d")))
    
    #checking t for NAs. only years with no NAs are used. HWDI for years with NAs will be NA
    #preparing daily mean t max data frame for merge 
      if (sum(is.na(y$t_max)) > 0){
        periods[l] <- NA
      }
      else{
        df_mean$date <- as.Date(paste0(year, "-", df_mean$month_day), format = "%Y-%m-%d")

      #merging single year data frame with mean t max data frame
        mer <- merge(df_mean, y, by = "date", all.x = FALSE, all.y = TRUE)
        mer$dif <- mer$t_max - mer$t_mean

      #heat wave temp condition check
        dif_check <- function(x){if (is.na(x)==TRUE){FALSE} else {if (x > 5){TRUE} else {FALSE}}}
        mer$check <- lapply(mer$dif, dif_check)
      
      #giving heat wave periods ids
        count <- 1
        event <- rep(NA, length(mer$check))
        for (j in 1:length(mer$check)){
          if (mer$check[j] == FALSE){
            event[j] <- NA
            count <- count + 1
          }  
          else{
            event[j] <- count
          }
        }
        mer$event <- event
  
      #getting length of each period
        event_id <- unique(na.omit(mer$event))
        period_lengths <- rep(NA, length(event_id))
          
        for (k in 1:length(event_id)){
          event_temp <- mer[which(mer$event==event_id[k]),]
          period_lengths[k] <- length(event_temp$event)
        }
        periods[l] <- max(period_lengths)
      }
    }

    #heat wave period length check
      p_check <- function(x){if (is.na(x)==TRUE){NA} else {if (x > 5){x} else {NA}}}
      year <- as.integer(years)
      df_HWI <- data.frame(year, periods)
      df_HWI$pday <- as.integer(lapply(df_HWI$periods, p_check))

    #calculating mean HWDI for 1977 to 2016 period
    #at least 10 values are needed, otherwise HWI_mean will be NA
      HWI_mean <- if (sum(!is.na(df_HWI$pday)) >= 10) {mean(df_HWI$pday, na.rm = TRUE)} else {NA}
    
    #calculating ratio for 1977 to 1996 period and 1997 to 2016 period
    #at least 10 values per period are needed, otherwise HWI_mean will be NA
      df_HWI_first <- subset(df_HWI, (year >= 1977) & (year <= 1996))
      HWI_first <- if (sum(!is.na(df_HWI_first$pday)) >= 10) {mean(df_HWI_first$pday, na.rm = TRUE)} else {NA}
      df_HWI_second <- subset(df_HWI, (year >= 1997) & (year <= 2016))
      HWI_second <- if (sum(!is.na(df_HWI_second$pday)) >= 10) {mean(df_HWI_second$pday, na.rm = TRUE)} else {NA}
      HWI_ratio <- HWI_second / (HWI_first / 100)
 
    #creating output data frame  
      HWI_indices <- data.frame(HWI_mean, HWI_ratio)
      
    #output
      return(HWI_indices)
  }

#------------------------------------------------------------------------------------------------------------
#result
#------------------------------------------------------------------------------------------------------------      

#change station_id to calculate the heat wave indices for your station
#  station_id <- 755
#  result <- HWDI(data, station_id)
#  print(result)
  
