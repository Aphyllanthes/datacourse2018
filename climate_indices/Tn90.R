
Tn90 <- function(x,ID){            
  
  # The function works with the whole data set and the station-ID 
  # and constructs a list with the years and the Tn90 index 
  # as well as the average and the rate of change 
  
  
  # ----- packages required -----
  require(tidyverse)
  require(lubridate)
  
  
  
  # ----- prepare variables ------
  df <- x %>% filter(id==ID) 
  
  df$year <- year(df$date)
  
  
  # ----- quality check -----   
  df <- df %>% group_by(year) %>% filter(sum(is.na(TNK))<60)
  
  
  # ----- analysis -----    
  q <- quantile(df$TNK,0.9,na.rm=TRUE)
  
  Tn90df <- df  %>% group_by(year) %>% 
    summarise(Tn90 = length(which(TNK > q))/length(TNK)*100)
  
  Tn90df2 <- Tn90df %>% filter(year >= 1997, year <= 2016)
  
  if(length(Tn90df2$year) < 10) {
    stop("length(y) < 10")
  }
  
  Tn90df2 <- Tn90df2 %>% summarise(AVG1 = mean(Tn90, na.rm=TRUE))
  
  Tn90df3 <- Tn90df %>% filter(year >= 1977, year <= 1996)
  
  if(length(Tn90df3$year) < 10) {
    stop("length(y) < 10")
  }
  
  Tn90df3 <- Tn90df3 %>% summarise(AVG2 = mean(Tn90, na.rm=TRUE))
  
  avg <- mean(Tn90df$Tn90, na.rm = TRUE)
  
  roc <- (Tn90df2$AVG1/Tn90df3$AVG2)*100
  
  # ----- return -----   
  list <- list(year = Tn90df$year, Tn90 = Tn90df$Tn90, avg = avg, rate_of_change = roc)
  return(list)
}
