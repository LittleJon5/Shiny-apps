# FRED transformation functions
# October 11, 2012 Revised March 19, 2015
# Author: Ray Nelson
###############################################################################
require(magrittr)

# Change
chg <- function(tsData) {
  tsData %>% 
    diff %>% 
    na.omit
}

# Year over year change
ch1 <- function(tsData, n_obs_per_year) {
  tsData %>% 
    diff(lag = n_obs_per_year) %>% 
    na.omit
}

# Percentage change
pch <- function(tsData) {
  (tsData/lag(tsData, 1)-1) * 100 %>% 
    na.omit
}

# Year over year percentage change
pc1 <- function(tsData, n_obs_per_year) {
  (tsData/lag(tsData, n_obs_per_year)-1) * 100 %>% 
    na.omit
}

# Compounded annual change
pca <- function(tsData, n_obs_per_year) {
  na.omit(((tsData/lag(tsData, 1))^n_obs_per_year-1) * 100)
}

# Continously compounded percentage change
cch <- function(tsData) {
  na.omit((log(tsData) - log(lag(tsData, 1))) * 100)
}

# Continuously compounded annual change
cca <- function(tsData, n_obs_per_year) {
  na.omit((log(tsData) - log(lag(tsData, 1))) * n_obs_per_year * 100)
}

freq.switcher <- function(month){switch(paste(month[1], month[2], sep = ""),
                                        "0101" = 1,
                                        "0104" = 4,
                                        "0407" = 4,
                                        "0710" = 4,
                                        "1001" = 4,
                                        "0102" = 12,
                                        "0203" = 12,
                                        "0304" = 12,
                                        "0405" = 12,
                                        "0506" = 12,
                                        "0607" = 12,
                                        "0708" = 12,
                                        "0910" = 12,
                                        "1011" = 12,
                                        "1112" = 12,
                                        "1201" = 12
)
}

time.freq <- function(tsData)
                    {
  short.date <- strftime(time(tsData), "%Y-%m")
  
  aggr.stat  <- aggregate(tsData ~ short.date, FUN = mean)
  
  short.years <- substr(aggr.stat[,1], 1, 4)
  
  short.months <- substr(aggr.stat[,1], 6, 7)
  
  myts <- ts(aggr.stat[,2],
             start=c(as.numeric(short.years[1]),
                     as.numeric(short.months[1])),
             end=c(as.numeric(short.years)[length(short.years)],
                   as.numeric(short.months)[length(short.months)]),
             frequency = freq.switcher(short.months))
  
  return(myts)

                    }