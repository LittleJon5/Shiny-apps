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
 
# final plot function
##################


ggforecast <- function(past, future, span.val, input.date){
  
  load("data\\recessions.RData")
  recessions <- subset(recessions, Start >= input.date)
  
  startDate <- past$time[1]
  endDate <- future$time[nrow(future)]
  
  ggplot ( data = past ) +
    geom_rect ( data = recessions , aes ( xmin = Start , xmax = End , 
                                     ymin = -Inf , ymax = +Inf ) , fill = 'grey65', alpha = 0.4 ) +
    geom_ribbon ( data = future , fill = 'lightblue ' ,
                aes ( x = time , ymin = lower95 , ymax = upper95 ) ) +
    geom_ribbon ( data = future , fill = 'yellow' ,
                aes( x = time , ymin = lower80 , ymax = upper80 ) ) +
    geom_line ( data = future , aes ( x = time , y = forecast ) , size = 1.0 ,
              colour = 'red' ) +
    geom_point ( aes ( x = time , y = values ) , size = 1.0 , color = "red" ) +
    geom_line ( aes  ( x = time , y = values ) , color = "blue" ) +
    geom_smooth ( aes ( x = time, y = values ) , method = "loess" , span = span.val,
                size = 0.50 , color = "darkblue" , fill = "springgreen4" ) +
    scale_x_date( "" , limits = c( startDate , endDate ) )
}

# future data frame assembly function
###########################

final.frame <- function(ets.data){
  framed <- data.frame(as.character(as.Date(time(ets.data$mean))),
                      ets.data$lower[,2],
                      ets.data$lower[, 1],
                      ets.data$mean,
                      ets.data$upper[, 1],
                      ets.data$upper[, 2])
  names(framed) <- c('time', 'lower95', 'lower80', 'forecast',
                     'upper80', 'upper95')
  return(framed)
}

