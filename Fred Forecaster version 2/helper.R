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

