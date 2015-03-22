library(quantmod)
library(forecast)
library(mFilter)
library(timeSeries)
library(ggplot2)

# --------------------------------Time Series Specifications
##########################

horizon <- 24
startDate <- as.Date('1960-01-01')
endDate <- as.Date('2014-01-01')
series <- "M2SL"
scaleFactor <- 1000
y.title <- "Trillions of Dollars\n"
chart.title <- "M2 Money Supply (M2SL)\n"

# -------------------------------Retrieve and subset data from FRED
######################################

indicator <- getSymbols(series, src = 'FRED', auto.assign = FALSE)

indicator <- window(indicator,
                    start = startDate) / scaleFactor


# -------------------------------Hoddrick Prescott for Trend and Cycle
######################################

indicator.hp <- hpfilter(indicator,
                         freq = 100000000,
                         type = 'lambda')

hp.data <- data.frame(as.Date(rownames(indicator.hp$x)), 
                      indicator.hp$x,
                      indicator.hp$trend,
                      indicator.hp$cycle)

names(hp.data) <- c("xTime",
                    "indicator",
                    "trend",
                    "cycle")

#--------------------------------Extend the trend 
#######################################

spline.model <- smooth.spline(hp.data$xTime, hp.data$trend)

futureTime <- seq(as.Date(max(time(indicator))),
                  by="months",
                  length = (horizon + 1))

futureTime <- futureTime[2:(horizon + 1)]

xTime <- c(hp.data$xTime, futureTime)

predicted <- predict(spline.model, as.numeric(xTime))

predicted <- data.frame(xTime, smooth.trend = predicted$y)

# -------------------------------forecast next five years
########################################

indicator.ets = ets(as.timeSeries(indicator),
                    model = "ZZZ")

forecast.ets = forecast(indicator.ets,
                        h = horizon)

forecast.df <- data.frame(xTime = futureTime,
                          forecast.ets)

# -------------------------------cleanup
################################

rm(indicator,
   indicator.hp,
   spline.model,
   futureTime,
   xTime,
   indicator.ets,
   forecast.ets)
