require(googleVis)
require(magrittr)


sum.cars <- aggregate(mpg~cyl, data = mtcars, FUN = mean)

g <- gvisGauge(sum.cars, labelvar = "mpg", numvar = "mpg")

plot(g)
