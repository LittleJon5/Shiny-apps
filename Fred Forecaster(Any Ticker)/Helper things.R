as.duration(interval(ymd(time(indicator)[1]), ymd(time(indicator)[2])))

mon <- month(time(indicator))[1:4]
paste(mon[1], mon[2], mon[3], mon[4], sep = "")



day(time(indicator))

fred.compound <- function(fred.data){switch(paste(month(time(fred.data))[1],
                                        month(time(fred.data))[2],
                                        month(time(fred.data))[3],
                                        month(time(fred.data))[4],
                                        sep = ""),
                                  "1111" = 1,
                                  "1234" = 12,
                                  "1717" = 2,
                                  "14710" = 4)
}