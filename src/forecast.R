#select directory to load load data 
#setwd("~/Documentos/Madimon/Investment/econ_outlook/fred/realgdpusa")
#setwd("~/Documentos/Madimon/Investment/econ_outlook/fred/inflationusa")
#setwd("~/Documentos/Madimon/Investment/econ_outlook/fred/employment_thousands_usa")
setwd("~/Documentos/Madimon/Investment/econ_outlook/fred/unemployment_rate_usa")

MyData <- read.csv("data.csv", header=TRUE, sep=",")
all<-as.timeSeries(MyData)

# subset the time series 
#tmp = window(all,"2010-01-01",end(all)) 
# plot series
plot(all)
#plot(tmp)

#fit <- ets(all)
fit <- auto.arima(all)
#fit <- auto.arima(tmp)

forecast(fit, 1)
plot(forecast(fit, 5))
accuracy(fit)


