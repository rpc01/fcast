library('ProjectTemplate')
load.project()

#json config to download data from fred
api_key<-"&api_key=9912ee381321a596b999f26cfa5da2a5"
url<-"https://api.stlouisfed.org/fred/series/observations?"
file_type<-"&file_type=json"

#Change in nonfarm payroll employment
s1<-"series_id=PAYEMS" #seasonally adjusted monthly in nonfarm payroll employment to the nearest thousand persons
s1<-fromJSON(paste0(url, s1, api_key,file_type))
s1_df<-as.data.frame(s1)
#names(s1_df)
charvec<-as.Date(as.character(s1_df[,15]))
data<-as.numeric(s1_df[,16])
s1<-as.timeSeries(data, charvec)
s1<-diff(s1,1) #to compute monthly change


#Inflation
s2<-"series_id=CPIAUCSL"    #"seasonally adjusted annualized monthly percentage change in headline CPI
                            #rounded to the nearest tenth of a percent"
s2<-fromJSON(paste0(url, s2, api_key,file_type))
s2_df<-as.data.frame(s2)
charvec<-as.Date(as.character(s2_df[,15]))
data<-as.numeric(s2_df[,16])
s2<-as.timeSeries(data, charvec)
s2<-((Delt(s2, type="log"))+1)^12-1 #annualized percentage change


#Real GDP Growth
s3<-"series_id=A191RL1Q225SBEA"     #"seasonally adjusted annualized quarter-to-
                                    #quarter percentage change advance estimate 
                                    #of GDP from the previous final estimate of 
                                    #GDP, rounded to the nearest tenth of a percent"
s3<-fromJSON(paste0(url, s3, api_key,file_type))
s3_df<-as.data.frame(s3)
charvec<-as.Date(as.character(s3_df[,15]))
data<-as.numeric(s3_df[,16])
s3<-as.timeSeries(data, charvec)

#"Unemployment 
s4<-"series_id=UNRATE"      #is the seasonally adjusted level of the headline
                            #unemployment rate in percentage points rounded to 
                            #the nearest tenth of a percent")
s4<-fromJSON(paste0(url, s4, api_key,file_type))
s4_df<-as.data.frame(s4)
charvec<-as.Date(as.character(s4_df[,15]))
data<-as.numeric(s4_df[,16])
s4<-as.timeSeries(data, charvec)

# FORECAST

fit1 <- auto.arima(s1)
fit2 <- auto.arima(s2)
fit3 <- auto.arima(s3)
fit4 <- auto.arima(s4)

ts1_cast<-forecast(fit1, 1)
ts2_cast<-forecast(fit2, 1)
ts3_cast<-forecast(fit3, 1)
ts4_cast<-forecast(fit4, 1)

parameter<-c("Employment", "Inflation", "RealGDP", "Unemployment")
value<-round(c(ts1_cast$mean,ts2_cast$mean,ts3_cast$mean,ts4_cast$mean),4)
forecast<-data.frame(parameter, value)#%, unit)
colnames(forecast) <- c("Parameter", "Fcast")#, "Desc")
forecast

