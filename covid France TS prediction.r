covid19 = read.csv("~/Downloads/WHO-COVID-19-global-data-2021-03-24.csv")
covid19_F=covid19[covid19$Country=="France",]
covid19_F_nc=ts(covid19_F$New_cases)
covid19_F_nd=ts(covid19_F$New_deaths)

library(forecast)
autoplot(covid19_F_nd)+ ggtitle('Number of new deaths of Covid19 in France')+ xlab('days')+ ylab('Number of deaths of Covid19')
library(ggplot2) 
autoplot(covid19_F_nc) +
  ggtitle('Number of new cases of Covid19 in France')+ 
  xlab('days')+ ylab('Number of new cases of Covid19')

acf(covid19_F_nc)

covid19_F_nc=ts(covid19_F_nc,freq=7) 
acf(diff(covid19_F_nc,lag=7))

fit=hw(covid19_F_nc) 
autoplot(forecast(fit,h=7))

print(forecast(fit,h=7))

covid19_2 = read.csv("~/Downloads/WHO-COVID-19-global-data-2021-03-30.csv")
covid19_F_2=covid19_2[covid19_2$Country=="France",]

covid19_F_2_nc=ts(covid19_F_2$New_cases) 
covid19_F_2_nc=ts(covid19_F_2_nc,freq=7)

prev=forecast(fit,h=7)
autoplot(prev) + autolayer(tail(covid19_F_2_nc,7), series="true data")+
  autolayer(prev$mean, series="HW forecasts")


autoplot(tail(covid19_F_2_nc,17*7), 
         series="true data")+ autolayer(prev$mean, series="HW forecasts")



