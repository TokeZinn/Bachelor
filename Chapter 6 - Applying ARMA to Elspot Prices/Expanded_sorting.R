
{
library(plyr) 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(readxl)
library(tseries)
library(forecast)
} #Library



# Initial data gymnastics
{
hourly2017 = read_excel("Data/2017_hourly.xlsx",skip = 2)
hourly2016 = read_excel("Data/2016_hourly.xlsx",skip = 2); hourly2016 = hourly2016[,-20]

Daily2017 = read_excel("Data/2017_daily.xlsx",skip = 2)
Daily2016 = read_excel("Data/2016_daily.xlsx",skip = 2) ; Daily2016 = Daily2016[-19]
Daily2018 = read_excel("Data/elspot-prices_2018_daily_eur.xlsx" , skip = 2)
Daily = rbind(Daily2016,Daily2017)
Daily = na.omit(Daily) ; Daily = Daily[,-(2:7)]
Daily = Daily[,-(3:12)]
colnames(Daily) = c("Date","Price")
Daily$Date = as.Date(Daily$Date)
Daily$weekday = weekdays.POSIXt(Daily$Date)



Prices = rbind(hourly2016,hourly2017)
Prices = data.frame(Prices)
Prices = Prices[,-(3:8)] ; Prices = Prices[,-(4:13)]
Prices = na.omit(Prices)
Prices[,1] = as.Date(Prices[,1])
colnames(Prices) = c("Date", "Hour", "Price")


Prices$weekday = weekdays.POSIXt(Prices$Date)


#English weekdays
{
  for(i in 1:nrow(Prices)){
    if(Prices$weekday[i] == "mandag"){
      Prices$weekday[i] = "Monday"
    }
    if(Prices$weekday[i] == "tirsdag"){
      Prices$weekday[i] = "Tuesday"
    }
    if(Prices$weekday[i] == "onsdag"){
      Prices$weekday[i] = "Wednesday"
    }
    if(Prices$weekday[i] == "torsdag"){
      Prices$weekday[i] = "Thursday"
    }
    if(Prices$weekday[i] == "fredag"){
      Prices$weekday[i] = "Friday"
    }
    if(Prices$weekday[i] == "lørdag"){
      Prices$weekday[i] = "Saturday"
    }
    if(Prices$weekday[i] == "søndag"){
      Prices$weekday[i] = "Sunday"
    }
  }
  
for(i in 1:nrow(Daily)){
    if(Daily$weekday[i] == "mandag"){
      Daily$weekday[i] = "Monday"
    }
    if(Daily$weekday[i] == "tirsdag"){
      Daily$weekday[i] = "Tuesday"
    }
    if(Daily$weekday[i] == "onsdag"){
      Daily$weekday[i] = "Wednesday"
    }
    if(Daily$weekday[i] == "torsdag"){
      Daily$weekday[i] = "Thursday"
    }
    if(Daily$weekday[i] == "fredag"){
      Daily$weekday[i] = "Friday"
    }
    if(Daily$weekday[i] == "lørdag"){
      Daily$weekday[i] = "Saturday"
    }
    if(Daily$weekday[i] == "søndag"){
      Daily$weekday[i] = "Sunday"
    }
}
}

Prices$weekday <- factor(Prices$weekday, levels= c("Monday", "Tuesday", 
                                                   "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
Daily$weekday <- factor(Daily$weekday, levels= c("Monday", "Tuesday", 
                                                   "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


weekend = subset(Prices,Prices$weekday == "Saturday" | Prices$weekday == "Sunday")
weekday = subset(Prices,Prices$weekday != "Saturday" & Prices$weekday != "Sunday")
}



# More data gymnastics
{
Prices$month = month(Prices$Date)
for(i in 1:nrow(Prices)){
  if(Prices$month[i] == 1 | Prices$month[i] == 2){Prices$season[i] = "Winter"}
  else if(Prices$month[i] == 3 | Prices$month[i] == 4 | Prices$month[i] == 5){Prices$season[i] = "Spring"}
  else if(Prices$month[i] == 6 | Prices$month[i] == 7 | Prices$month[i] == 8){Prices$season[i] = "Summer"}
  else if(Prices$month[i] == 9 | Prices$month[i] == 10 | Prices$month[i] == 11){Prices$season[i] = "Fall"}
  else if(Prices$month[i] == 12){Prices$season[i] = "Winter"}
}

for(i in 1:nrow(Prices)){
  if(Prices$month[i] == 1){Prices$month[i] = "January"}
  else if(Prices$month[i] == 2){Prices$month[i] = "February"}
  else if(Prices$month[i] == 3){Prices$month[i] = "March"}
  else if(Prices$month[i] == 4){Prices$month[i] = "April"}
  else if(Prices$month[i] == 5){Prices$month[i] = "May"}
  else if(Prices$month[i] == 6){Prices$month[i] = "June"}
  else if(Prices$month[i] == 7){Prices$month[i] = "July"}
  else if(Prices$month[i] == 8){Prices$month[i] = "August"}
  else if(Prices$month[i] == 9){Prices$month[i] = "September"}
  else if(Prices$month[i] == 10){Prices$month[i] = "October"}
  else if(Prices$month[i] == 11){Prices$month[i] = "November"}
  else if(Prices$month[i] == 12){Prices$month[i] = "December"}
}

Prices$month <- factor(Prices$month, levels= c("January", "February", "March", "April", "May", 
                                               "June", "July", "August", "September", "October", "November", "December"))

Daily$month = month(Daily$Date)
for(i in 1:nrow(Daily)){
  if(Daily$month[i] == 1 | Daily$month[i] == 2){Daily$season[i] = "Winter"}
  else if(Daily$month[i] == 3 | Daily$month[i] == 4 | Daily$month[i] == 5){Daily$season[i] = "Spring"}
  else if(Daily$month[i] == 6 | Daily$month[i] == 7 | Daily$month[i] == 8){Daily$season[i] = "Summer"}
  else if(Daily$month[i] == 9 | Daily$month[i] == 10 | Daily$month[i] == 11){Daily$season[i] = "Fall"}
  else if(Daily$month[i] == 12){Daily$season[i] = "Winter"}
}

for(i in 1:nrow(Daily)){
  if(Daily$month[i] == 1){Daily$month[i] = "January"}
  else if(Daily$month[i] == 2){Daily$month[i] = "February"}
  else if(Daily$month[i] == 3){Daily$month[i] = "March"}
  else if(Daily$month[i] == 4){Daily$month[i] = "April"}
  else if(Daily$month[i] == 5){Daily$month[i] = "May"}
  else if(Daily$month[i] == 6){Daily$month[i] = "June"}
  else if(Daily$month[i] == 7){Daily$month[i] = "July"}
  else if(Daily$month[i] == 8){Daily$month[i] = "August"}
  else if(Daily$month[i] == 9){Daily$month[i] = "September"}
  else if(Daily$month[i] == 10){Daily$month[i] = "October"}
  else if(Daily$month[i] == 11){Daily$month[i] = "November"}
  else if(Daily$month[i] == 12){Daily$month[i] = "December"}
}

Daily$month <- factor(Daily$month, levels= c("January", "February", "March", "April", "May", 
                                               "June", "July", "August", "September", "October", "November", "December"))

} # More data gymnastics


#Index for hourlies
{
index = data.frame()
for(k in 1:731){
  for(i in 1:24){
    index[k,i] = i + (k-1)*24
  }
}
  for(i in 1:24){
    Prices$Hour[index[,i]] = as.character(i)
  }
} 
  

#Grouping (summarise = mean)
{
Hourly = Prices %>% 
  group_by(Hour = Hour) %>%
  summarise(Price = mean(Price, na.rm = T))
  
  

Weekly = Prices %>% 
  group_by(weekday = weekday) %>%
  summarise(Price = mean(Price, na.rm = T))

Monthly = Prices %>% 
  group_by(month = month) %>%
  summarise(Price = mean(Price, na.rm = T))

Season = Prices %>% 
  group_by(season = season) %>%
  summarise(Price = mean(Price, na.rm = T))


} 




#Grouping by hour)
{
  Hours = data.frame(filter(Prices,Prices$Hour == as.character(1)))
  for(i in 2:24){
    Hours[,(i+5)] = filter(Prices,Prices$Hour == as.character(i))$Price
  }
  Hours = Hours[,-2]
  Hours = Hours[,c(1,2,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,3,4,5)]
  namer = c("Date","Hour1")
  for(i in 2:24){
    namer = c(namer, paste("Hour",i,sep =""))
  }
  namer = c(namer, "weekday", "month","season")
  colnames(Hours) = namer

  # for(i in 1:24){
  #   assign(paste("Hour",i,sep="_"),filter(Prices,Prices$Hour == as.character(i)))
  # }

}


# New data frame excluding weekends. 
{
  noweekends = Hours
  for(i in nrow(Hours):1){
    if(noweekends$weekday[i] == "søndag" | noweekends$weekday[i] == "lørdag"){
      noweekends = noweekends[-i,]
    }
  }
}


