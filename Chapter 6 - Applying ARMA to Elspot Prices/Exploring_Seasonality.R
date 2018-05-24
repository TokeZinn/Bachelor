
  rm(list=ls()) #Clear all
source("Expanded_sorting.R")
{
library(ggplot2)
} #Library

#Plotting group means
{
  p1 = ggplot(Hourly, aes(Hourly$Hour,Hourly$Price)) + geom_point() + geom_line(aes(group = 1)) +
    labs(x = "Hour", y = "Price", title = "(A)") +
    scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"),
                     labels = c("1","","","4","","","","8","","","","12","","","","16","","","","20","","","","24")) 
  
  p2 = ggplot(Weekly, aes(Weekly$weekday,Weekly$Price)) + geom_point() + geom_line(aes(group = 1)) +
    labs(x = "Day", y = "Price", title = "(C)")+
    scale_x_discrete("Day", labels=c("Monday","","Wednesday","","Friday","","Sunday"))
  
  p3 = ggplot(Monthly, aes(Monthly$month,Monthly$Price)) + geom_point() + geom_path(aes(group = 1)) +
    scale_x_discrete("Month",breaks = c("January", "May", "August","December")) +
    labs(x = "Month", y = "Price", title = "(B)") + xlab("Month")
  
  p4 = ggplot(Season, aes(Season$season,Season$Price)) + geom_point() + geom_line(aes(group = 1)) +
    labs(x = "Season", y = "Price", title = "(D)") + 
    scale_x_discrete("Season",limits=c("Spring","Summer","Fall","Winter"))
  

source("Multiplot.r")
multiplot(p1,p2,p3,p4, cols = 2)

}

#Plotting moving average
ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=1)}
Dailyma = Daily
Dailyma$Price = ma(x = Daily[,2],n = 30)
b1 = ggplot(Daily,aes(Date,Price)) + geom_line() + ggtitle("Daily prices")
b2 = ggplot(Dailyma,aes(Date,Price)) + geom_line() + ggtitle("Moving average")
multiplot(b1,b2,cols = 2)




Names = namer[-(26:28)]
# Grouping by hour and day
Hour_day = Prices %>% 
  group_by(weekday = weekday,Hour)  %>%
  summarise(Price = mean(Price,na.rm = T))

for(i in 168:1){
  if(Hour_day[i,2] == "14" |Hour_day[i,2] == "15" | Hour_day[i,2] == "16"|
     Hour_day[i,2] == "17"|Hour_day[i,2] == "19"|Hour_day[i,2] == "20"|Hour_day[i,2] == "21" |
     Hour_day[i,2] == "22"|Hour_day[i,2] == "23"|Hour_day[i,2] == "24"){
    Hour_day = Hour_day[-i,]
  }
  if(Hour_day[i,2] == "13" | Hour_day[i,2] == "11"|Hour_day[i,2] == "10" |
     Hour_day[i,2] == "9"|Hour_day[i,2] == "8"|Hour_day[i,2] == "7"|Hour_day[i,2] == "6" |
     Hour_day[i,2] == "5"|Hour_day[i,2] == "4"|Hour_day[i,2] == "2"|Hour_day[i,2] == "1"){
    Hour_day = Hour_day[-i,]
  }
}

Hour_day$Hour = factor(Hour_day$Hour, levels = c("3","12","18"))


#Unused
{
Hour_mean_day = Hours %>%
  group_by(weekday = weekday) %>%
  summarise(Hour1 = mean(Hour1),
            Hour2 = mean(Hour2),
            Hour3 = mean(Hour3),
            Hour4 = mean(Hour4),
            Hour5 = mean(Hour5),
            Hour6 = mean(Hour6),
            Hour7 = mean(Hour7),
            Hour8 = mean(Hour8),
            Hour9 = mean(Hour9),
            Hour10 = mean(Hour10),
            Hour11 = mean(Hour11),
            Hour12 = mean(Hour12),
            Hour13 = mean(Hour13),
            Hour14 = mean(Hour14),
            Hour15 = mean(Hour15),
            Hour16 = mean(Hour16),
            Hour17 = mean(Hour17),
            Hour18 = mean(Hour18),
            Hour19 = mean(Hour19),
            Hour20 = mean(Hour20),
            Hour21 = mean(Hour21),
            Hour22 = mean(Hour22),
            Hour23 = mean(Hour23),
            Hour24 = mean(Hour24))
}

ggplot(Hour_day, aes(weekday, Price,group = Hour,color = Hour,shape=Hour)) + 
  geom_point(aes(weekday, Price,group = Hour , size = 0)) +
  geom_line() + xlab("Weekday") + ylab("Euro/MWh") + scale_color_manual(values = c("red","blue","black")) +
  ggtitle("Weekly seasonality grouped by Hour") + scale_size_continuous(guide = FALSE)








