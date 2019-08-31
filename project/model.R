train <- read.csv("train.csv")
test <- read.csv("test.csv")

View(train)

#combine test with train 
test$registered=0
test$casual=0
test$count=0
data=rbind(train,test)

View(data)

str(data)

table(is.na(data))

# Histogram for every independent variable 
par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(data$season)
hist(data$weather)
hist(data$humidity)
hist(data$holiday)
hist(data$workingday)
hist(data$temp)
hist(data$atemp)
hist(data$windspeed)

#analyse prob distribution
prop.table(table(data$weather))
#Thus, Weather 1 ( mostly clear weather) has more counts od users

#Similarly others
prop.table(table(data$workingday))
#Thus, working day has more counts of users using the bikes

prop.table(table(data$holiday))
#Thus, no holiday means more bike rental

prop.table(table(data$season))
#Almost equal distribution in all 4 seasons


#Convert discrete variables into factor (season, weather, holiday, workingday)
data$season<-as.factor(data$season)
data$weather<-as.factor(data$weather)
data$holiday<-as.factor(data$holiday)
data$workingday<-as.factor(data$workingday)

str(data)

### Hypothesis Testing(multivariate analysis)

# 1. Hourly trend
# Hourly trend: There must be high demand during office timings. 
# Early morning and late evening can have different trend (cyclist) 
# and low demand during 10:00 pm to 4:00 am.
# Extract using datetime column
data$hour<-substr(data$datetime,12,13)
data$hour<-as.factor(data$hour)

#Greater than 20 users , less than 19 users
train<-data[as.integer(substr(data$datetime,9,10))<20,]
test<-data[as.integer(substr(data$datetime,9,10))>19,]

par(mfrow = c(1,1))
boxplot(train$count~train$hour,xlab="hour", ylab="count of users")

# Thus, most users in the following trend:
# High       : 7-9 and 17-19 hours
# Average  : 10-16 hours
# Low         : 0-6 and 20-24 hours


# Hypothesis 2. Weekdays Trend
# To see if the registered users use bike system more on weekdays than weekends 
date <- substr(data$datetime,1,10)
days<-weekdays(as.Date(date))
data$day=days

boxplot(data$registered ~ data$day, xlab = "day", ylab = "count of registered users")
boxplot(data$casual ~ data$day, xlab = "day", ylab = "count of casual users")

# Thus, it can be seen that the casual users 
# prefer to use the bikes more on weekends 
# while registered users do so more on weekdays



# Hypothesis 3: 'Rain' Trend: People prefer to rent bikes more on clear weather than rainy
# for total users
boxplot(data$count ~ data$weather, xlab= "Weather conditions", ylab = "Count of users")

#For registered users
boxplot(data$registered ~ data$weather, xlab= "Weather conditions", ylab = "Count of users")

#For casual users
boxplot(data$casual ~ data$weather, xlab= "Weather conditions", ylab = "Count of users")


  
# Hypothesis 4:  The correlation table of continous variables like wind, temperature and humididity 
# with the reg, casual and total user count

# Make a subset of data
subset <- data.frame(train$registered, train$casual, train$count, train$temp, 
                     train$humidity, train$atemp, train$windspeed)
cor(subset)

# Inferences
# Variable atemp is highly correlated with temp.
# Variable temp is positively correlated with the dependent variables (casual is more compare to registered)
# Windspeed has lower correlation as compared to temp and humidity


# Another thing to note: The demand of bikes increases considerably from 2011 to 2012
data$year=substr(data$datetime,1,4)
data$year=as.factor(data$year)
train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]
boxplot(train$count~train$year,xlab="year", ylab="count")


#######################################

library(ggplot2)
library(lubridate)
library(randomForest)
set.seed(1)


