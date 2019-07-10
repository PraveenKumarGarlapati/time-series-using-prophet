# Time-series in R

library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)

#Data from this AV link
##https://datahack.analyticsvidhya.com/contest/practice-problem-time-series-2/

train = fread("Train_SU63ISt.csv")
test = fread("Test_0qrQsBZ.csv")

# Extract date from the Datetime variable
train$Date = as.POSIXct(strptime(train$Datetime, "%d-%m-%Y"))
test$Date = as.POSIXct(strptime(test$Datetime, "%d-%m-%Y"))

# Convert 'Datetime' variable from character to date-time format
train$Datetime = as.POSIXct(strptime(train$Datetime, "%d-%m-%Y %H:%M"))
test$Datetime = as.POSIXct(strptime(test$Datetime, "%d-%m-%Y %H:%M"))

# Aggregate train data day-wise
aggr_train = train[,list(Count = sum(Count)), by = Date]

# Visualize the data
ggplot(aggr_train) + geom_line(aes(Date, Count))

# Change column names
names(aggr_train) = c("ds", "y")

# Model building
m = prophet(aggr_train)
future = make_future_dataframe(m, periods = 213)
forecast = predict(m, future)

# Visualize forecast
plot(m, forecast)

# proportion of mean hourly 'Count' based on train data
mean_hourly_count = train %>%
  group_by(hour = hour(train$Datetime)) %>%
  summarise(mean_count = mean(Count))

s = sum(mean_hourly_count$mean_count)
mean_hourly_count$count_proportion = mean_hourly_count$mean_count/s

# variable to store hourly Count
test_count = NULL

for(i in 763:nrow(forecast)){
  test_count = append(test_count, mean_hourly_count$count_proportion * forecast$yhat[i])
}

test$Count = test_count
