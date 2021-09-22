#Code for reading in the dataset and/or processing the data
#Histogram of the total number of steps taken each day
#Mean and median number of steps taken each day
#Time series plot of the average number of steps taken
#The 5-minute interval that, on average, contains the maximum number of steps
#Code to describe and show a strategy for imputing missing data
#Histogram of the total number of steps taken each day after missing values are imputed
#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
#All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
require(dplyr)
require(ggplot2)
require(knitr)
#load in the data
activity_data <- read.csv('data/activity.csv')
View(activity_data)

#Histogram of the total number of steps taken each day
total_steps <- activity_data %>%
          group_by(date) %>%
          summarise(step_total = sum(steps, na.rm = TRUE))

View(total_steps)
hist(total_steps$step_total,col = 'steelblue', main = "Total Steps per day", xlab = "Total Steps per day", 
        ylab = 'Frequency' )


#Mean and median number of steps taken each day
summary(total_steps)

#time series plot of the average number of steps taken

interval_steps <- aggregate(steps ~ interval, data=activity_data , mean)
View(interval_steps)
plot(interval_steps$interval, interval_steps$steps, type='l',main = "Average number of steps per day", xlab = "Interval", 
     ylab = 'Avg Number of Steps' )

#The 5-minute interval that, on average, contains the maximum number of steps
max_steps <- which.max(interval_steps$steps)
interval_steps[max_steps, ]

#code to describe and show a strategy for imputing missing data - looking at replacing NAs with the mean
sum(is.na(activity_data))
data_imputed <- activity_data
for (i in 1:nrow(data_imputed)) {
  if (is.na(data_imputed$steps[i])) {
    interval_value <- data_imputed$interval[i]
    steps_value <- interval_steps[
      interval_steps$interval == interval_value,]
    data_imputed$steps[i] <- steps_value$steps
  }
}
#recalculate number of steps ttaken now with missing data inc
activity_data_imputed <- aggregate(steps ~ date, data_imputed, sum)

#histogram of the total number of steps taken each day after missing values are imputed
hist(activity_data_imputed$steps,col = 'steelblue', main = "Total Steps per day - imputed", xlab = "Total Steps per day", 
     ylab = 'Frequency' )


#panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

data_imputed['type_of_day'] <- weekdays(as.Date(data_imputed$date))
data_imputed$type_of_day[data_imputed$type_of_day %in% c('Saturday', 'Sunday')] <-"weekend"
data_imputed$type_of_day[data_imputed$type_of_day != "weekend"] <-"weekday"

#need to convert character to factor
data_imputed$type_of_day <- as.factor(data_imputed$type_of_day)

#get average steps by day
avg_steps_imputed <- aggregate(steps ~ interval + type_of_day, data_imputed, mean)

#panel plot
qplot(interval, steps, data = avg_steps_imputed, type = 'l', geom = c("line"), xlab = "Interval", ylab = "Number of Steps", main = "") + facet_wrap(~ type_of_day, ncol=1)
 