# Reproducible Research Project 1 original R code
# Unzip and read in data
unzip("activity.zip")
activity <- read.csv("activity.csv")


# Load packages used
library(plyr)
library(dplyr)
library(ggplot2)

# Convert date column from factor to date format
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

# Group data by date and calculate total number of steps per day
by_date <- group_by(activity, date)
per_day <- summarize(by_date, sum(steps))


## Histogram
hist(per_day$`sum(steps)`, breaks = 10, main = "Histogram of Total Steps Per Day", 
     xlab = "Total Steps Per Day", col = 'blue')

# Calculate mean and median steps per day 
mean(per_day$`sum(steps)`, na.rm = T)
median(per_day$`sum(steps)`, na.rm = T)   

# Group data by interval and calculate mean total number of steps per interval
by_date <- group_by(activity, date)
by_interval <- group_by(activity, interval)
mean_per_interval <- summarize(by_interval, mean(steps, na.rm = T))

# Plot time series
plot(mean_per_interval$interval, mean_per_interval$`mean(steps, na.rm = T)`, type = 'l',
     xlab = "Interval", ylab = "Average number of steps", col = "darkgreen")

# interval with max avg steps
max_int <- which.max(mean_per_interval$`mean(steps, na.rm = T)`)
mean_per_interval[max_int, 1]

# Calculate the number of NAs in dataset:
sapply(activity, function(x) sum(is.na(x)))

# Replace NAs in steps with mean steps for that interval
# Add a column with mean of each interval

newdata <- ddply(by_interval, .(interval), transform, mean = mean(steps, na.rm = T))

#For each NA value, replace with the mean for that interval
for(i in 1:nrow(newdata)) {
      if(is.na(newdata[i , 1])) {
            newdata[i, 1] = newdata[i, 4]
      }
}

# Calculate total steps perday
new_by_date <- group_by(newdata, date)
new_per_day <- summarize(new_by_date, sum(steps))


# Histogram
hist(new_per_day$`sum(steps)`, breaks = 10, main = "Histogram of Total Steps Per 
     Day (NAs Imputed)", xlab = "Total Steps Per Day", col = 'blue')

# Calculate mean and median of new dataset
mean(new_per_day$`sum(steps)`)
median(new_per_day$`sum(steps)`) 

# Label rows as weekend or weekday by adding a column to ID the day of the week and
# then replacing those with weekday or weekend.
newdata$day <- weekdays(newdata$date)

for(i in 1:nrow(newdata)) {
      if(newdata[i , 5] == "Saturday" | newdata[i , 5] == "Sunday" ) {
            newdata[i, 5] = "weekend"
      } else {
            newdata[i, 5] = "weekday"
      }
}

# Group data by weekend/weekday and calculate mean steps per interval for each group
split_week <- group_by(newdata, day, interval)
mean_per_interval2 <- summarize(split_week, mean(steps))

# Panel plot to compare weekday and weekend    
p <- qplot(interval, mean_per_interval2$`mean(steps)`, data = mean_per_interval2, 
      facets = day ~ ., geom = "line", ylab = "Mean Number of Steps") 
p + theme_bw(base_size = 24)
