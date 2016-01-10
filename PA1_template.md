# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
# Unzip and read the data
unzip("activity.zip")
activity <- read.csv("activity.csv")
```


```r
#Load packages used
library(plyr)
library(dplyr)
library(ggplot2)
```

```r
# Convert date column from factor to date format
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?


```r
# Group data by date and calculate total number of steps per day
by_date <- group_by(activity, date)
per_day <- summarize(by_date, sum(steps))
```

By plotting steps per day in a histogram, we can see that the mean would be just over 10,000 steps per day. 

```r
hist(per_day$`sum(steps)`, breaks = 10, main = "Histogram of Total Steps Per Day", 
     xlab = "Total Steps Per Day", col = 'blue')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

We can also calculate the exact mean and median total steps per day:

```r
mean(per_day$`sum(steps)`, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(per_day$`sum(steps)`, na.rm = T)   
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
# Group data by inteval and calculate mean steps per interval
by_interval <- group_by(activity, interval)
mean_per_interval <- summarize(by_interval, mean(steps, na.rm = T))

plot(mean_per_interval$interval, mean_per_interval$`mean(steps, na.rm = T)`, type = 'l',
     xlab = "Interval", ylab = "Average number of steps", col = "darkgreen")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

It appears from the plot that the interval with the highest average number of steps is close to 850 (or close to 2:10 PM). We can get more specific by identifying the interval from the mean steps per interval dataset with the highest number of steps. That interval is 835 (or 1:55 PM).

```r
max_int <- which.max(mean_per_interval$`mean(steps, na.rm = T)`)
mean_per_interval[max_int, 1]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
##      (int)
## 1      835
```

## Imputing missing values
A quick examination of the original datasets reveals there are 2304 missing values,and all of those occur in the "steps" variable.

```r
sapply(activity, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```

We can impute the missing data by replacing the NAs with the mean number of steps for the specific 5-minute interval. I approached this by adding a column with mean of each interval, and then replacing the NAs with the value in that column:

```r
newdata <- ddply(by_interval, .(interval), transform, mean = mean(steps, na.rm = T))

for(i in 1:nrow(newdata)) {
      if(is.na(newdata[i , 1])) {
            newdata[i, 1] = newdata[i, 4]
      }
}
```

Let's look at the total number of steps per day now that the missing values have been replaced.

```r
new_by_date <- group_by(newdata, date)
new_per_day <- summarize(new_by_date, sum(steps))
hist(new_per_day$`sum(steps)`, breaks = 10, main = "Histogram of Total Steps Per 
     Day (NAs Imputed)", 
     xlab = "Total Steps Per Day", col = 'blue')
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

The shape histogram looks similar to the one we created without missing data. The mean does not change, and the median changes only very slightly:

```r
mean(new_per_day$`sum(steps)`)
```

```
## [1] 10766.19
```

```r
median(new_per_day$`sum(steps)`) 
```

```
## [1] 10766.19
```

This makes sense given the selected approach for imputing the data. Since most missing values were associated with entire missing days, replacing the the values of each interval with the mean for that interval created days in which the total for the day was equal to the mean. 

## Are there differences in activity patterns between weekdays and weekends?

To answer this, rows were labeled as "weekend"" or "weekday" by adding a column to identify the day of the week and
then replacing those labels with "weekend"" or "weekday".


```r
newdata$day <- weekdays(newdata$date)
for(i in 1:nrow(newdata)) {
      if(newdata[i , 5] == "Saturday" | newdata[i , 5] == "Sunday" ) {
            newdata[i, 5] = "weekend"
      } else {
            newdata[i, 5] = "weekday"
      }
}
```

This new data can be grouped by "weekend" or "weekday" and by plotting these datasets together we can see if there are differences in weekday vs weekend patterns.


```r
split_week <- group_by(newdata, day, interval)
mean_per_interval2 <- summarize(split_week, mean(steps))

p <- qplot(interval, mean_per_interval2$`mean(steps)`, data = mean_per_interval2, 
      facets = day ~ ., geom = "line", ylab = "Mean Number of Steps") 
p + theme_bw(base_size = 24)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

Yes, there are differenes in activity patterns between weekdays and weekends. It appears that activity increases earlier in the day and decreases earlier in the evening on weekdays compared to weekends. However, on weekends, there are more peaks of higher activity throughout the day.
