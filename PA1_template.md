# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip", "activity.csv")
data <- read.csv("activity.csv")
clean_data <- subset (data, !is.na(steps))
head(clean_data)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

## What is mean total number of steps taken per day?

```r
library(plyr)
steps_per_day <- ddply(clean_data, .(date), summarize,  steps=sum(steps))
hist(steps_per_day$steps, main="Histogram of the total steps taken each day", 
     xlab="Steps per day", col="orange", breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

 - Mean and median of the total number of steps taken per day


```r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
five_min_interval <- ddply(clean_data, .(interval), summarize,  steps=mean(steps))
plot(five_min_interval$interval, five_min_interval$steps, type="l", 
     main="Average daily activity pattern", col="blue", 
     xlab="5-minute interval", ylab="Steps count")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

 - 5-minute interval which contains the maximum number of steps


```r
five_min_interval[which.max(five_min_interval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

Observations:

Based on steps taken pattern, the person's daily activity peaks around 8:35am.

## Imputing missing values

 - The total number of missing values in the dataset


```r
na_data <- subset(data, is.na(steps))
nrow(na_data)
```

```
## [1] 2304
```

 - Replace NAs with mean for that day
 

```r
 modified_data <- data
 impute <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
 modified_data$steps <- impute(data$steps)
 head(modified_data)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```
 
  - Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
  

```r
steps_per_day1 <- ddply(modified_data, .(date), summarize,  steps=sum(steps))
hist(steps_per_day1$steps, main="Histogram of the total steps taken each day", 
    xlab="Steps per day", col="orange", breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
mean(steps_per_day1$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day1$steps)
```

```
## [1] 10766.19
```

Observations:

 -  Do these values differ from the estimates from the first part of the assignment?- Mean differs by one which means values remain pretty much the same.
 - What is the impact of imputing missing data on the estimates of the total daily number of steps? - Shape of the histogram remains the same but frquency count increases. 
 
## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
modified_data$dateType <-  ifelse(as.POSIXlt(modified_data$date)$wday %in% c(0,6), 'weekend', 'weekday')
head(modified_data)
```

```
##     steps       date interval dateType
## 1 37.3826 2012-10-01        0  weekday
## 2 37.3826 2012-10-01        5  weekday
## 3 37.3826 2012-10-01       10  weekday
## 4 37.3826 2012-10-01       15  weekday
## 5 37.3826 2012-10-01       20  weekday
## 6 37.3826 2012-10-01       25  weekday
```

Make a panel plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(ggplot2)
avg_data <- aggregate(steps ~ interval + dateType, data=modified_data, mean)
 ggplot(avg_data, aes(interval, steps)) + 
     geom_line() + 
     facet_grid(dateType ~ .) +
     xlab("5-minute interval") + 
     ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

Observations

Are there differences in activity patterns between weekdays and weekends? Yes. The plot indicates that the person moves around more (or more active) during the weekend days.
