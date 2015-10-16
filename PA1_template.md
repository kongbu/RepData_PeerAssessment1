# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip(zipfile="activity.zip")
activitydata <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
library(ggplot2)
steps_eachday <- tapply(activitydata$steps, activitydata$date, sum, na.rm=TRUE)
qplot(steps_eachday, binwidth=800, main = "Histogram for number of steps taken per day", xlab="total number of steps taken per day", fill=I("blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(steps_eachday, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(steps_eachday, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
averages <- tapply(activitydata$steps, activitydata$interval, mean, na.rm=TRUE) 
plot(averages, type = "l", main = "Average steps taken per 5-min interval", xlab="5-min interval", ylab = "average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 
### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
activitydata$interval[which.max(averages)]
```

```
## [1] 835
```

## Imputing missing values

###1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
notavailables <- is.na(activitydata$steps)
numberMissing <- sum(notavailables)
numberMissing
```

```
## [1] 2304
```
###2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. And 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
imputeddata <- activitydata
averageOf5min <- aggregate(x=list(steps=imputeddata$steps), by=list(intervals=imputeddata$interval),mean, na.rm=TRUE)
impute <- function(steps, intervals) {
    fill <- NA
    if (!is.na(steps))
        fill <- c(steps)
    else
        fill <- (averageOf5min[averageOf5min$interval==intervals, "steps"])
    return(fill)
}
imputeddata$steps <- mapply(impute, imputeddata$steps, imputeddata$interval)
```
###4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
library(ggplot2)
steps_eachday2 <- tapply(imputeddata$steps, imputeddata$date, sum, na.rm=TRUE)
qplot(steps_eachday2, binwidth=800, main = "Histogram for number of steps taken per day", xlab="total number of steps taken per day", fill=I("blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
mean(steps_eachday2, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(steps_eachday2, na.rm=TRUE)
```

```
## [1] 10766.19
```
### The mean and median value is different from the first part of the assignment. THe imputing fill all the NAs and the NAs will count zero in histogram. Therefore, the mean and median increase.

## Are there differences in activity patterns between weekdays and weekends?

### 1 Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
weekdayend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else (day %in% c("Saturday", "Sunday"))
        return("weekend")
}
imputeddata$date <- as.Date(imputeddata$date)
imputeddata$day <- sapply(imputeddata$date, weekdayend)
```
### 2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
averages <- aggregate(steps ~ interval + day, data=imputeddata, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-min interval") + ylab("average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 
