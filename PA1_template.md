# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
unzip("activity.zip")
activity <- read.csv("activity.csv", colClasses = c("integer", "character", 
    "character"))
activity$time <- factor(activity$interval, levels = activity$interval[1:288])
activity$day <- strptime(activity$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

Let's take a look at the daily number of steps.


```r
stepsByDate <- by(activity$steps, activity$date, sum)
hist(stepsByDate, breaks = 20, xlab = "Number of daily steps", main = "Histogram of number of daily steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


Compute mean and median:


```r
meanDailySteps <- mean(stepsByDate, na.rm = T)
meanDailySteps
```

```
## [1] 10766
```

```r
medianDailySteps <- median(stepsByDate, na.rm = T)
medianDailySteps
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
stepsByInterval <- by(activity$steps, activity$time, mean, na.rm = T)
plot(stepsByInterval, type = "l", xlab = "Interval", ylab = "Average # of steps", 
    xaxt = "n")
ticks <- seq(from = 1, by = 24, to = 288)
axis(1, at = activity$time[ticks], labels = activity$time[ticks])
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


Which interval has the most steps on average:


```r
maxStepInterval <- activity$interval[which.max(stepsByInterval)]
maxStepInterval
```

```
## [1] "835"
```


## Imputing missing values

Number of missing values:


```r
missingValues <- is.na(activity$steps)
sum(missingValues)
```

```
## [1] 2304
```


There are 2304 missing values.

`activityFilled` has missing values replaced with the average number of steps for that interval (so if a step is missing in a row and its interval is 835, then it will be filled with the average for that interval).


```r
missingSteps <- stepsByInterval[activity[missingValues, "time"]]
stepsFilled <- activity$steps
stepsFilled[missingValues] <- round(stepsByInterval[activity[missingValues, 
    "time"]])
activityFilled <- cbind(activity, stepsFilled)
```


Updated Histogram:


```r
stepsByDateFilled <- by(activityFilled$stepsFilled, activity$date, sum)
hist(stepsByDateFilled, breaks = 20, xlab = "Number of daily steps", main = "Histogram of number of daily steps with missing values filled")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


Compute mean and median:


```r
meanDailyStepsFilled <- mean(stepsByDateFilled, na.rm = T)
meanDailyStepsFilled
```

```
## [1] 10766
```

```r
medianDailyStepsFilled <- median(stepsByDateFilled, na.rm = T)
medianDailyStepsFilled
```

```
## [1] 10762
```


The mean is unchanged but the median is slightly less (1.0762 &times; 10<sup>4</sup> is less that 10765).


```r
totalDailySteps <- sum(stepsByDate, na.rm = T)
totalDailyStepsFilled <- sum(stepsByDateFilled, na.rm = T)
```


With imputed missing values, the total number of steps is 6.567 &times; 10<sup>5</sup> compared to 570608 previously.


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable for the type of day (weekday or weekend):


```r
activity$dayType <- factor(ifelse(weekdays(activity$day) %in% c("Saturday", 
    "Sunday"), "weekend", "weekday"))
```


Plot number of steps by day type:


```r
weekdayActivity <- subset(activity, activity$dayType == "weekday")
weekdayByInterval <- by(weekdayActivity$steps, weekdayActivity$time, mean, na.rm = T)

weekendActivity <- subset(activity, activity$dayType == "weekend")
weekendByInterval <- by(weekendActivity$steps, weekendActivity$time, mean, na.rm = T)

ticks <- seq(from = 1, by = 24, to = 288)
par(mfrow = c(2, 1))
par(mar = c(1, 3, 1, 1))
plot(weekdayByInterval, type = "l", xlab = "", ylab = "", xaxt = "n", main = "weekday")
par(mar = c(2, 3, 1, 1))
plot(weekendByInterval, type = "l", xlab = "", ylab = "", xaxt = "n", main = "weekend")
axis(1, at = activity$time[ticks], labels = activity$time[ticks])
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

