---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
---


## Loading and preprocessing the data

Loading the data:


```r
unzip('activity.zip')
activity <- read.csv('activity.csv',na.strings=c("NA", "NULL"))
```






## What is mean total number of steps taken per day?

Calculating the total number of steps taken per day:

```r
activityNoNA <- activity[complete.cases(activity),]
stepsPerDay <- tapply(activity$steps, activity$date, sum)
```

Making a histogram of the total number of steps taken each day:

```r
png("stepsPerDay.png")
hist(stepsPerDay, xlab= "Steps Per Day", main= " ", col=c("green"))
dev.off()
```

```
## RStudioGD 
##         2
```

Calculating the mean and median of the total number of steps taken per day:

```r
mean <- mean(activityNoNA[,1])
median <- median(activityNoNA[,1])
```
_The mean is __37.3825996__ and the median is __0__._



## What is the average daily activity pattern?

1. Making a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsInterval <- aggregate(steps ~ interval, data = activityNoNA, FUN = "mean")
 png("figures/dailyActivity.png")
plot(stepsInterval, type="l",xlab="Interval", ylab="Steps", col=c("red"), main = "Daily Activity Pattern")
interval = stepsInterval$interval[which.max(stepsInterval$steps)]
dev.off()
```

```
## RStudioGD 
##         2
```



2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

_The 5-minute interval that contains the maximum number of steps is the __835th__._








## Imputing missing values
1. Calculating the total number of missing values in the dataset:

```r
valuesMissing <- sum(is.na(activity))
```

_The number of missing values is __2304__._


2. Devising a strategy for filling in all of the missing values in the dataset:

_The missing values will be replaced by the mean based on its interval._

3. Creating the new database:

```r
stepsInterval <- aggregate(steps ~ interval, data = activityNoNA, FUN = "mean")
activityFilled <- activity 
for (i in 1:nrow(activityFilled)) {
if (is.na(activityFilled[ i ,1]))
{
int <- activityFilled[i,3]
x <-   stepsInterval[(stepsInterval$interval == int),2]
activityFilled[i,1] <- x
}

}
summary(activityFilled)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```
 
4. Making a histogram of the total number of steps taken each day and calculating the mean and median total number of steps taken per day. 



```r
mean <- mean(activityFilled[,1])
median <- median(activityFilled[,1])
stepsPerDay <- tapply(activityFilled$steps, activityFilled$date, sum)
png("stepsPerDayNonoise.png")
hist(stepsPerDay, xlab= "Steps Per Day", main="After Filling NA Values with Means",col=c("yellow"))
dev.off()
```

```
## RStudioGD 
##         2
```

####Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The values of steps per day have risen notably after the missing values were replaced with the mean.











## Are there differences in activity patterns between weekdays and weekends?
_Based on the steps below, there is observable differences between the two._

 1. Creating a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
 

```r
activityFilled$day <- activityFilled$date
class(activityFilled$day) <- as.factor(activityFilled$day)

activityFilled$day <- weekdays(as.Date(activityFilled$date))

activityFilled$daytype <- ifelse(activityFilled$day %in% c("Monday", "Tuesday", "Wednesday","Thursday", "Friday"), "weekday", "weekend")


 activityWeekdays <- activityFilled[activityFilled$day %in% c("Monday", "Tuesday", "Wednesday","Thursday", "Friday"),]
activityWeekends <- activityFilled[activityFilled$day %in% c("Sunday",  "Saturday"), ]
stepsIntervalWeekdays <- aggregate(steps ~ interval, data = activityWeekdays, FUN = "mean")
stepsIntervalWeekends <- aggregate(steps ~ interval, data = activityWeekends, FUN = "mean")
```


2. Making a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


 
 ```r
 png("figures/DayTypes.png")
 par(mfrow=c(2,1))
 par(mar=c(0.5, 4.5, 0.5, 0.5))
 plot(stepsIntervalWeekdays$interval, stepsIntervalWeekdays$steps, type="l",ylab="", xaxt='n', ann=TRUE , col=c("blue"))
 title(main="Daily Activity for Weekdays", line = -1)
 plot(stepsIntervalWeekends$interval, stepsIntervalWeekends$steps, type="l",ylab="",  xaxt='n',ann=TRUE,col=c("blue") )
 title(main="Daily Activity for Weekends",line = -1)
 dev.off()
 ```
 
 ```
 ## RStudioGD 
 ##         2
 ```
 
 


