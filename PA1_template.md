# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
unzip("activity.zip")
activity_data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
1. Construct a histogram of the total number of steps taken each day


```r
steps_date <- aggregate(steps ~ date, data=activity_data, FUN=sum)
barplot(steps_date$steps, names.arg=steps_date$date, xlab="date", ylab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate **mean** and **median** total number of
   steps taken per day and report them


```r
m <- mean(steps_date$steps)
md <- median(steps_date$steps)
```

The mean is **1.0766189\times 10^{4}** and median is **10765**.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute
   interval (x-axis) and the average number of steps taken, averaged
   across all days (y-axis)


```r
steps_interval <- aggregate(steps ~ interval, data=activity_data, FUN=mean)
plot(steps_interval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the
   dataset, contains the maximum number of steps?


```r
max <- steps_interval$interval[which.max(steps_interval$steps)]
```

The 5-minute interval with the maximum number of steps is **835**.

## Imputing missing values

1. Calculate and report the total number of missing values in the
   dataset (i.e. the total number of rows with `NA`s)


```r
missing <- sum(is.na(activity_data))
```

The total number of missing values in the dataset are **2304**.

2. Devise a strategy for filling in all of the missing values in the
   dataset. The strategy does not need to be sophisticated. For
   example, you could use the mean/median for that day, or the mean
   for that 5-minute interval, etc.

In the assignment I will use the means for the 5-minute intervals for filling the missing
values.

3. Create a new dataset that is equal to the original dataset but with
   the missing data filled in.


```r
activity_data1 <- merge(activity_data, steps_interval, by="interval", suffixes=c("",".y"))
nas <- is.na(activity_data1$steps)
activity_data1$steps[nas] <- activity_data1$steps.y[nas]
activity_data1 <- activity_data1[,c(1:3)]
```

4. Make a histogram of the total number of steps taken each day and
   Calculate and report the **mean** and **median** total number of
   steps taken per day. Do these values differ from the estimates from
   the first part of the assignment? What is the impact of imputing
   missing data on the estimates of the total daily number of steps?


```r
steps_date <- aggregate(steps ~ date, data=activity_data1, FUN=sum)
barplot(steps_date$steps, names.arg=steps_date$date, xlab="date", ylab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
m1 <- mean(steps_date$steps)
md1 <- median(steps_date$steps)
```

The mean number of steps taken per day for the new dataset is  **1.0766189\times 10^{4}** and median is **1.0766189\times 10^{4}**.

The mean from the previous data set was **1.0766189\times 10^{4}** and median was **10765**.

The impact of the missing data seems rather low, at least when
estimating the total number of steps per day. Median had a slight
increase in its value.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels --
   "weekday" and "weekend" indicating whether a given date is a
   weekday or weekend day.


```r
day_type <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity_data1$day_type <- as.factor(sapply(activity_data1$date, day_type))
```

2. Make a panel plot containing a time series plot (i.e. `type = "l"`)
   of the 5-minute interval (x-axis) and the average number of steps
   taken, averaged across all weekday days or weekend days
   (y-axis).


```r
par(mfrow=c(2,1))
for (type in c("weekend", "weekday")) {
    steps_type <- aggregate(steps ~ interval, data=activity_data1, subset=activity_data1$day_type==type, 
                  FUN=mean)
    plot(steps_type, type="l", main=type)
}
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 
