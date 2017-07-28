---
title: "reproducible_assignment"
author: "ck kang"
date: "2017.7."
output: html_document
---



## Loading and preprocessing the data

Load the data


```r
library(ggplot2)
library(lattice)
rdata <- read.csv("~/project1/activity.csv", header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))
str(rdata)
```

'data.frame':	17568 obs. of  3 variables:
 $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
 $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
 $ interval: num  0 5 10 15 20 25 30 35 40 45 ...

## transform the data

change the class


```r
rdata$date <- as.Date(rdata$date, format = "%Y-%m-%d")
rdata$interval <- as.factor(rdata$interval)
str(rdata)
```

'data.frame':	17568 obs. of  3 variables:
 $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
 $ date    : Date, format: "2012-10-01" "2012-10-01" ...
 $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...

# what is mean total number of steps taken per day?
1. calculation of total no. of steps per day

```r
steps_day <- aggregate(steps ~ date, rdata, sum)
colnames(steps_day) <- c("date","steps")
head(steps_day)
```

        date steps
1 2012-10-02   126
2 2012-10-03 11352
3 2012-10-04 12116
4 2012-10-05 13294
5 2012-10-06 15420
6 2012-10-07 11015

2. histogram of the total number of steps taken each day

```r
hist(steps_day$steps, 
     main=" ",
     breaks=10,
     xlab="Total Number of Steps Taken Daily")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
3. Calculate and report the mean and median of the total number of steps taken per day

```r
steps_mean   <- mean(steps_day$steps, na.rm=TRUE)
steps_median <- median(steps_day$steps, na.rm=TRUE)
steps_mean
```

[1] 10766.19

```r
steps_median
```

[1] 10765

# What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps_interval <- aggregate(steps ~ interval, rdata, mean, na.rm=TRUE)
colnames(steps_interval) <- c("interval","steps")
head(steps_interval)
```

  interval     steps
1        0 1.7169811
2        5 0.3396226
3       10 0.1320755
4       15 0.1509434
5       20 0.0754717
6       25 2.0943396

```r
ggplot(steps_interval,  aes(x = as.numeric(as.character(interval)), y = steps)) + xlab("interval") +theme_light()+geom_line()
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max <- steps_interval$interval[which.max(steps_interval$steps)]
max
```

[1] 835
288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
835 contains maximum number of steps

# Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing_vals <- sum(is.na(rdata$steps))
missing_vals
```

[1] 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
rdata2 <- rdata
which <- is.na(rdata2$steps)

rdata2$steps[which] <- steps_interval$steps[steps_interval$interval == rdata2$interval[which]]
rdata2$steps[is.na(rdata2$steps)] <- mean(rdata$steps, na.rm=TRUE)
head(rdata2)
```

      steps       date interval
1 1.7169811 2012-10-01        0
2 0.3396226 2012-10-01        5
3 0.1320755 2012-10-01       10
4 0.1509434 2012-10-01       15
5 0.0754717 2012-10-01       20
6 2.0943396 2012-10-01       25

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
sum(is.na(rdata2$steps))
```

[1] 0

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_day2 <- aggregate(steps ~ date, rdata2, sum)
colnames(steps_day2) <- c("date","steps")
head(steps_day2)
```

        date    steps
1 2012-10-01 10766.19
2 2012-10-02   126.00
3 2012-10-03 11352.00
4 2012-10-04 12116.00
5 2012-10-05 13294.00
6 2012-10-06 15420.00

```r
steps_mean2   <- mean(steps_day2$steps, na.rm=TRUE)
steps_median2 <- median(steps_day2$steps, na.rm=TRUE)
steps_mean2
```

[1] 10766.19

```r
steps_median2
```

[1] 10766.19
the mean is same, but the median value slighlty goes up and closer to the mean because the missing values are replaced by the mean value of the day and intervals.


```r
hist(steps_day2$steps, 
     breaks=10,
     xlab="Steps Taken Daily", border="blue")
hist(steps_day$steps, breaks=10, border="orange", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "orange"), lwd=5)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)



# Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels <e2>Ä<93> <e2>Äúweekday<e2>Ä<9d> and <e2>Äúweekend<e2>Ä<9d> indicating whether a given date is a weekday or weekend day.

```r
day1 <-weekdays(rdata2$date)
rdata4 <- cbind(rdata2, day1)
head(rdata4)
rdata4$weekday <- ifelse(rdata4$day1 == "<U+653C><U+3E64>Ü†<U+653C><U+3E63>öî<U+653C><U+3E63>ùº" ,  
                         "weekend", (ifelse(rdata4$day1 == "<U+653C><U+3E63>ùº<U+653C><U+3E63>öî<U+653C><U+3E63>ùº" ,  
                                           "weekend", "weekday")))
str(rdata4)
```

```
## Error: invalid multibyte character in parser at line 4
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
steps_day4 <- aggregate(steps ~ interval + weekday, rdata4, mean)
g4 <- ggplot(steps_day4, aes(x = as.numeric(as.character(interval)),y = steps), colour = "weekday")
g4 + geom_line() + facet_grid(. ~ weekday)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)
