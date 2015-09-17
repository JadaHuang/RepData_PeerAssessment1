# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# Load the data
df <- read.csv("activity.csv")

# Process/transform the data (if necessary) into a format suitable for your analysis
df$date <- as.Date(df$date)
```

## What is mean total number of steps taken per day?

```r
# Calculate the total number of steps taken per day
total_steps_per_day <- tapply(df$steps, df$date, sum, na.rm = TRUE)

# Make a histogram of the total number of steps taken each day
hist(total_steps_per_day)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# Calculate and report the mean and median of the total number of steps taken per day
mean(total_steps_per_day)
```

```
## [1] 9354.23
```

```r
median(total_steps_per_day)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
average_steps_by_interval <- tapply(df$steps, df$interval, mean, na.rm=T)
plot(names(average_steps_by_interval), average_steps_by_interval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
names(which.max(average_steps_by_interval))
```

```
## [1] "835"
```

## Imputing missing values

```r
# Calculate and report the total number of missing values in the dataset
nrow(df[is.na(df$steps),])
```

```
## [1] 2304
```

```r
# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
x <- as.data.frame(average_steps_by_interval)
x <- cbind(row.names(x), x)
names(x) <- c("interval", "steps")

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
new_df <- merge(df, x, by = "interval", all.x = T)
new_df[is.na(new_df$steps.x),"steps.x"] <- new_df[is.na(new_df$steps.x),"steps.y"]

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
new_total_steps_per_day <- tapply(new_df$steps.x, new_df$date, sum, na.rm = TRUE)
hist(new_total_steps_per_day)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean(new_total_steps_per_day)
```

```
## [1] 10766.19
```

```r
median(new_total_steps_per_day)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
new_df$weekdays <- as.factor(weekdays(new_df$date)=="星期六" | weekdays(new_df$date)=="星期日")
levels(new_df$weekdays) <- c("weekday", "weekend")

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
par(mfrow= c(2,1))
weekday_average_steps <- tapply(new_df[new_df$weekdays=="weekday","steps.x"], new_df[new_df$weekdays=="weekday", "interval"], mean, na.rm=T)
plot(names(weekday_average_steps), weekday_average_steps, type="l", xlab = "", ylab = "weekday")

weekend_average_steps <- tapply(new_df[new_df$weekdays=="weekend","steps.x"], new_df[new_df$weekdays=="weekend", "interval"], mean, na.rm=T)
plot(names(weekend_average_steps), weekend_average_steps, type="l", ylab = "weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
