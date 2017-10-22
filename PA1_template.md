# Reproducible Research: Peer Assessment 1


```r
echo = TRUE
```

## **Loading and preprocessing the data**
1.Load the data (i.e. read.csv())  
2. Process/transform the data (if necessary) into a format suitable for your analysis  

```r
totalfile <- read.csv(file="activity.csv", sep=",", header=TRUE)  
remfile <- na.omit(totalfile)  
daily <- aggregate(steps ~ date, remfile, sum)  
```

## **What is mean total number of steps taken per day?**

1.Make a histogram of the total number of steps taken each day  

```r
hist(daily$steps, main="Histogram of Average steps taken each day", xlab="Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

2.Calculate and report the mean and median total number of steps taken per day  

```r
mean(daily$steps)
```

```
## [1] 10766.19
```

```r
median(daily$steps)  
```

```
## [1] 10765
```

## **What is the average daily activity pattern?**
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgInt <- aggregate(steps ~ interval, remfile, mean)
plot(type="l", x=avgInt$interval, y=avgInt$steps)  
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgInt[avgInt$steps == max(avgInt$steps),]  
```

```
##     interval    steps
## 104      835 206.1698
```

## **Imputing missing values**
1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(totalfile))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

*Put the average for the interval into the missing value  

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newtotal <- totalfile
for(i in 1:nrow(newtotal)){
      if(is.na(newtotal$steps[i])){
            newtotal$steps[i] <- avgInt[which(avgInt$interval == newtotal$interval[i]),]$steps
      }
}
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
newDaily <- aggregate(steps ~ date, newtotal, sum)
hist(newDaily$steps, main="Histogram of Average steps taken each day", xlab="Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

The values are the same for mean but for Median they are, in fact, different.  

```r
newm <- c(mean(newDaily$steps), median(newDaily$steps))
oldm <- c(mean(daily$steps), median(daily$steps))
averages <- data.frame(newm,oldm)
row.names(averages)<-c("Mean","Median")
colnames(averages)<-c("New","Old")
averages
```

```
##             New      Old
## Mean   10766.19 10766.19
## Median 10766.19 10765.00
```

## **Are there differences in activity patterns between weekdays and weekends?**
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
weeks <- newtotal
weeks$date <- as.Date(weeks$date, "%Y-%m-%d")
weeks$days <- weekdays(weeks$date)
for(i in 1:nrow(weeks)){
      if(weeks$days[i]=="Monday" | weeks$days[i]=="Tuesday"|weeks$days[i]=="Wednesday"|weeks$days[i]=="Thursday"|weeks$days[i]=="Friday"){
            weeks$work[i] <- "Weekday"
      } else{weeks$work[i]<-"Weekend"}
}
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
avgIntW <- aggregate(steps ~ interval + work, weeks, mean)
library(lattice)
xyplot(avgIntW$steps ~ avgIntW$interval | avgIntW$work, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

