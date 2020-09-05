---
title: "PROJECT 1"
author: "Lluc"
date: "5/9/2020"
output: html_document
---



# PROYECT 1: reproducible Research


###Loading and preprocessing the data

We open the file and read the data:

```r
activity<-read.csv("activity.csv")
tail(activity)
head(activity)

sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
sum(is.na(activity$date))
```

```
## [1] 0
```

```r
sum(is.na(activity$interval))
```

```
## [1] 0
```

###What is mean total number of steps taken per day?

For this part of the assignment, we ignore the missing values in the dataset.
We calculate the total number of daily steps:


```r
totalSteps<-aggregate(steps~date,data=activity,sum,na.rm=TRUE)
totalSteps
```

We create a histogram (hist1) to analyze the data:


```r
hist(totalSteps$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

We find the mean and median of the initial data:


```r
mean(totalSteps$steps) 
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps) 
```

```
## [1] 10765
```

###What is the average daily activity pattern?

We create a time series plot of the average number of steps taken:


```r
stepsInterval<-aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
stepsInterval
plot(steps~interval,data=stepsInterval,type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

The 5-minute interval that contains the maximum number of steps:


```r
max(stepsInterval$steps)
```

```
## [1] 206.1698
```

```r
stepsInterval[which.max(stepsInterval$steps),]$interval
```

```
## [1] 835
```

```r
interval2steps<-function(interval){
  stepsInterval[stepsInterval$interval==interval,]$steps
}
```

###Imputing missing values


We calculate the number of missing data:


```r
activityFilled<-activity

count=0
for(i in 1:nrow(activityFilled)){
  if(is.na(activityFilled[i,]$steps)){
    activityFilled[i,]$steps<-interval2steps(activityFilled[i,]$interval)
    count=count+1
  }
}
cat("Total ",count, "NA values were filled.\n\r")
```

```
## Total  2304 NA values were filled.
## 
```

We fill in the missing data with the previous average:


```r
totalSteps2<-aggregate(steps~date,data=activityFilled,sum)
totalSteps2
```

We generate the new histogram (hist2):

```r
hist(totalSteps2$steps)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

We calculate the new mean and the new median:


```r
mean(totalSteps2$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps2$steps)
```

```
## [1] 10766.19
```

###Are there differences in activity patterns between weekdays and weekends?

We differentiate between weekdays and weekends and compare both groups using a graph (comp):


```r
activityFilled$day=ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6==0,
                          "weekend","weekday")
activityFilled$day=factor(activityFilled$day,levels=c("weekday","weekend"))

stepsInterval2=aggregate(steps~interval+day,activityFilled,mean)

library(lattice)
xyplot(steps~interval|factor(day),data=stepsInterval2,aspect=1/2,type="l")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

