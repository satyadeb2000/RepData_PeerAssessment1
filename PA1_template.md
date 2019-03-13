
---
title: "Assigment - Reproducable Research.rmd"
author: "Satya Majumder"
date: "March 12, 2019"
output: html_document
---


```
## Warning: package 'plyr' was built under R version 3.5.3
```

```
## Warning: package 'lattice' was built under R version 3.5.3
```


##Loading and Cleaning the data.....

```r
    setwd("C:/Users/i376752/Desktop/personal/R assignment")
    destfilename <- "activity.zip"
    if (!file.exists(destfilename)){
      sourceURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
      download.file(sourceURL, destfilename)
    }  
    if (!file.exists("activity.csv")) { 
      unzip(destfilename) 
    }
    activity <- read.csv("activity.csv", header=TRUE,na.strings = "NA")
    activity$day <- weekdays(as.Date(activity$date))
    activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
    cleanData <- activity[!is.na(activity$steps),]
```

##Printing the head of the cleaned up data

```r
    print(head(cleanData))
```

```
##     steps       date interval     day   DateTime
## 289     0 2012-10-02        0 Tuesday 2012-10-02
## 290     0 2012-10-02        5 Tuesday 2012-10-02
## 291     0 2012-10-02       10 Tuesday 2012-10-02
## 292     0 2012-10-02       15 Tuesday 2012-10-02
## 293     0 2012-10-02       20 Tuesday 2012-10-02
## 294     0 2012-10-02       25 Tuesday 2012-10-02
```
#Working with Cleaned Data
##Calculate Averages and Build graphs
Total number of steps taken per day

```r
    StepsPerDay <- aggregate(cleanData$steps ~ cleanData$date, FUN=sum)
    colnames(StepsPerDay)<- c("Date", "Steps")
```

Histogram of total steps per day

```r
  hist(StepsPerDay$Steps,  xlab="Steps", main = "Total Steps per Day")
```

![plot of chunk histogram](figure/histogram-1.png)

Mean of steps per day

```r
  as.integer(mean(StepsPerDay$Steps))
```

```
## [1] 10766
```

Median of steps per day

```r
  as.integer(median(StepsPerDay$Steps))
```

```
## [1] 10765
```

Time Series plot of steps per interval

```r
  StepsPerInterval <- aggregate(cleanData$steps ~ cleanData$interval, FUN=mean)
  colnames(StepsPerInterval)<- c("Interval", "Steps")
  plot(StepsPerInterval$Interval, StepsPerInterval$Steps, type="l", xlab = "Interval", ylab = "Steps", col = "black",  lwd=2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
  maxSteps <- max(StepsPerInterval$Steps)
  StepsPerInterval[StepsPerInterval$Steps == maxSteps, 1]
```

```
## [1] 835
```

#Working with Missing Data
##Number of NAs in original data set

```r
  nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```
##Substitute Missing Data:
*Substitute the missing steps with the average 5-minute interval based on the day of the week.
*Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
  avgTable <- ddply(cleanData, .(interval, day), summarize, Avg = mean(steps))
  nadata<- activity[is.na(activity$steps),]
  newdata<-merge(nadata, avgTable, by=c("interval", "day"))
  newdata2<- newdata[,c(6,4,1,2,5)]
  colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")
  mergeData <- rbind(cleanData, newdata2)
  print(head(mergeData))
```

```
##     steps       date interval     day   DateTime
## 289     0 2012-10-02        0 Tuesday 2012-10-02
## 290     0 2012-10-02        5 Tuesday 2012-10-02
## 291     0 2012-10-02       10 Tuesday 2012-10-02
## 292     0 2012-10-02       15 Tuesday 2012-10-02
## 293     0 2012-10-02       20 Tuesday 2012-10-02
## 294     0 2012-10-02       25 Tuesday 2012-10-02
```

##Calculate Averages and build graphs with mising data
Total number of steps taken each day with mising values filled in

```r
  StepsPerDay2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
  colnames(StepsPerDay2)<- c("Date", "Steps")
```

Mean of total number of steps per day with missing values filled in

```r
  as.integer(mean(StepsPerDay2$Steps))
```

```
## [1] 10821
```

Median of total number of steps per day with missing values filled in

```r
  as.integer(median(StepsPerDay2$Steps))
```

```
## [1] 11015
```

Histogram of total steps per day, categorized by data set to show impact

```r
  hist(StepsPerDay2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="maroon")
  hist(StepsPerDay$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="blue", add=T)
  legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("maroon", "blue") )
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

The new mean of the imputed data is 10821 steps compared to the old mean of 10766 steps. That creates a difference of 55 steps on average per day.

The new mean of the imputed data is 11015 steps compared to the old mean of 10765 steps. That creates a difference of 250 steps on average per day.

#Are there differences in activity patterns between weekdays and weekends?
*Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
*Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
  mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))
  xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)


