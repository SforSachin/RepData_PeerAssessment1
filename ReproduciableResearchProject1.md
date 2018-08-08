---
title: "Reproduciable Research Project 1"
output: 
  html_document:
    keep_md: true
---



#Calculating and plotting the mean of the total number of steps per day

## Read activity.csv file into "activity" variable


```r
activity <- read.csv(file="C:/Coursera/ReproduciableResearch/activity.csv",sep = ",")

## Add the day field
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
```

## Aggregate total steps per date

```r
AggrTable <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(AggrTable)<- c("Date", "Steps")
```

## Creating the historgram of total steps per day

```r
hist(AggrTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
```

![](/figure/unnamed-chunk-3-1.png)<!-- -->

## Calculate mean and median of daily steps

```r
paste("Mean Steps per Day =", mean(AggrTable$steps))
```

```
## Warning in mean.default(AggrTable$steps): argument is not numeric or
## logical: returning NA
```

```
## [1] "Mean Steps per Day = NA"
```

```r
paste("Median Steps per Day =", median(AggrTable$steps))
```

```
## Warning in is.na(x): is.na() applied to non-(list or vector) of type 'NULL'
```

```
## [1] "Median Steps per Day = "
```

# Calculating and plotting the average daily activity pattern by interval

## pull data without NAs

```r
activityWithoutNA <- activity[!is.na(activity$steps),]
```

## Create Average steps per interval and plot


```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.4.4
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```

```r
intervalTable <- ddply(activityWithoutNA, .(interval), summarize, Avg = mean(steps))

p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
```

![](/figure/unnamed-chunk-6-1.png)<!-- -->

## Output interval that has max value along with the max value

```r
maxSteps <- max(intervalTable$Avg)
paste("Interval with max value =",intervalTable[intervalTable$Avg==maxSteps,1])
```

```
## [1] "Interval with max value = 835"
```

# Imputing missing values to replace NAs in data set and compare results


```r
totalNAs<-nrow(activity[is.na(activity$steps),])

paste("Total rows with steps = 'NA' is=",totalNAs)
```

```
## [1] "Total rows with steps = 'NA' is= 2304"
```

##  strategy for filling in NAs will be to substitute the missing steps with the average 5-minute interval based on the day of the week.


```r
## Create the average number of steps per weekday and interval
avgTable <- ddply(activityWithoutNA, .(interval, day), summarize, Avg = mean(steps))

## Create dataset with all NAs for substitution
nadata<- activity[is.na(activity$steps),]
## Merge NA data with average weekday interval for substitution
newdata<-merge(nadata, avgTable, by=c("interval", "day"))
```

## Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
## Reorder the new substituded data in the same format as clean data set
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval","day","DateTime")

##Merge the NA averages and non NA data together
mergeData <- rbind(activityWithoutNA, newdata2)
```

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
##Create sum of steps per date 
sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(sumTable2)<- c("Date", "Steps")

hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )
```

![](/figure/unnamed-chunk-11-1.png)<!-- -->

```r
paste("Mean daily steps =", as.integer(mean(sumTable2$Steps)))
```

```
## [1] "Mean daily steps = 10821"
```

```r
paste("Median daily steps =",as.integer(median(sumTable2$Steps)))
```

```
## [1] "Median daily steps = 11015"
```

## Note the difference in values:
## The new mean of the imputed data is 10821 steps compared to the old mean of 10766 steps. That creates a difference of 55 steps on average per day.

## The new median of the imputed data is 11015 steps compared to the old median of 10765 steps. That creates a difference of 250 steps for the median.

# Check if differences exist in activity patterns between weekdays and weekends

## Create a field in the merged dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
## Create new category based on the days of the week

mergeData$day<- weekdays(as.Date(activity$date))

mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```

## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice) 
## Summarize data by interval and type of day
intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))

##Plot data in a panel plot
xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```

![](/figure/unnamed-chunk-13-1.png)<!-- -->

## Yes, the step activity trends are different based on whether the day occurs on a weekend or not.
