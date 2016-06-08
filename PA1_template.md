# Peer Assesment
Soni Baidya 
February 7, 2016  

# Introduction

This report makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


# Data

[Activity Monitoring Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
 
The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


## Loading Data into R


```r
#set file path
filePath <- "F:/Pratssubedi/Coursera-DataScience/Rep Research/repdata-data-activity/activity.csv"
# read file as dataset
activity_data <- read.csv(filePath)
```


## Task 1: What is mean total number of steps taken per day?

* For this part of the assignment, you can ignore the missing values in the dataset.
* Calculate the total number of steps taken per day
* If you do not understand the difference between a histogram and a barplot, research the   difference between them. 
* Make a histogram of the total number of steps taken each day
* Calculate and report the mean and median of the total number of steps taken per day

We will aggregate the steps for each day and plot a histogram for this.


```r
# Get total number of steps for each day
steps_taken_per_day<- aggregate(cbind(activity_data$steps) ~ date, data=activity_data, FUN=sum)
#set proper column names
colnames(steps_taken_per_day) <- c("dates", "total_Steps_Per_Day")

#plot histogram for total number of steps taken each data
hist(x=steps_taken_per_day$total_Steps_Per_Day,
     col="dark blue",
     xlab="Total steps per day",
     ylab="Frequency",
     main="The distribution of total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

Now calculating the mean and median:


```r
#mean of total steps for each day
mean(steps_taken_per_day$total_Steps_Per_Day)
```

```
## [1] 10766.19
```

```r
#median of total steps for each day
median(steps_taken_per_day$total_Steps_Per_Day)
```

```
## [1] 10765
```


So, the mean is 10766.19 steps and the median is 10765 steps.

## Task 2: What is the average daily activity pattern?

* Next, we will make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


Similar to previous step, we will first aggregate data and plot it.


```r
# Get total number of steps for each interval
agg_Steps_By_Intervals <- aggregate(cbind(activity_data$steps) ~ interval, data=activity_data, FUN=mean)
colnames(agg_Steps_By_Intervals) <- c("intervals", "total_Steps_By_Interval")

plot(agg_Steps_By_Intervals$intervals,agg_Steps_By_Intervals$total_Steps_By_Interval, type="l", 
     xlab="Interval", ylab="Number of Steps", col="dark blue",
     main="Average Number of Steps per Day by Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

Now, calculating the maximum interval: 

```r
#calculate interval that has the maximum number of steps using which.max function
agg_Steps_By_Intervals[which.max(agg_Steps_By_Intervals$total_Steps_By_Interval),1]
```

```
## [1] 835
```

So interval 835 contains maximum number of steps.

## Task 3: Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
* Create a new dataset that is equal to the original dataset but with the missing data filled in.
* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Total missing values calculation: 

```r
sum(is.na(activity_data$steps))
```

```
## [1] 2304
```

So, total 2304 NAs need to be removed and replaced.
These values will be replaced with the average value for that particular interval where the value is missing.


```r
#replace NAs with average for that interval
imputed_Data <- transform(activity_data, steps = ifelse(is.na(activity_data$steps),
                                              agg_Steps_By_Intervals$total_Steps_By_Interval[match(activity_data$interval, agg_Steps_By_Intervals$intervals)], 
                                               activity_data$steps))
```

And now we are aggregating the steps for the imputed dataset


```r
# Get total number of steps for each day from the imputed_Data
steps_taken_per_dayForImputed <- aggregate(cbind(imputed_Data$steps) ~ date, data=imputed_Data, FUN=sum)

#set proper column names
colnames(steps_taken_per_dayForImputed) <- c("dates", "total_Steps_Per_Day")
```

Now we can compare the histogram for the imputed v/s non-imputed data set.


```r
hist(steps_taken_per_dayForImputed$total_Steps_Per_Day, main = paste("Total Steps Each Day"), col="dark blue", xlab="Number of Steps")

#Create Histogram to show difference. 
hist(steps_taken_per_day$total_Steps_Per_Day,  col="green", add=T)

legend("topright", c("Imputed", "Non-imputed"), col=c("dark blue", "green"), lwd=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)

Now, we will calculate the mean and median based on the new data set.


```r
mean(steps_taken_per_dayForImputed$total_Steps_Per_Day)
```

```
## [1] 10766.19
```

```r
median(steps_taken_per_dayForImputed$total_Steps_Per_Day)
```

```
## [1] 10766.19
```

Imputed Data Mean = 10766.19 and Meadian = 10766.19

The median and mean are exactly same for the imputed data set.The reason behind this is because we replaced the NA's with average values.

The impact of imputing missing data on the estimates of the total daily number of steps is that 
we now have higher frquency counts in the histogram at the center region which is closer to the mean value.

## Task 4: Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

* Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

Here, we will identify each date as weekend or weekday and label that in the data set as a factor.


```r
# helper function to decide if a day is a week day or not
# pass date as parameter
is_Weekday <- function(day) {
  day <- weekdays(day)
  ifelse (day == "Saturday" | day == "Sunday", "Weekend", "Weekday")
}

imputed_Data$dayIndicator <- sapply(as.Date(imputed_Data$date), is_Weekday)

#explore the new data column
head(imputed_Data)
```

```
##       steps       date interval dayIndicator
## 1 1.7169811 2012-10-01        0      Weekday
## 2 0.3396226 2012-10-01        5      Weekday
## 3 0.1320755 2012-10-01       10      Weekday
## 4 0.1509434 2012-10-01       15      Weekday
## 5 0.0754717 2012-10-01       20      Weekday
## 6 2.0943396 2012-10-01       25      Weekday
```

Finally, we plot the aggregate steps using lattice library to identify the  activity patterns between weekdays and weekends.


```r
agg_week_Data <- aggregate(steps ~ dayIndicator+interval, data=imputed_Data, FUN=mean)

library(lattice)
xyplot(steps ~ interval | factor(dayIndicator),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=agg_week_Data)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)



