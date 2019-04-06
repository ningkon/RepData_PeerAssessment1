---
title: "Reproducible Research Course Project 1"
author: "Rajkumar Ningkonsin"
date: "April 4, 2019"
output: 
  html_document :
    keep_md: true
---
  
  



# Loading and preprocessing the data

### Load the Activity Monitor data into a dataframe and exclude NA values


```r
activity <- read.csv("~/Coursera/data/activity.csv")
activity_sub <- na.exclude(activity)
```
### Mean of number of steps per day

```r
num_steps <- aggregate(activity_sub$steps, by = list(date = activity_sub$date), FUN = sum )
```
              
                     
### Histogram of the total number of steps taken each day

```r
library(ggplot2)

ggplot(num_steps, aes(x = num_steps$date, y = num_steps$x)) + 
  geom_bar(stat = "identity") + xlab("Date") + ylab("Number of steps") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 70, hjust = 1))
```

![plot of chunk plot1](figure/plot1-1.png)


### Summary of total number of steps taken each day 

```r
steps_summary <- summary(num_steps$x)

steps_summary
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

### Mean of steps taken each day

```r
steps_summary["Mean"]
```

```
##     Mean 
## 10766.19
```

### Median of steps taken each day

```r
steps_summary["Median"]
```

```
## Median 
##  10765
```


# What is the average daily activity pattern?

### Time series plot of 5-minute interval and average number of steps taken, averaged across all days 


```r
ave_steps <- aggregate(activity_sub$steps, by = list(interval = activity_sub$interval), FUN = mean )

plot(ave_steps$interval, ave_steps$x, type = "l", ylab = "Number of steps", xlab = "5 min interval")
```

![plot of chunk plot2](figure/plot2-1.png)

### 5-minute interval with maximum number of steps


```r
subset(ave_steps$interval, ave_steps$x == max(ave_steps$x))
```

```
## [1] 835
```


# Inputing missing values

### Total number of NAs in the dataset

```r
table(is.na(activity$steps))["TRUE"]
```

```
## TRUE 
## 2304
```

### Filling in all of the missing values in the dataset with correspnding mean

```r
activity_na <- activity

for (i in seq_along(activity_na$steps)) {
  if(is.na(activity_na$steps)[i] == TRUE){
    activity_na$steps[i] <- subset(ave_steps$x, ave_steps$interval == activity_na$interval[i])
  }
  else activity_na$steps[i] <- activity_na$steps[i]
}
```
### New dataset equal to the original dataset but with missing data filled in (sample 5 records)

```r
head(activity_na,5)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
```

### Histogram of total number of steps taken each day with filled-in data

```r
num_steps_na <- aggregate(activity_na$steps, by = list(date = activity_na$date), FUN = sum )

ggplot(num_steps_na, aes(x = num_steps_na$date, y = num_steps_na$x)) + 
  geom_bar(stat = "identity") + 
  xlab("Date") + ylab("Steps") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 70, hjust = 1))
```

![plot of chunk plot3](figure/plot3-1.png)

### Summary of total number of steps taken each day for filled-in data

```r
steps_summary_na <- summary(num_steps_na$x)

steps_summary_na
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

### Mean of steps taken each day for filled-in data

```r
steps_summary_na["Mean"]
```

```
##     Mean 
## 10766.19
```

### Median of steps taken each day for filled-in data

```r
steps_summary_na["Median"]
```

```
##   Median 
## 10766.19
```

# Are there differences in activity patterns between weekdays and weekends?

### Dataset with two levels "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```r
activity_na$date <- as.Date(activity_na$date)

activity_na$day <- weekdays(activity_na$date)

for (i in seq_along(activity_na$day)) {
  if(activity_na$day[i] %in% c("Saturday","Sunday")){
    activity_na$weekend[i] <- "weekend"
  }
  else activity_na$weekend[i] <- "weekday"
}
```

### Dataset with two levels "weekday" and "weekend" (sample 5 records)

```r
head(activity_na,5)
```

```
##       steps       date interval    day weekend
## 1 1.7169811 2012-10-01        0 Monday weekday
## 2 0.3396226 2012-10-01        5 Monday weekday
## 3 0.1320755 2012-10-01       10 Monday weekday
## 4 0.1509434 2012-10-01       15 Monday weekday
## 5 0.0754717 2012-10-01       20 Monday weekday
```

### Dataset of average steps average across all days by interval and weekend




```r
ave_steps_days <- activity_na %>% group_by(interval, weekend) %>% summarise_at(vars(steps), mean)
```


### Dataset of average steps average across all days by interval and weekend (sample 6 records)

```r
head(ave_steps_days,6)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [3]
##   interval weekend  steps
##      <int> <chr>    <dbl>
## 1        0 weekday 2.25  
## 2        0 weekend 0.215 
## 3        5 weekday 0.445 
## 4        5 weekend 0.0425
## 5       10 weekday 0.173 
## 6       10 weekend 0.0165
```

### Panel plot containing a time series plots of 5-minute interval and average number of steps taken, averaged across all weekday days or weekend days.


```r
library(lattice)

xyplot(steps~interval | factor(weekend), data = ave_steps_days, main="Steps vs Interval", xlab="5 min Interval",  ylab="Number of Steps",layout=c(1,2),type="l")
```

![plot of chunk plot4](figure/plot4-1.png)






