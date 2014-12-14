# Peer Assesment 1 (Reproducible Research)
Caroline Sielfeld  
14 de diciembre de 2014  
##Loading and preprocessing the data

First, we load the data, and we transform the dates to "date-form":

```r
data <- read.csv("activity.csv")
summary(data)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```

```r
data$date <- as.Date(data$date)
```

##What is mean total number of steps taken per day?

Now, we group the data corresponding to each day, so we can make a histogram of the steps taken per day and get the mean and the median:


```r
stepsday <- aggregate(steps ~ date, data = data, FUN = sum)
hist(stepsday$steps, breaks = 20, xlab="Steps per day", ylab="Frequency", main="Histogram of steps per day")
```

![plot of chunk unnamed-chunk-2](./Peerass_1CS_files/figure-html/unnamed-chunk-2.png) 

```r
mean(stepsday$steps)
```

```
## [1] 10766
```

```r
median(stepsday$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

To get the daily average activity pattern, we group the information that corresponds to each interval. Then, we plot the mean of steps taken each interval:


```r
stepsinter <- aggregate(steps ~ interval, data = data, FUN = mean)
plot(stepsinter, type = "l", main="Average daily activity pattern")
```

![plot of chunk unnamed-chunk-3](./Peerass_1CS_files/figure-html/unnamed-chunk-3.png) 

Now, we get the maximum of steps taken in one interval:


```r
which.max(stepsinter$steps)
```

```
## [1] 104
```

```r
stepsinter[which.max(stepsinter$steps),]
```

```
##     interval steps
## 104      835 206.2
```

##Imputing missing values

First, we calculate the numbers of "NA's" in the dataset:


```r
sum(is.na(data))
```

```
## [1] 2304
```

Then, we use the following strategy to fill the NA's: we merge the original data and the data that contains the means for each interval (stepsinter). Then, if a row of the original data has an NA, that value gets the value of the mean of that interval. Finally, we "cut" the column again to get the original data but with the NA`s filled in:


```r
data <- merge(data, stepsinter, by = "interval", suffixes = c("",".y"))
nas <- is.na(data$steps)
data$steps[nas] <- data$steps.y[nas]
data <- data[, c(1:3)]
```

Now, we make a new histogram for the average steps taken per day, and we get again the mean and the median:


```r
stepsday2 <- aggregate(steps ~ date, data = data, FUN = sum)
hist(stepsday2$steps, breaks = 20, xlab="Steps per day", ylab="Frequency", main="Histogram of steps per day")
```

![plot of chunk unnamed-chunk-7](./Peerass_1CS_files/figure-html/unnamed-chunk-7.png) 

```r
mean(stepsday2$steps)
```

```
## [1] 10766
```

```r
median(stepsday2$steps)
```

```
## [1] 10766
```

##Are there differences in activity patterns between weekdays and weekends?

Finally, we see if there are differences between the steps taken in weekends and weekdays. For this, we make a function that defines a new factor for each row, which indicates if the day is part of the weekend or week. 

```r
day <- function(date) {
  if (weekdays(date) %in% c("Saturday", "Sunday")) {
    "weekend"
  } 
  else {
    "weekday"
  }
}

data$daytype <- as.factor(sapply(data$date, day))
```

We make then a plot that contains the average steps for each interval, first for the weekdays, then for the weekends:


```r
par(mfrow = c(2, 1))
plotwday <- aggregate(steps ~ interval, data = data, subset = data$daytype =="weekday" , FUN = mean)
plot(plotwday, type = "l", main = "Weekday")

plotwend <- aggregate(steps ~ interval, data = data, subset = data$daytype =="weekend" , FUN = mean)
plot(plotwend, type = "l", main = "Weekend")
```

![plot of chunk unnamed-chunk-9](./Peerass_1CS_files/figure-html/unnamed-chunk-9.png) 

