<br><center>**Course Project 1**</center>
<br> <br>
1.) Reading in dataset.


```r
#setwd("./data")
library(plyr)
library(graphics)
library(lattice)
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

<br/>
2.) Histogram of the total number of steps taken each day.


```r
stepsPerDay <- aggregate(activity$steps, by=list(date=activity$date), sum, na.rm=TRUE)
```

```
## Error in aggregate(activity$steps, by = list(date = activity$date), sum, : object 'activity' not found
```

```r
hist(stepsPerDay$x, breaks=30, main = "Total steps per day", xlab = "Total steps")
```

```
## Error in hist(stepsPerDay$x, breaks = 30, main = "Total steps per day", : object 'stepsPerDay' not found
```

<br/>
3.) Mean steps/day: 9354.23; median steps/day: 10395


```r
mean(stepsPerDay$x)
```

```
## Error in mean(stepsPerDay$x): object 'stepsPerDay' not found
```

```r
median(stepsPerDay$x)
```

```
## Error in median(stepsPerDay$x): object 'stepsPerDay' not found
```

<br/>
4.) Time series plot of the average number of steps taken


```r
avgInterval <- aggregate(activity$steps, by=list(int=activity$interval), mean, na.rm=TRUE)
```

```
## Error in aggregate(activity$steps, by = list(int = activity$interval), : object 'activity' not found
```

```r
plot(avgInterval$int, avgInterval$x, type="l", main = "Avg. steps in each five-minute interval", xlab = "Interval", ylab = "Avg. steps")
```

```
## Error in plot(avgInterval$int, avgInterval$x, type = "l", main = "Avg. steps in each five-minute interval", : object 'avgInterval' not found
```

<br/>
5.) The interval from 0835-0840, on average, contains the maximum number of steps.


```r
avgInterval$int[avgInterval$x == max(avgInterval$x)]
```

```
## Error in eval(expr, envir, enclos): object 'avgInterval' not found
```

<br/>
6.) Code to describe and show a strategy for imputing missing data

This code uses the mean number of steps for each interval (from avgInterval$x) as a basis for imputing missing values.


```r
sum(is.na(activity$steps))
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
avgStpsByInt <- rep(avgInterval$x, 61)
```

```
## Error in eval(expr, envir, enclos): object 'avgInterval' not found
```

```r
newSteps <- c()
for (i in seq(length(activity$steps))) {
  if (is.na(activity$steps[i])) {
    newSteps <- append(newSteps, avgStpsByInt[i])
  } else {
    newSteps <- append(newSteps, activity$steps[i])
  }
}
```

```
## Error in seq(length(activity$steps)): object 'activity' not found
```

```r
newActivity <- data.frame(newSteps, date=activity$date, interval=activity$interval)
```

```
## Error in data.frame(newSteps, date = activity$date, interval = activity$interval): object 'activity' not found
```

<br/>
7.) Histogram of the total number of steps taken each day after missing values are imputed


```r
newStepsPerDay <- aggregate(newSteps, by=list(date=activity$date), sum)
```

```
## Error in aggregate.data.frame(as.data.frame(x), ...): no rows to aggregate
```

```r
hist(newStepsPerDay$x, breaks = 30, main = "Total steps per day w/ imputed values", xlab = "Total steps")
```

```
## Error in hist(newStepsPerDay$x, breaks = 30, main = "Total steps per day w/ imputed values", : object 'newStepsPerDay' not found
```


```r
mean(newStepsPerDay$x)
```

```
## Error in mean(newStepsPerDay$x): object 'newStepsPerDay' not found
```

```r
median(newStepsPerDay$x)
```

```
## Error in median(newStepsPerDay$x): object 'newStepsPerDay' not found
```
Mean: 10766.19
Median: 10766.19

Both values are higher than the original estimates. Imputing missing data seems to raise estimates and bring the mean and median closer together.


<br/>
8.) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
days <- weekdays(as.Date(activity$date))
```

```
## Error in as.Date(activity$date): object 'activity' not found
```

```r
isWknd <- (days == "Saturday") | (days == "Sunday")
```

```
## Error in eval(expr, envir, enclos): object 'days' not found
```

```r
dayFactor <- factor(isWknd, labels = c("weekday", "weekend"))
```

```
## Error in factor(isWknd, labels = c("weekday", "weekend")): object 'isWknd' not found
```

```r
newActivity$day.f <- dayFactor
```

```
## Error in eval(expr, envir, enclos): object 'dayFactor' not found
```

```r
avgStps <- aggregate(newActivity$newSteps, by = list(int = newActivity$interval, wknd = newActivity$day.f), FUN = mean)
```

```
## Error in aggregate(newActivity$newSteps, by = list(int = newActivity$interval, : object 'newActivity' not found
```

```r
p <- xyplot(avgStps$x ~ avgStps$int | avgStps$wknd, layout = c(1, 2), type = "l", main = "Avg. steps by interval", xlab = "Interval", ylab = "Avg. steps")
```

```
## Error in eval(expr, envir, enclos): object 'avgStps' not found
```

```r
print(p)
```

```
## Error in print(p): object 'p' not found
```
