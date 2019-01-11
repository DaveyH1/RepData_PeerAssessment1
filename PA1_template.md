---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Set the global option so that ech ois always set to True - this will always display the code chunks.

```r
library(knitr)
opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data
Unzip the actvity.zip data file and load into "act"

```r
unzip("activity.zip")
act <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
Ignore missing values for this part.

First we need to aggregate the number of steps for each day (61 days)

```r
stepDate <- aggregate(act$steps, by = list(date = act$date), sum)
```

Now show a histogram of the number of steps for each day

```r
library(ggplot2)
ggplot(stepDate, aes(x  = x)) +
        geom_histogram() + 
        xlab("Steps") +
        labs(title = "Histogram of steps per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/histogram_plot-1.png)<!-- -->

Then we need to calculate the mean and median steps per day

```r
meanStep <- round(mean(stepDate$x, na.rm = T),3)
medianSteps <- median(stepDate$x, na.rm = T)
```

The mean number of daily steps is 10766.19 and the median number of dail steps is 10765

## What is the average daily activity pattern?
First create a data frame containing the acerage number of steps for each interval

```r
avgSteps <- aggregate(act$steps, by = list(interval = act$interval), mean, na.rm = T)
```

Now create a line plot which looks at the average number of steps per interval, average over all days

```r
ggplot(avgSteps, aes(x = interval, y = x, group = 1)) +
        geom_line() +
        xlab("Interval") +
        ylab("Average Steps") +
        labs(title = "Average number of steps per Interval")
```

![](PA1_template_files/figure-html/steps_interval_plot-1.png)<!-- -->

Which 5 minute interval, average across all days has the highest avgerage number of steps


```r
avgSteps[which(avgSteps$x == max(avgSteps$x)),]$interval
```

```
## [1] 835
```


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
