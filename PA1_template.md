---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

``` r
 library(readr)
 library(ggplot2)
 library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
 activity <- read_csv("activity.csv")
```

```
## Rows: 17568 Columns: 3
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```



## What is mean total number of steps taken per day?

``` r
# Summarize steps by day
 daily_steps <- activity %>%
     group_by(date) %>%
     summarize(total_steps = sum(steps, na.rm = TRUE))
 
 # Calculate mean and median
 mean_steps <- mean(daily_steps$total_steps, na.rm = TRUE)
 median_steps <- median(daily_steps$total_steps, na.rm = TRUE)
 
 # Print the results
 cat("Mean of daily steps: ", mean_steps, "\n")
```

```
## Mean of daily steps:  9354.23
```

``` r
 cat("Median of daily steps: ", median_steps, "\n")
```

```
## Median of daily steps:  10395
```

``` r
 # Plot histogram
 hist(daily_steps$total_steps, main = "Histogram of Daily Steps", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->



## What is the average daily activity pattern?

``` r
# Group the data by 'interval' and summarize the total steps
 interval_summary <- activity %>%
     group_by(interval) %>%
     summarize(total_steps = sum(steps, na.rm = TRUE))
 
 # Calculate mean and median of the total steps for each interval
 mean_steps_interval <- mean(interval_summary$total_steps, na.rm = TRUE)
 median_steps_interval <- median(interval_summary$total_steps, na.rm = TRUE)
 
 # Print the mean and median
 cat("Mean of total steps for each interval: ", mean_steps_interval, "\n")
```

```
## Mean of total steps for each interval:  1981.278
```

``` r
 cat("Median of total steps for each interval: ", median_steps_interval, "\n")
```

```
## Median of total steps for each interval:  1808
```

``` r
 # Plot histogram
 plot(interval_summary$interval, interval_summary$total_steps, main = "Plot of Total Steps by Interval", xlab = "Interval", ylab = "Total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## Imputing missing values

``` r
# Calculate and report the total number of missing values in the dataset 
 mean(is.na(activity$steps))
```

```
## [1] 0.1311475
```

``` r
 mean(is.na(activity$date))
```

```
## [1] 0
```

``` r
 mean(is.na(activity$interval))
```

```
## [1] 0
```

``` r
 # Calculate the mean steps for each interval
 mean_steps_by_interval <- activity %>%
     group_by(interval) %>%
     summarize(mean_steps = mean(steps, na.rm = TRUE))
 
 # Merge the original data with the mean steps by interval
 activity_imputed <- activity %>%
     left_join(mean_steps_by_interval, by = "interval")
 
 # Replace NA values in the 'steps' column with the corresponding mean steps
 activity_imputed$steps[is.na(activity_imputed$steps)] <- activity_imputed$mean_steps[is.na(activity_imputed$steps)]
 
 # Drop the temporary 'mean_steps' column
 activity_imputed <- activity_imputed %>% select(-mean_steps)
 
 # Show the first few rows of the imputed dataset
 head(activity_imputed)
```

```
## # A tibble: 6 × 3
##    steps date       interval
##    <dbl> <date>        <dbl>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
## 6 2.09   2012-10-01       25
```

``` r
 # Summarize steps by day
 daily_steps_c <- activity_imputed %>%
     group_by(date) %>%
     summarize(total_steps = sum(steps, na.rm = TRUE))
 
 # Calculate mean and median
 mean_steps <- mean(daily_steps_c$total_steps, na.rm = TRUE)
 median_steps <- median(daily_steps_c$total_steps, na.rm = TRUE)
 
 # Print the mean and median
 cat("Mean of daily steps: ", mean_steps, "\n")
```

```
## Mean of daily steps:  10766.19
```

``` r
 cat("Median of daily steps: ", median_steps, "\n")
```

```
## Median of daily steps:  10766.19
```

``` r
 # Plot histogram
 par(mfrow = c(1,2))
 hist(daily_steps_c$total_steps, main = "Histogram of Daily Steps(Computed)", xlab = "Total Steps", col = "lightblue", border = "black")
 hist(daily_steps$total_steps, main = "Histogram of Daily Steps", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?

``` r
# Create a new column 'day_type' indicating whether the date is a weekday or weekend
 activity_imputed$day_type <- ifelse(weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
 
 # Calculate the mean steps by 'interval' and 'day_type'
 mean_steps_by_day_type <- activity_imputed %>%
     group_by(interval,day_type) %>%
     summarize(mean_steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```

``` r
 # Create a panel plot using ggplot2
 ggplot(mean_steps_by_day_type, aes(x = interval, y = mean_steps)) +
     geom_line() +
     facet_wrap(~ day_type, nrow = 2) +
     labs(title = "Average Steps by Interval and Day Type",
          x = "Interval",
          y = "Average Steps") +
     theme_minimal()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

