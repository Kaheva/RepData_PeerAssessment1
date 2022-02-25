---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

### Load tidyverse and data.table packages


```r
library(tidyverse)
library(data.table)
```

## Loading and preprocessing the data

**Load the data in R. Environment**


```r
unzip("./activity.zip")
Activity_Data <- read.csv("./activity.csv")
```


**Convert dataframe to a Tibble**

```r
Activity_Data <- as_tibble(Activity_Data)
```



**Initial data exploration**


```r
summary(Activity_Data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?

**Calculate the total number of steps taken per day**

```r
Total_Steps_per_Day <- aggregate(steps ~ date, Activity_Data, sum, na.rm=TRUE)
```

**Make a histogram of the total number of steps taken each day**

```r
ggplot(Total_Steps_per_Day, aes(steps)) +
  geom_histogram(color ="#000000", fill="#69b3a2", binwidth = 800) +
  labs(x = "Number of Daily Steps", y = "Frequency", title = "Total Number of Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


**Calculate and report the mean and median of the total number of steps taken per day**

Mean:

```r
mean(Total_Steps_per_Day$steps)
```

```
## [1] 10766.19
```

Median:

```r
median(Total_Steps_per_Day$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

**Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days**


```r
Average_Steps_per_Day <- aggregate(Activity_Data$steps, by = list(Activity_Data$interval), FUN = mean, na.rm = TRUE)
```

```r
names(Average_Steps_per_Day) <- c("Interval", "Mean")
```

```r
ggplot(Average_Steps_per_Day, aes(x = Interval, y = Mean)) +
  geom_line(color = "#69b3a2", size = 0.8) +
  labs(x = "5mn Intervals", y = "Average Nb of Steps", title = "Average Nb of Steps per Interval Across all Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

**Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

```r
Average_Steps_per_Day[which.max(Average_Steps_per_Day$Mean), ]$Interval
```

```
## [1] 835
```


## Imputing missing values

**Calculate and report the total number of missing values in the dataset**

```r
sum(is.na(Activity_Data$steps))
```

```
## [1] 2304
```

**Devise a strategy for filling in all of the missing values in the dataset**
We will fill the missing data with the mean for the same interval. Let's define a function to do so

```r
NoNAs <- Average_Steps_per_Day$Mean[match(Activity_Data$interval, Average_Steps_per_Day$Interval)]
```


**Create a new dataset that is equal to the original dataset but with the missing data filled in**

Let's first create a new general dataset with NAs filled with "mean per interval"

```r
Activity_Data_Filled <- transform(Activity_Data, steps = ifelse(is.na(Activity_Data$steps), yes = NoNAs, no = Activity_Data$steps))
```

Now create a new Total_Steps dataframe with NAs filled with "mean per interval"

```r
Total_Steps_per_Day_Filled <- aggregate(steps ~ date, Activity_Data_Filled, sum)
```

**Make a histogram of the total number of steps taken each day**

```r
ggplot(Total_Steps_per_Day_Filled, aes(steps)) +
  geom_histogram(color ="#000000", fill="#69b3a2", binwidth = 800) +
  labs(x = "Number of Daily Steps", y = "Frequency", title = "Total Number of Daily Steps No NAs")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
**Calculate and report the mean and median total number of steps taken per day**
mean:

```r
mean(Total_Steps_per_Day_Filled$steps)
```

```
## [1] 10766.19
```
Median:

```r
median(Total_Steps_per_Day_Filled$steps)
```

```
## [1] 10766.19
```

**Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**
=> No difference between the means (both 10,766.5) but a slight difference between medians (10,765 with NAs and 10,766.19 without NAs)

## Are there differences in activity patterns between weekdays and weekends?

**Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day**


```r
Activity_Data_Dates <- mutate(Activity_Data, Day_of_Week = weekdays(as.POSIXct(date, format = "%Y-%m-%d")))
```


```r
Activity_Data_Dates <- Activity_Data_Dates %>%
  mutate ("weekday/weekend" =
        ifelse(Day_of_Week %in% c("lundi", "mardi", "mercredi", "jeudi", "vendredi"), "weekday",
        ifelse( Day_of_Week %in% c("samedi", "dimanche"), "weekend", no = FALSE)))
Activity_Data_Dates$`weekday/weekend` <- as.factor(Activity_Data_Dates$`weekday/weekend`) 
```





```r
NoNAs2 <- filter(Activity_Data_Dates, (!is.na(steps & steps==median(steps)))) %>%
                    head(n=1) %>%
                    select(steps) %>%
                    as.numeric()
```

```r
Activity_Data_Dates <- mutate(Activity_Data_Dates, steps = tidyr ::replace_na(steps, NoNAs2))
```

```r
Tidy_Activity_Data_Dates <- group_by(Activity_Data_Dates, interval, `weekday/weekend` ) %>%
  summarize(mean = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```


**Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days**



```r
ggplot(Tidy_Activity_Data_Dates, aes(x = interval, y = mean, color = `weekday/weekend`)) +
  geom_line(size = 0.7) +
  labs(x = "5mn Intervals", y = "Average Nb of Steps", title = "Average Nb of Steps per Interval per WeekDay Type") +
  facet_grid(rows = vars(`weekday/weekend`)) +
  theme(legend.position = "none") 
```

![](PA1_template_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

**Are there differences in activity patterns between weekdays and weekends?**
Not surprisingly, more activity is recorded early morning in the week days than during week-ends. Activity trend seems to decrease and stabilize after the early morning peak during week days while , during the week-ends, activity trend looks more regular while starting a bit later and ending a bit later than during week days. 
