---
title: "Week2_projet_report"
author: "Heiden89"
date: "February 26, 2018"
output:
  html_document: default
  pdf_document: default
  word_document: default
---


```r
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(ggplot2)
```

##Loading and preprocessing the data

####1 & 2- Loading of the data

```r
the_data <- read.csv("./activity.csv",header=TRUE)
```

For the moment, no transformation is done


##What is mean total number of steps taken per day?

####1 & 2
the following graph illustrate an histogram of the total number of steps per day

```r
grouped_data <- the_data %>% 
                group_by(date) %>% summarise(sum_steps = sum(steps,rm.na=TRUE))

g <- ggplot(data=grouped_data,aes(sum_steps))+
      geom_histogram(binwidth = 800)+ggtitle("Histogram of total steps for each dates")
    g
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk Prep_part_1](figure/Prep_part_1-1.png)

```r
#Mean and Median
    
steps_summary <- summary(grouped_data$sum_steps)
print(steps_summary)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      42    8842   10766   10767   13295   21195       8
```

##What is the average daily activity pattern?
####1 & 2



```r
by_int <- the_data%>% group_by(interval) %>% summarise(the_avr= mean(steps,na.rm=TRUE))



ggplot(by_int) + geom_line(mapping= aes(x=interval, y= the_avr)) +
      xlab("Period") + ylab("Steps per period")+theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))+
      ggtitle("Time series of average of steps per period")
```

![plot of chunk time_series](figure/time_series-1.png)

```r
the_max <- by_int%>% filter(by_int$the_avr==max(by_int$the_avr)) 
```

The maximum average steps is 206.1698113 and is observable in the interval 835
The 


##Imputing missing values
####1,2,3 & 4


```r
num_missing= nrow(the_data[is.na(the_data$steps)==TRUE,])
```

There is 2304 period of 5 minutes that are Not Available in the data.frame


```r
the_filling= the_data %>% merge(by_int,by="interval") %>% mutate(with_filled= ifelse(!is.na(steps)==TRUE,steps,the_avr))
```

The strategy to fill the Na's was to use the same interval average for the period. The new dataset is called **the_filling**


```r
total_with_filling <- the_filling%>% group_by(date) %>% summarise(sum_with_filled = sum(with_filled))

ggplot(total_with_filling,aes(sum_with_filled)) + geom_histogram(binwidth = 800) +
      xlab("Period") + ylab("Steps per period")+theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))+
      ggtitle("Histogram of total steps per day with filling")
```

![plot of chunk Part_4_histogram](figure/Part_4_histogram-1.png)

The difference in mean is 0 wich is caused by the error handling in the mean function. There is no difference in the mean of the two distribution


The real difference resides in the standard deviation. the difference in sd is -6.6707538 which is negative and caused by a more central distribution for the data that have been filled with the mean


##Are there differences in activity patterns between weekdays and weekends?


```r
the_day <- factor("weekday","weekend")
the_filling$day <- weekdays(as.Date(the_filling$date))
the_filling <- the_filling %>% mutate(week_timing = factor(case_when(.$day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")~ "weekday",.$day %in% c("Saturday","Sunday")~"weekend")))

print(head(the_filling))
```

```
##   interval steps       date  the_avr with_filled      day week_timing
## 1        0    NA 2012-10-01 1.716981    1.716981   Monday     weekday
## 2        0     0 2012-11-23 1.716981    0.000000   Friday     weekday
## 3        0     0 2012-10-28 1.716981    0.000000   Sunday     weekend
## 4        0     0 2012-11-06 1.716981    0.000000  Tuesday     weekday
## 5        0     0 2012-11-24 1.716981    0.000000 Saturday     weekend
## 6        0     0 2012-11-15 1.716981    0.000000 Thursday     weekday
```




```r
by_int_filled <- the_filling%>% group_by(interval,week_timing) %>% summarise(the_avr_filled= mean(steps,na.rm=TRUE))


ggplot(by_int_filled) + geom_line(aes(x=interval,y=the_avr_filled))+
  facet_grid(week_timing ~ .)+
  xlab("Period") + ylab("average Steps per period")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))+ ggtitle("Time series of average of steps per period")
```

![plot of chunk Week plot](figure/Week plot-1.png)


The previous Graphs shows the two pattern of average steps per period for the weekday and weekend. we can see that people tends to wake up later and stay active longer


knit2html("PA1_template.html")


