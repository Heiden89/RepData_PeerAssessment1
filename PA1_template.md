---
title: "Week2_projet_report"
author: "Heiden89"
date: "February 26, 2018"
output:
  word_document: default
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(ggplot2)
```

##Loading and preprocessing the data

####1 & 2- Loading of the data
```{r Data_download, echo = TRUE}

the_data <- read.csv("./activity.csv",header=TRUE)

```

For the moment, no transformation is done


##What is mean total number of steps taken per day?

####1 & 2
the following graph illustrate an histogram of the total number of steps per day
```{r, Prep_part_1, echo=TRUE}
grouped_data <- the_data %>% 
                group_by(date) %>% summarise(sum_steps = sum(steps,rm.na=TRUE))

g <- ggplot(data=grouped_data,aes(sum_steps))+
      geom_histogram(binwidth = 800)+ggtitle("Histogram of total steps for each dates")
    g

    
#Mean and Median
    
steps_summary <- summary(grouped_data$sum_steps)
print(steps_summary)
```

##What is the average daily activity pattern?
####1 & 2


``` {r, time_series, echo=TRUE}
by_int <- the_data%>% group_by(interval) %>% summarise(the_avr= mean(steps,na.rm=TRUE))



ggplot(by_int) + geom_line(mapping= aes(x=interval, y= the_avr)) +
      xlab("Period") + ylab("Steps per period")+theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))+
      ggtitle("Time series of average of steps per period")

the_max <- by_int%>% filter(by_int$the_avr==max(by_int$the_avr)) 

```

The maximum average steps is `r the_max$the_avr ` and is observable in the interval `r the_max$interval`
The 


##Imputing missing values
####1,2,3 & 4

```{r Part_1, echo=TRUE}
num_missing= nrow(the_data[is.na(the_data$steps)==TRUE,])

```

There is `r num_missing` period of 5 minutes that are Not Available in the data.frame

```{r Part_2 & 3, echo=TRUE}
the_filling= the_data %>% merge(by_int,by="interval") %>% mutate(with_filled= ifelse(!is.na(steps)==TRUE,steps,the_avr))

```

The strategy to fill the Na's was to use the same interval average for the period. The new dataset is called **the_filling**

```{r Part_4_histogram, echo=TRUE}

total_with_filling <- the_filling%>% group_by(date) %>% summarise(sum_with_filled = sum(with_filled))

ggplot(total_with_filling,aes(sum_with_filled)) + geom_histogram(binwidth = 800) +
      xlab("Period") + ylab("Steps per period")+theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))+
      ggtitle("Histogram of total steps per day with filling")

```

The difference in mean is `r mean(the_filling$with_filled) - mean(the_filling$steps,na.rm=TRUE)` wich is caused by the error handling in the mean function. There is no difference in the mean of the two distribution


The real difference resides in the standard deviation. the difference in sd is `r sd(the_filling$with_filled) - sd(the_filling$steps,na.rm=TRUE)` which is negative and caused by a more central distribution for the data that have been filled with the mean


##Are there differences in activity patterns between weekdays and weekends?

```{r Weekend & Weekday, echo=TRUE}
the_day <- factor("weekday","weekend")
the_filling$day <- weekdays(as.Date(the_filling$date))
the_filling <- the_filling %>% mutate(week_timing = factor(case_when(.$day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")~ "weekday",.$day %in% c("Saturday","Sunday")~"weekend")))

print(head(the_filling))
```



```{r Week plot, echo=TRUE}

by_int_filled <- the_filling%>% group_by(interval,week_timing) %>% summarise(the_avr_filled= mean(steps,na.rm=TRUE))


ggplot(by_int_filled) + geom_line(aes(x=interval,y=the_avr_filled))+
  facet_grid(week_timing ~ .)+
  xlab("Period") + ylab("average Steps per period")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))+ ggtitle("Time series of average of steps per period")

```


The previous Graphs shows the two pattern of average steps per period for the weekday and weekend. we can see that people tends to wake up later and stay active longer


knit2html("PA1_template.html")


