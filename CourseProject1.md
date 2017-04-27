---
title: "Class 5 Week 2 Course Project 1"
author: "Berthold Jaeck"
date: "4/26/2017"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache=TRUE, fig.width =6, fig.height=4)
```

## General remarks

This document will provide a simple data analysis and presentation to demonstrate the application and the potential of the R-markdown file concept. To this end, we will work with the dplyr and ggplot2 libraries.

```{r libs, echo=FALSE, include=FALSE}
library(dplyr)
library(ggplot2)
```

## Loading data

The motion tracking data is loaded from "activity.cvs" found in the repository and stored in the dataframe format of the dplyr package.

```{r load}
df<-tbl_df(read.csv("activity.csv", header=TRUE))
```

## Exploratory data analysis

This part of the course project addresses the problems by means of simple statistical analysis' of the data set and related figures.

### What is total number of steps taken per day?

First we'll calculate the total number of steps per day:
```{r q1grouping, echo=TRUE}
df %>% group_by(date) %>% summarise(total=sum(steps)) ->ts
```

A histogram appears helpful to understand the distribution of steps per day:
```{r q1plot, echo=TRUE}
g<-ggplot(ts, aes(total*1e-3))+geom_histogram(binwidth=1, na.rm=TRUE, col="steelblue4", fill="steelblue1")+labs(title="Total steps per day")+labs(x="Number of steps (Thousands)")+
    labs(y="Count")
print(g)
```

From the histogram we see that the center of the distribution is at about 10 to 15 thousand steps per day.

Let's compare this *optical impression* with some statistics and calculate the mean and median value of steps taken per day. We find a mean and median value of `r 1e-3*mean(ts$total, na.rm=TRUE)` and `r 1e-3*median(ts$total, na.rm=TRUE)` thousand steps per day, respectively.

As expected the histogram already gave us a good idea about the data.

### What is the average daily activity pattern?

To understand the daywise distribution of the user activity, let us have a look at the number of steps as a function of the time interval. Let's first calculate the mean value of the daily activity for each interval:

```{r q2grouping, echo=TRUE}
df %>% group_by(interval) %>% summarise(ave=mean(steps, na.rm=TRUE)) ->ms
ms$interval<-as.numeric(ms$interval)
```

To get a better understanding of the data structure, we'll plot the average step number across all days as a function of the time intervall.
```{r q2plot, echo=TRUE}
g1<-ggplot(ms, aes(interval, ave))+geom_point(col="steelblue")+labs(title="Activity Chart")+labs(x="Time (minutes)")+
    labs(y="Step Count")
print(g1)
```

From the plot, we can identify times of no activity - likely at night time -  times of elevated acitivities, as well as a sharp peak in activity. From the peak we can locate the activity maximum in the interval at `r ms$interval[which.max(ms$ave)]` mins.

### Imputing missing values

From the written code, it can be seen that we had to exclude missing values. To obtain a larger data set and, thus get better statistics it would be nice to impute the missing values. Before doing that, let's see how many data points are missing.

```{r q3isna, echo=TRUE}
mv.s<-sum(is.na(df$steps))
mv.i<-sum(is.na(df$interval))
mv.d<-sum(is.na(df$date))
print (c(mv.s, mv.i, mv.d))
```

While we have `r mv.s` missing values in the step variable, we find `r mv.i` missing values in the interval and `r mv.d` missing values in date variable.

What is now the right strategy to impute the missing values? As we have seen from the previous plot, the average number of steps varies a lot from interval to interval. We'll impute the missing values by taking the average step value of the respective interval we have calculated previously. This approach should yield reasonable outcome, given the activity patterns don't change to much from one day to the other.

```{r q3imputing, echo=TRUE}
df1<-df 
for(i in 1:length(df1$steps)){
    if(is.na(df1$steps[i])){
        df1$steps[i]<-ms$ave[ms$interval==df1$interval[i]]
    }
}
```

From the new data set containing the imputed data, we can now calculate the total number of steps per day:
```{r q3grouping, echo=TRUE}
df1 %>% group_by(date) %>% summarise(total=sum(steps)) ->ts1
```

Let's look at a histogram to see how the distribution has changed from the non-imputed data
```{r q3plot, echo=TRUE}
g2<-ggplot(ts1, aes(total*1e-3))+geom_histogram(binwidth=1, na.rm=TRUE, col="steelblue4", fill="steelblue1")+labs(title="Total steps per day - imputed data")+labs(x="Number of steps (Thousands)")+
    labs(y="Count")
print(g2)
```

Compared to the first plot, we see that the imputed data exhibit a different distribution.

Let's have lithmus test of this *optical impression* and calculate the mean and median value of steps taken per day for this new data set. We find a mean and median value of `r 1e-3*mean(ts1$total)` and `r 1e-3*median(ts1$total, na.rm=TRUE)` thousand steps per day, respectively. We directly see that imputing the missing values did not change the statistics of the data up to the second decimal digit. This is very good news, as we now have a larger data set to work with. Concretely we now can work with `r length(df1$steps)` observations compared to `r (length(df1$steps)-sum(is.na(df)))` observations, when not imputing missing data.

### Are there differences in activity patterns between weekdays and weekends?

In the last part of our explanatory data analysis we are interested in whether or not activity on weekends is different from weekdays. To solve this question, let us first create a character variable from the date variable that contains the weekday. For the following, we'll keep on working with the imputed dataset *df1*.

```{r q4weekdays, echo=TRUE}
df1$date<-as.POSIXct(df1$date, format="%Y-%m-%d")
df1$dt<-weekdays(df1$date)
print(head(df1$dt))
```

From this new variable, we now create a factor variable that indicates whether a day corresponds to a weekend or not.

```{r q4factor, echo=TRUE}
df1$fc<-as.factor(is.element(df1$dt, c("Saturday" , "Sunday")))
print(head(df1$fc))
```

To see the difference between weekends and weekdays, we now calculate the average number of steps per interval across all days in the same fashion as before for question 2 on the data set grouped by weekend/weekday:

```{r q4means, echo=TRUE}
df1 %>% filter(df1$fc=="TRUE") %>% group_by(interval) %>% summarise(ave=mean(steps), fc=unique(fc)) -> ms1.true
df1 %>% filter(df1$fc=="FALSE") %>% group_by(interval) %>% summarise(ave=mean(steps), fc=unique(fc)) -> ms1.false
ms1<-bind_rows(ms1.true, ms1.false)
ms1$interval<-as.numeric(ms1$interval)
print(head(ms1))
```

Finally, we can plot the steps averaged across the day as a function of the time interval and make use of the weekend/weekeday factor variable *df1$fc*:

```{r q4plot, echo=TRUE}
g3<-ggplot(ms1, aes(interval, ave))+geom_point(col="steelblue")+facet_wrap(~fc, ncol=1, nrow=2)+labs(title="Activity Chart split ")+labs(x="Time (minutes)")+
    labs(y="Step Count")
print(g3)
```

As we can see from plot, the activity pattern on weekdays, "FALSE", is quantitatively and qualitatively different from weekends, "TRUE". On weekdays, activity is higher with more steps walked. Also, the weekday activity exhibits a temporal pattern with well separated activity peaks, while weekend activity is more uniform.