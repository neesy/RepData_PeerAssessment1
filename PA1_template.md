---
title: "PA1_template"
author: "Denise Olson"
date: "April 21, 2016"
output: html_document
---

This is an R Markdown document for Project 1 - Reproducible Research. 
When reviewing my code, please look at PA1_template.html which contains the graphs and code. This .md file will only contain code.

Chunk 1: contains the code to load the appropriate libraries 
```{r, echo=TRUE}
## set the environment and libraries necessary
library(dplyr)
library(ggplot2)
library(chron)
setwd("C:/Users/Denise/Documents/R/Coursera")
```

Chunk 2: Loads the activity data
```{r, echo=TRUE}
## Step 1 read data
if (!exists("activity")) {
  tf <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",tf)
  activity <- read.csv(unz(tf,"activity.csv"))
  unlink(tf)
}
```
Chunk 3: Creates a histogram of the raw data for total daily steps. The daily steps are totaled by using the summarize function and grouping by date. 
```{r, echo=TRUE}
## Step 2 Create histogram of raw data for total steps
activity$interval <- factor(activity$interval)
activity$date <- as.Date(activity$date)
step_total <- summarize(group_by(activity, date), stepsum=sum(steps))
par(mfrow=c(1,1))
hist(step_total$stepsum, xlab="Total Steps", main="Histogram: Total Steps", col="blue")
dev.copy(png, file="plot1c1.png", width = 840, height = 480)
dev.off()
```
Chunk 4: This section shows the mean and median of the data.
```{r, echo=TRUE}
## Step 3 - print mean and median of total steps by date
step_mean <- round(mean(step_total$stepsum, na.rm=TRUE),2)
step_med <- median(step_total$stepsum, na.rm=TRUE)
print(c("mean=",step_mean), quote=FALSE)
print(c("median=",step_med), quote=FALSE)
```

Chunk 5: This section summarizes data by time interval and creates a time series plot by interval showing the average steps by interval. 
```{r, echo=TRUE}
## Step 4 - create time series of mean by 5 minute interval
sub_activity <- activity[complete.cases(activity), ]
interval_step <- aggregate(steps ~ interval, data=sub_activity, mean)
plot.ts(interval_step$steps, xlab="5 min intervals",ylab="Avg Steps",main="Time Series: Avg Steps by Interval", col="red")
dev.copy(png,file="plot2c1.png", width = 840, height = 480)
dev.off()
```
Chunk 6: This section calcualtes the interval time with the maximum # of steps
```{r, echo=TRUE}
## Step 5 - calculate with maximum mean value for steps
max_interval <- interval_step[interval_step$steps==max(interval_step$steps),]
max_interval[1]
```

Chunk 7: This section of the code identifies the missing records and replaces these with the average for that time interval.
```{r, echo=TRUE}
## Step 6 - Impute missing data (used average for the interval)
na_activity <- activity[!complete.cases(activity),]
mrg_data <- merge(na_activity, interval_step, by.x="interval", by.y="interval")
mrg_data <- mrg_data[, c("steps.y","date","interval")]
names(mrg_data) <- c("steps","date","interval")
mrg_data <- as.data.frame(mrg_data)
na_vect <- rep("imputed",nrow(mrg_data))
mrg_data <- cbind(na_vect,mrg_data)

na_vect <- rep("actual",nrow(sub_activity))
sub_activity <- cbind(na_vect,sub_activity)

comb_activity <- rbind(sub_activity,mrg_data)
```

Chunk 8: This section of code creates a histogram of the data after adding back the imputed steps in place of the missing values.
```{r, echo=TRUE}
## Step 7 - Histogram of # of steps taken each day after missing values are imputed
step_comb <- summarize(group_by(comb_activity, date), stepsum=sum(steps))
hist(step_comb$stepsum, xlab="Total Steps", main="Histogram: Total Steps (w/imputed)", col="blue")
dev.copy(png, file="plot3c1.png", width = 840, height = 480)
dev.off()
```

Chunk 9: This section creates a panel plot of steps taken by weekend and weekday groups. By comparing the results of the 2 plots we can see how the two sets compare.
```{r, echo=TRUE, fig.height=6}
## Step 8 - Panel plot comparing the average # of steps taken per by interval on weekends and weekdays
## using imputed values for missing
wkend <- ifelse(is.weekend(comb_activity$date),"weekend","weekday")
comb_activity <- cbind(wkend,comb_activity)
interval_comb <- comb_activity %>% group_by(interval,wkend) %>% summarize_each(funs(mean),steps)
weekend <- interval_comb[interval_comb$wkend=="weekend",]
weekday <- interval_comb[interval_comb$wkend=="weekday",]
par(mfrow=c(2,1))
plot.ts(weekend$steps, xlab="5 min intervals",ylab="Avg Steps",main="WEEKEND Time Series: Avg Steps by Interval", col="red")
plot.ts(weekday$steps, xlab="5 min intervals",ylab="Avg Steps",main="WEEKDAY Time Series: Avg Steps by Interval", col="blue")
dev.copy(png, file ="plot4c1.png", width = 840, height = 480)
dev.off()
```
