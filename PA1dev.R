# Repro Research Project 1 code development

## Load the data

### Load default libraries
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(httr)
library(XML)
library(quantmod)
library(pastecs)
library(psych)

unzip("activity.zip")
data = read.csv("activity.csv", stringsAsFactors=FALSE, na.strings = "NA")
data = tbl_df(data)
data$steps = as.numeric(data$steps)

## What is mean total number of steps taken per day?

totalStepsPerDay = data %>%
        group_by(date) %>%
        summarize(Steps_per_day=sum(steps, na.rm = FALSE))

meanStepsPerDay = mean(totalStepsPerDay$Steps_per_day, na.rm=T)

medianStepsPerDay = median(totalStepsPerDay$Steps_per_day, na.rm=T)

hist(totalStepsPerDay$Steps_per_day)

ggplot(totalStepsPerDay, aes(Steps_per_day)) +
        geom_histogram(binwidth=2500) +
        geom_vline(aes(xintercept=mean(Steps_per_day, na.rm=T)),  # Ignore NA values for mean
                   color="red",  size=2) +
        geom_vline(aes(xintercept=median(Steps_per_day, na.rm=T)),
                   color="green", size=1, linetype="dashed")


## What is the average daily activity pattern?
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

avgStepsPerInterval =  data %>%
        group_by(interval) %>%
        summarize(Mean_steps_per_interval=mean(steps, na.rm = TRUE))

ggplot(avgStepsPerInterval, aes(interval, Mean_steps_per_interval)) +
        geom_line()

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

intervalWithMaxSteps = avgStepsPerInterval %>%
       arrange(desc(Mean_steps_per_interval)) %>%
        slice(1)

## Imputing missing values
## Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

NAs = data %>%
        filter(is.na(steps))
length(NAs$steps)

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
## Create a new dataset that is equal to the original dataset but with the missing data filled in.

dataNoNAs = data %>%
        group_by(interval) %>%
        mutate(steps1 = ifelse(is.na(steps), median(steps, na.rm=T) , steps))

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


totalStepsPerDayNoNAs = dataNoNAs %>%
        group_by(date) %>%
        summarize(Steps_per_day=sum(steps, na.rm = FALSE))

meanStepsPerDayNoNAs = mean(totalStepsPerDayNoNAs$Steps_per_day, na.rm=T)

medianStepsPerDayNoNAs = median(totalStepsPerDayNoNAs$Steps_per_day, na.rm=T)


ggplot(totalStepsPerDayNoNAs, aes(Steps_per_day)) +
        geom_histogram(binwidth=1000) +
        geom_vline(aes(xintercept=mean(Steps_per_day, na.rm=T)),  # Ignore NA values for mean
                   color="red",  size=2) +
        geom_vline(aes(xintercept=median(Steps_per_day, na.rm=T)),
                   color="green", size=1, linetype="dashed")

# Are there differences in activity patterns between weekdays and weekends?

## For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


dataNoNas1 = dataNoNAs %>%
        mutate(day = wday(date )) %>%
        mutate(day_factor = ifelse(day==1 | day==7 , "weekend" , "weekday")) %>%
        mutate(day_factor = as.factor(day_factor))


## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

avgStepsPerIntervalPerWeekday =  dataNoNas1 %>%
        group_by(day_factor,interval) %>%
        summarize(avg_steps_per_interval_per_weekday=mean(steps, na.rm = TRUE))

ggplot(avgStepsPerIntervalPerWeekday, aes(interval,avg_steps_per_interval_per_weekday)) +
        geom_line() +
        facet_grid(day_factor~.)