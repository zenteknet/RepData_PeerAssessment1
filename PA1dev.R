# Repro Research Project 1 code development

## Load the data

### Load default libraries
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(httr)
library(XML)
library(quantmod)
library(pastecs)
library(psych)

unzip("activity.zip")
data = read.csv("activity.csv", stringsAsFactors=FALSE)
data = tbl_df(data)

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
        

