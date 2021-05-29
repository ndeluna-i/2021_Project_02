# packages used
library(RCurl)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

# read_csv downloads is better, single line of code this time and wont mess up column names
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# Your first task as a CSIT-165 data scientist is to determine the global trajectory of COVID-19 as far as 
# confirmed, recovers, and deaths are concerned.Create three overlapped scatter plots for the global 
# trajectory of COVID-19 for confirmations, recoveries, and deaths. This required that the cases for 
# each date are summed for all countries and provinces in the data set. The scatter plots should be similar 
# to the plots shown below (custom colors were used for these plots for continuity with the rest of the project
# but that is not required).

# use ncol(confirmed) to get most up to date number of columns, create new data frames with total sums for each day
# use gather() to make long form data

confirmed_sum <- confirmed %>%
  select(5:ncol(confirmed)) %>%
  summarize_if(is.numeric,sum,na.rm = TRUE) %>%
  gather(, key=date, value=confirmed)

deaths_sum <- deaths %>%
  select(5:ncol(deaths)) %>%
  summarize_if(is.numeric,sum,na.rm = TRUE) %>%
  gather(, key=date, value=confirmed)
  
recovered_sum <- recovered %>%
  select(5:ncol(recovered)) %>%
  summarize_if(is.numeric,sum,na.rm = TRUE) %>%
  gather(, key=date, value=confirmed)

# create global trajectory data frame, needs some cleaning up
global_trajectory <- left_join(confirmed_sum, deaths_sum, by="date")
global_trajectory <- left_join(global_trajectory, recovered_sum, by="date")
colnames(global_trajectory) <- c("Date", "Confirmed", "Deaths", "Recovered")
global_trajectory <- gather(global_trajectory, key=Object, value=Total, -Date)
global_trajectory$Date <- as.Date(global_trajectory$Date, "%m/%d/%y")

# plot, needs adjustments and have a weird hiccup right before 2021
ggplot(global_trajectory) +
  geom_point(aes(x=Date, y=Total, color=Object), size = 0.25)+
  labs(title = "Global Trajectory of COVID-19",
    y = "Cases",
    x = "Date",
    color = "Global Data Set") +
  scale_y_continuous(limits = c(0,200000000), labels = scales::comma) +
  scale_x_date(date_labels = "%B'%y")

#UPDATE 05/28/2021: I think this is good now. 
  