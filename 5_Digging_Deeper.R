
## Objective 5 - Digging Deeper

library(knitr)
library(magrittr)
library(RCurl)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(tidyverse)
library(cowplot)


USconfirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
USdeaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

##  Modified population dataset from this website: 
##  https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html

USpop <- read.csv("https://raw.githubusercontent.com/ndeluna-i/2021_Project_02/main/Population_Data_Join.csv")

Confirmed_needed <- select(USconfirmed, c(Combined_Key, ncol(USconfirmed)))

Pop_confirmed <- left_join(Confirmed_needed, USpop, by = c("Combined_Key" = "Geographic.Area"))
Pop_confirmed <- Pop_confirmed %>% drop_na()
colnames(Pop_confirmed) = c("City", "Confirmed", "Population")

## Graph for CA confirmed and deaths

obj5_p1 <- ggplot(data = Pop_confirmed) +
  geom_point(mapping = aes(x = Population, y = Confirmed), color = "dark blue") +
  labs(
    title = "Population vs. Confirmed",
    y = "Confirmed Number",
    x = "Population (2019)"
  ) + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme_minimal() +
  scale_y_continuous(trans = "log2", limits=c(1,3000000), labels = scales::comma) +
  scale_x_continuous(trans = "log2", limits=c(17,10000000), labels = scales::comma) +
  theme(legend.title = element_blank())


###  Confirmed vs. Deaths

Deaths_needed <- select(USdeaths, c(Combined_Key, ncol(USdeaths)))
Con_death_comp <- left_join(Confirmed_needed, Deaths_needed, by = "Combined_Key")
Con_death_comp <- Con_death_comp %>% drop_na()
colnames(Con_death_comp) = c("City", "Confirmed", "Deaths")
Con_death_comp <- filter(Con_death_comp, Confirmed > 0)


obj5_p2 <- ggplot(data = Con_death_comp) +
  geom_point(mapping = aes(x = Confirmed, y = Deaths), color = "dark red") +
  labs(
    title = "Confirmed Cases to Reported Number of Deaths",
    y = "Number of Deaths",
    x = "Number of Confirmed Cases"
  ) + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme_minimal() +
  scale_y_continuous(trans = "log2", limits=c(1,200000), labels = scales::comma) +
  scale_x_continuous(trans = "log2", limits=c(1,3000000), labels = scales::comma) +
  theme(legend.title = element_blank())

plot_grid(obj5_p1, obj5_p2, labels = c('A', 'B'))



#NDL attempt
library(readr)

deaths_US <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
confirmed_US <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

# note population data is in deaths_US data frame as Population, column 12

# make a pop vs confirmed counts data frame, start with variables 

Population <- deaths_US$Population
City <- deaths_US$Admin2
Confirmation <- confirmed_US$'5/28/21'
State <- confirmed_US$Province_State
Deaths <- deaths_US$'5/28/21'

data <- data.frame(City, State, Population, Confirmation, Deaths)

data %>%
  

# make plot
g1 <- ggplot(data = data) +
  geom_point(mapping = aes(x = Population, y = Confirmation), color = "red") +
  labs(
    title = "Population vs. Confirmed",
    y = "Confirmed Number",
    x = "Population"
  ) +
  scale_y_log10(limits=c(1,200000), labels = scales::comma) +
  scale_x_log10(limits=c(1,11000000), labels = scales::comma)

g2 <- ggplot(data = data) +
  geom_point(mapping = aes(x = Confirmed, y = Deaths), color = "dark blue") +
  labs(
    title = "Confirmed Cases to Reported Number of Deaths",
    y = "Number of Deaths",
    x = "Number of Confirmed Cases"
  ) +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10(labels = scales::comma)

#side by side
plot_grid(g1, g2)

