
## Objective 4 - Zooming into our State


library(knitr)
library(magrittr)
library(RCurl)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(tidyr)
library(stringr)


USconfirmed_download <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
USconfirmed <- read.csv(text=USconfirmed_download, stringsAsFactors = FALSE)

USdeath_download <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
USdeaths <- read.csv(text=USdeath_download, stringsAsFactors = FALSE)

Calconfirmed <- filter(USconfirmed, Province_State == "California") # Filter CA only
Caldeaths <- filter(USdeaths, Province_State == "California") # Filter CA only

View(Calconfirmed) # Only if needed
View(Caldeaths) # Only if needed

## Create California summed confirmed cases and deaths

Calcon_needed <- select(Calconfirmed, -c(UID, iso2, iso3, code3, FIPS, Admin2, Country_Region, Lat, Long_, Combined_Key))

Calcon_grouped <- Calcon_needed %>%
  group_by(Province_State) %>%
  summarize_each(funs(sum))
Calcon_grouped[1] <- "CA_confirmed"  

Caldeath_needed <- select(Caldeaths, -c(UID, iso2, iso3, code3, FIPS, Admin2, Country_Region, Lat, Long_, Combined_Key, Population))

Caldeath_grouped <- Caldeath_needed %>%
  group_by(Province_State) %>%
  summarize_each(funs(sum))
Caldeath_grouped[1] <- "CA_deaths"

## Gather data to long format

CAcon_gathered <- gather(
  Calcon_grouped,
  key = date,
  value = number,
  -Province_State
)

CAdeath_gathered <- gather(
  Caldeath_grouped,
  key = date,
  value = number,
  -Province_State
)

## Date manipulation/formatting/union

CAcon_gathered_1 <- str_replace_all(CAcon_gathered$date, "X", "")
CAcon_gathered_2 <- gsub("\\.", "/", CAcon_gathered_1)
CAcon_gathered$date <- CAcon_gathered_2
CAcon_gathered_exper <- CAcon_gathered
CAcon_gathered_exper$date <- as.Date(CAcon_gathered_exper$date, "%m/%d/%Y")
CAcon_gathered_exper$date <- str_replace(CAcon_gathered_exper$date, "0", "2") 

CAdeath_gathered_1 <- str_replace_all(CAdeath_gathered$date, "X", "")
CAdeath_gathered_2 <- gsub("\\.", "/", CAdeath_gathered_1)
CAdeath_gathered$date <- CAdeath_gathered_2
CAdeath_gathered_exper <- CAdeath_gathered
CAdeath_gathered_exper$date <- as.Date(CAdeath_gathered_exper$date, "%m/%d/%Y")
CAdeath_gathered_exper$date <- str_replace(CAdeath_gathered_exper$date, "0", "2") 

CAcondeath <- rbind(CAcon_gathered_exper, CAdeath_gathered_exper)

View(CAcondeath)


## Experimental graph


ggplot(data = CAcondeath) +
  geom_point(mapping = aes(x = date, y = number, color = Province_State))+
  labs(
    title = "California's Trajectory for COVID-19",
    y = "Confirmed Number",
    x = "Date"
  ) + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme_minimal() +
  scale_y_continuous(limits=c(0,3800000)) +
  scale_x_discrete(breaks = c("2020-01-22", "2020-06-01", "2021-01-01", "2021-05-18")) +
  theme(legend.title = element_blank())
