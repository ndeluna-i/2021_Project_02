
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

## Graph for CA confirmed and deaths

ggplot(data = CAcondeath) +
  geom_point(mapping = aes(x = date, y = number, color = Province_State))+
  labs(
    title = "California's Trajectory for COVID-19",
    y = "Confirmed Number",
    x = "Date"
  ) + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme_minimal() +
  scale_y_continuous(limits=c(0,3800000), labels = scales::comma) +
  scale_x_discrete(breaks = c("2020-01-22", "2020-07-01", "2020-12-13", "2021-05-27")) +
  geom_vline(xintercept = "2020-03-19", size = .25, linetype = "longdash") +
  theme(legend.title = element_blank())

#### Process for three main cities

lastday <- ncol(Calconfirmed)

Top3_CA_pre <- arrange(Calconfirmed, -Calconfirmed[lastday])
Top3_CA <- head(Top3_CA_pre, n = 3)
Top3_CA_needed <- select(Top3_CA, -c(UID, iso2, iso3, code3, FIPS, Province_State, Country_Region, Lat, Long_, Combined_Key))
Top3_CA_grouped <- Top3_CA_needed %>%
  group_by(Admin2) %>%
  summarize_each(funs(sum))
colnames(Top3_CA_grouped)[1] = "City"

CAcity_gathered <- gather(
  Top3_CA_grouped,
  key = date,
  value = number,
  -City
)

CAcity_gathered_1 <- str_replace_all(CAcity_gathered$date, "X", "")
CAcity_gathered_2 <- gsub("\\.", "/", CAcity_gathered_1)
CAcity_gathered$date <- CAcity_gathered_2
CAcity_gathered_exper <- CAcity_gathered
CAcity_gathered_exper$date <- as.Date(CAcity_gathered_exper$date, "%m/%d/%Y")
CAcity_gathered_exper$date <- str_replace(CAcity_gathered_exper$date, "0", "2") 

###  Graph for cities

ggplot(data = CAcity_gathered_exper) +
  geom_point(mapping = aes(x = date, y = number, color = City))+
  labs(
    title = "California's Top 3 Cities with confirmed COVID-19 cases",
    y = "Confirmed Number",
    x = "Date"
  ) + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme_minimal() +
  scale_y_continuous(limits=c(0,1300000), labels = scales::comma) +
  scale_x_discrete(breaks = c("2020-01-22", "2020-07-01", "2020-12-13", "2021-05-27")) +
  geom_vline(xintercept = "2020-03-19", size = .25, linetype = "longdash") +
  theme(legend.title = element_blank())

                   