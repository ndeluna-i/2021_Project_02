library(leaflet)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(htmltools)



# After realizing the vastness of the spread of COVID-19 from your global 
# trajectory you are then asked to create a world map to gain an appreciation 
# for where the most occurrences are.

# Create this map using leaflet for the most recent date as shown below. 
# Notice that hover labels show the province when the data is available 
# and show the country when it is not. Also notice that the popup labels 
# are customized to the data that is being clicked on. For extra help 
# using leaflet, please consult this website along with the information 
# provided in your textbooks.

# 
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

#subset each data frame
confirmed_recent <- confirmed %>%
  select(1:4, ncol(confirmed))

recovered_recent <- recovered %>%
  select(1:4, ncol(recovered))

deaths_recent <- deaths %>%
  select(1:4, ncol(deaths))

#join the three subsets into one data frame
all_recent <- left_join(confirmed_recent, recovered_recent, by = c("Province/State", "Country/Region", "Lat", "Long"))
all_recent <- left_join(all_recent, deaths_recent, by = c("Province/State", "Country/Region", "Lat", "Long"))
colnames(all_recent) <- c("Province", "Country", "Lat", "Long", "Confirmed", "Recovered", "Deaths")


#make map
leaflet(data = all_recent) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng=0, lat=0, zoom = 1) %>%
  addCircleMarkers(
    lat = ~Lat,
    lng = ~Long,
    color = "silver",
    radius = ~log10(Confirmed),
    label = ~htmlEscape(Country),
    popup = paste("Confirmed:", as.character(all_recent$Confirmed), sep = " "),
    group = "Confirmed") %>%
  addCircleMarkers(
    lat = ~Lat,
    lng = ~Long,
    color = "cyan",
    radius = ~log10(Recovered),
    label = ~htmlEscape(Country),
    popup = paste("Confirmed:", as.character(all_recent$Recovered), sep = " "),
    group = "Recovered") %>%
  addCircleMarkers(
    lat = ~Lat,
    lng = ~Long,
    color = "red",
    radius = ~log10(Deaths),
    label = ~htmlEscape(Country),
    popup = paste("Confirmed:", as.character(all_recent$Deaths), sep = " "),
    group = "Deaths") %>%
  addLayersControl(
    overlayGroups = c("Confirmed", "Recovered", "Deaths"),
    options = layersControlOptions(collapsed = FALSE))

# as close as I can get it right now, needs a few things
# 1. change labels so that country shows up when province is not provided.
# 2. change formatting on popup so there is a comma in the numbers (but not sure if this matters)