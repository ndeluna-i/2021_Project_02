# addapted from Marc's code

library(knitr)
library(kableExtra)

# data frames
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# subsetting and organizing 3 individual data frames 
confirmed_sum <- confirmed %>%
  select(2, ncol(confirmed)) %>%
  rename(., Country = 'Country/Region', Counts = '5/28/21') %>%
  group_by(Country) %>%
  summarize_if(is.numeric,sum,na.rm = TRUE) %>%
  arrange(desc(Counts)) %>%
  mutate(Rank = 1:193) %>%
  subset(select= c(3, 1, 2))

recovered_sum <- recovered %>%
  select(2, ncol(recovered)) %>%
  rename(Country = 'Country/Region', Counts = '5/28/21') %>%
  group_by(Country) %>%
  summarize_if(is.numeric,sum,na.rm = TRUE) %>%
  arrange(desc(Counts)) %>%
  mutate(Rank = 1:193) %>%
  subset(select= c(3, 1, 2))

deaths_sum <- deaths %>%
  select(2, ncol(deaths)) %>%
  rename(Country = 'Country/Region', Counts = '5/28/21') %>%
  group_by(Country) %>%
  summarize_if(is.numeric,sum,na.rm = TRUE) %>%
  arrange(desc(Counts)) %>%
  mutate(Rank = 1:193) %>%
  subset(select= c(3, 1, 2))

#combining into one data frame with 7 columns 
combine <- full_join(confirmed_sum, recovered_sum, by = "Rank") %>%
  full_join(., deaths_sum, by = "Rank")

#create kable 
kable(combine,
      col.names = c("Rank", "Country", "Count", "Country", "Count", "Country", "Count"),
      align = "clrlrlr") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left",font_size = 12) %>%
  column_spec(1, bold = T) %>%
  add_header_above(c(" " = 1, "Confirmations" = 2, "Recoveries" = 2, "Deaths" = 2))