library(ggplot2)
library(tidyverse)
library(lubridate)
library(plotly)
theme_set(theme_minimal())

confirm_url<-'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
confirm <- read_csv(confirm_url)
confirm <- confirm %>%
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long),
               names_to = "date",
               values_to = "confirmed_n"
  ) %>%
  select(-c(Lat, Long)) %>%
  rename(
    province_state = `Province/State`,
    country_region = `Country/Region`
  ) %>%
  mutate(date = mdy(date)) %>%
  group_by(country_region, date) %>%
  summarise(confirmed_n = sum(confirmed_n)) %>%
  ungroup()
confirm <- confirm %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(new_cases_n = confirmed_n - lag(confirmed_n, default = 0)) %>%
  ungroup()
confirm <- confirm %>%
  arrange(date) %>%
  group_by(country_region) %>%
  ungroup()

vn<-confirm %>% filter(country_region == "Vietnam")
vn <-vn[c('country_region','new_cases_n')]
vio<-vn


lao<-confirm %>% filter(country_region == "Laos")
lao <-lao[c('country_region','new_cases_n')]
vio<-rbind(vio, lao) 

tl<-confirm %>% filter(country_region == "Thailand")
tl <-tl[c('country_region','new_cases_n')]
vio<-rbind(vio, tl) 
 

fig <- vio %>%
  plot_ly(x = ~country_region, y = ~new_cases_n,
          split = ~country_region,
          type = 'box',
          box = list(
            visible = T
          ),
          meanline = list(
            visible = T
          )
  ) 

fig <- fig %>%
  layout(
    title='Boxplot',
    xaxis = list(
      title = "Country"
    ),
    yaxis = list(
      title = "New cases each day",
      zeroline = F
    )
  )

fig