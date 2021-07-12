library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)


url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/16-06-2021.csv"
df <- read.csv(url, header = TRUE)

country <- df %>% group_by(Country_Region)
country <- country %>% summarise(Recovered=sum(Recovered),
                                 Confirmed=sum(Confirmed),
                                 Deaths=sum(Deaths),
                                 Active=sum(Active))
#Sort
country <- country[order(-country$Confirmed),]
country<-country[-c(1,2,3,4,7,11),]
country<-country<-country[16: 22,]

fig <- plot_ly(country, x = ~Country_Region, y = ~Active, type = 'bar', name = 'Ca nhiem')
fig <- fig %>% add_trace(y = ~Recovered, name = 'Da chua tri')
fig <- fig %>% add_trace(y = ~Deaths, name = 'Chet',text =country$Confirmed, textposition = 'auto')
fig <- fig %>% layout(title='Stacked Bar Chart',yaxis = list(title = 'Count'), barmode = 'stack')

fig