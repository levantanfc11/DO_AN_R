library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
#du lieu vao
link = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/06-16-2021.csv"
df <- read.csv(link, header = TRUE, sep = ",")

country <- df %>% group_by(Country_Region)
country <- country %>% summarise(Recovered=sum(Recovered),
                                 Confirmed=sum(Confirmed),
                                 Deaths=sum(Deaths),
                                 Active=sum(Active))

country <- country[order(-country$Confirmed),]
groupbar <- country %>% filter(Country_Region == "Laos")
india<-country %>% filter(Country_Region == "Thailand")
groupbar<-rbind(groupbar,india)
rus<-country %>% filter(Country_Region == "Vietnam")
groupbar<-rbind(groupbar,rus)


fig <- plot_ly(groupbar, x = ~Country_Region, y = ~Confirmed, type = 'bar', name = 'Confirmed',text =groupbar$Confirmed, textposition = 'auto',
               marker= list(color='lightred'))
fig <- fig %>% add_trace(y = ~Recovered, name = 'Recovered',text =groupbar$Recovered, textposition = 'auto',
                         marker=list(color='lightgreen'))
fig <- fig %>% add_trace(y = ~Deaths, name = 'Deaths',text =groupbar$Deaths, textposition = 'auto',
                         marker=list(color='pink'))
fig <- fig %>% layout(title='Group Bar Chart',
                      yaxis = list(title = 'Con Nguoi'),
                      xaxis = list(title = 'QUOC GIA'), barmode = 'group')

fig