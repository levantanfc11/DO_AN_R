library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
#du lieu vao
link = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/06-16-2021.csv"
df <- read.csv(link, header = TRUE, sep = ",")

country <- df %>% group_by(Country_Region)
country <- country %>% summarise(Recovered=sum(Recovered),
                                 Confirmed=sum(Confirmed))

country <- country[order(-country$Confirmed),]
country<-country[-c(1,2,4,6,7),]
country<-country<-country[1:10,]
country$Country_Region <- factor(df$Country_Region, levels = df$Country_Region[order(df$Confirmed)])

fig <- plot_ly(country, color = I("gray80"))
fig <- fig %>% add_segments(y = ~Recovered, yend = ~Confirmed, x = ~Country_Region, xend = ~Country_Region, showlegend = FALSE)
fig <- fig %>% add_markers(y = ~Recovered, x = ~Country_Region, name = "Recovered", color = I("pink"))
fig <- fig %>% add_markers(y = ~Confirmed, x = ~Country_Region, name = "Confirmed", color = I("red"))
fig <- fig %>% layout(
  title = "Dumbbell Plot",
  xaxis = list(title = "Quoc Gia"),
  yaxis = list(title='Con Nguoi')
)

fig