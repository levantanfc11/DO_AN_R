#cai dat thu vien can thiet
update.packages("tools")
install.packages("jsonlite", type = "source") 
install.packages("ggplot2", lib="C:\\Users\\USER\\Documents\\R\\win-library")
update.packages("ggplot2")
update.packages("data.table")
library(data.table)

#doc du lieu vao dataframe
setwd("C:/VANTAN/COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports_us")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist(temp, fill = TRUE)
names(data)
View(data)

#tao dataframe ve so luong nguoi nhiem, nguoi chet, nguoi binh phuc, ti le tu vong tung ngay cua bang Texas , Washington o USA
texas <- data[Province_State=="Texas"]
washington <- data[Province_State=="Washington"]
texas$Case_Fatality_Ratio <- format(round(texas$Case_Fatality_Ratio, 2), nsmall = 2)
washington$Case_Fatality_Ratio <- format(round(washington$Case_Fatality_Ratio, 2), nsmall = 2)

#doc du lieu covid cua USA ngay 07-11-2021
df <- read.table("07-11-2021.csv",
                 header = TRUE,
                 sep = ",")
names(df)


df$Case_Fatality_Ratio <- format(round(df$Case_Fatality_Ratio, 2), nsmall = 2)
df$Case_Fatality_Ratio

#doc du lieu covid cua USA ngay 16-6-2021
df1 <- read.table("06-16-2021.csv",
                  header = TRUE,
                  sep = ",")
names(df1)

df1$Case_Fatality_Ratio <- format(round(df1$Case_Fatality_Ratio, 2), nsmall = 2)
df1$Case_Fatality_Ratio

#dung thu vien ggplot2 de ve bieu do
library("ggplot2", lib.loc = "~/R/win-library/4.0")

#bieu do 1
ggplot(texas, aes(x=Deaths, y=Last_Update)) + 
  geom_point(aes(colour = Confirmed), colour = "pink") + 
  labs(title="D??? TH??? TH??? HI???N S??? LU???NG CA T??? VONG DO COVID C???A BANG 
      TEXAS T??? KHI GHI NH???N CA T??? VONG D???U TIÊN CHO D???N THÁNG 07-2021")

#bieu do 2
ggplot(df1[5:9,], aes(x='', y=Deaths, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void() + labs(title="D??? TH??? TH??? HI???N S??? CA T??? VONG C???A CÁC BANG ??? USA T??? KHI 
                      GHI NH???N CA T??? VONG D???U TIÊN CHO D???N NGÀY 16-06-2021")  +
  geom_text(aes(label = paste0(Deaths)), position = position_stack(vjust=0.5)) 

#bieu do 3
ggplot(washington, aes(x=Deaths, y=Last_Update)) + 
  geom_point(aes(colour = Deaths), colour = "orange") +
  labs(title="D??? TH??? TH??? HI???N S??? LU???NG CA T??? VONG C???A BANG WASHINGTON
              T??? KHI GHI NH???N CA NHI???M D???U TIÊN CHO D???N THÁNG 07-2021",x = "Deaths", y="Last Update")

#bieu do 4
ggplot(df, aes(x=Deaths, color=Province_State)) +
  geom_col(aes(x=Deaths, y=Province_State, fill =Province_State)) + 
  theme_grey() +
  labs(title="D??? TH??? TH??? HI???N S??? CA T??? VONG C???A CÁC BANG ??? USA T??? KHI GHI NH???N CA T??? VONG D???U TIÊN CHO D???N NGÀY 11-07-2021 ")


#bieu do 5
ggplot(df1, aes(x=Confirmed, y=Province_State, fill= Confirmed)) + 
  geom_point(aes(color=Confirmed)) +
  labs(title="DÒ TH??? TH??? HI???N S??? CA NHI???M M???I DU???C PHÁT HI???N C???A NGÀY 16-06-2021",x = "Confirmed", y="Province")

#bieu do 6
ggplot(df[5: 9,], aes(x='', y=Confirmed, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="D??? TH??? TH??? HI???N S??? CA NHI???M COVID C???A CÁC BANG ??? USA T??? LÚC 
       GHI NH???N CA NHI???M D???U TIÊN D???N NGÀY 11-07-2021") +
  geom_text(aes(label = paste0(Confirmed)), position = position_stack(vjust=0.5)) 
#bieu do 7
ggplot(df[5:9,], aes(x=Case_Fatality_Ratio, y=Province_State, color=Province_State)) + 
  geom_point() + 
  labs(title="D??? TH??? TH??? HI???N T??? L??? T??? VONG C???A CÁC BANG ??? USA T??? LÚC 
       GHI NH???N CA NHI???M D???U TIÊN D???N NGÀY 11-07-2021",x="Mortality Rate", y="Province")
#bieu do 8
ggplot(df[13:17,], aes(x='', y=Case_Fatality_Ratio, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() + coord_polar("y", start=0) + 
  labs(title="D??? TH??? TH??? HI???N T??? L??? T??? VONG C???A CÁC BANG ??? USA T??? LÚC 
       GHI NH???N CA NHI???M D???U TIÊN D???N NGÀY 11-07-2021")  +
  geom_text(aes(label = paste0(Case_Fatality_Ratio)), position = position_stack(vjust=0.5)) 

#bieu do 9
ggplot(df1[25:29,], aes(x='', y=Deaths, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void() + labs(title="D??? TH??? TH??? HI???N S??? CA T??? VONG C???A CÁC BANG ??? USA T??? LÚC 
       GHI NH???N CA NHI???M D???U TIÊN D???N NGÀY 16-06-2021")  +
  geom_text(aes(label = paste0(Deaths)), position = position_stack(vjust=0.5)) 

#bieu do 10
ggplot(df1[15:25,], aes(x=Deaths, y=Province_State)) +
  geom_col(aes(x=Deaths, y=Province_State, fill = Province_State)) + 
  labs(title="D??? TH??? TH??? HI???N S??? CA T??? VONG C???A CÁC BANG ??? USA T??? LÚC 
       GHI NH???N CA NHI???M D???U TIÊN D???N NGÀY 16-06-2021",x = "Deaths", y="Province")

#bieu do 11
ggplot(texas) + 
  geom_line(aes(y=Case_Fatality_Ratio, x=Last_Update, color=Case_Fatality_Ratio)) +
  theme_gray() +
  labs(title="D??? TH??? TH??? HI???N T??? L??? T??? VONG C???A BANG 
      TEXAS T??? KHI GHI NH???N CA NHI???M D???U TIÊN CHO D???N THÁNG 07-2021", x="Last Update", y="Case Fatality Ratio")
