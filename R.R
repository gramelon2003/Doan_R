library(readxl)
library(tidyverse)
library(lubridate)
library(psych)
library(DT)

library(readxl)

setwd <- ("E:/")
Hotel <- read_excel("E:/Hotel(1).xlsx")


#loai bo gia tri na
str(Hotel)
head(Hotel, 10)
colsums(is.na(Hotel))
colSums(is.na(Hotel))
Hotel_1 <- na.omit(Hotel)
colSums(is.na(Hotel))
colSums(is.na(Hotel_1))
summary(Hotel_1)
head(Hotel_1,10)
head(Hotel_1,10)
multi.hist(Hotel_1[,sapply(Hotel_1, is.numeric)])
dfHotel <- data.frame(Hotel_1)



#loai bo bien dinh tinh
dfHotel1 <- dfHotel[ -c(1, 2, 4, 5, 7, 8, 19)]

#mean, mode, median
getmode <- function(v) {
uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]
}

mean <- colMeans(dfHotel)
mode <- apply(dfHotel1,2,getmode)
median <- apply(dfHotel1,2,median)
mean
mode
median
# do lech chuan
std <- apply(dfHotel1,2,sd)
std
# sai so chuan
se <- std/sqrt(length(dfHotel1))
se
#Max min
max<- apply(dfHotel1,2,max)
min <- apply(dfHotel1,2,min)
#gan gt
dfPoor<-dfHotel1$Poor
dfTerrible=dfHotel1$Terrible
dfVeryGood=dfHotel1$VeryGood
dfAverage=dfHotel1$Average
df1=c(sum(dfPoor),sum(dfTerrible),sum(dfVeryGood),sum(dfAverage))
df2=c(dfTerrible,dfVeryGood)
color=c("lightblue","red","yellow","purple")

#ve Bieu do
pie(df1,labels = c("poor","Terrible","VeryGood","Average"),main = "Bi???u d??? tròn mô t??? d??? li???u khách s???n",col = color)
boxplot(dfAverage,dfVeryGood,labels = c("Average","VeryGood"),main="bi???u d??? boxplot mô t??? d??? li???u Avarage và VeryGood",col = "blue",horizontal = TRUE,xlab="count",ylab="index")
barplot(df1,names.arg = c("poor","Terrible","VeryGood","Average"),main="Bi???u d??? bar c???a d??? li???u khách s???n",col="blue")

