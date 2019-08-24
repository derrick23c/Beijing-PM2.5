setwd("C:/Users/derri/Onedrive/r")
library(readxl)
Beijing_2008_HourlyPM2_5<- as.data.frame(read_excel("Beijing_2008_HourlyPM2.5.xls",na="-999"))
Beijing_2009_HourlyPM2_5 <- as.data.frame(read_excel("Beijing_2009_HourlyPM2.5.xls",na="-999"))
Beijing_2010_HourlyPM2_5 <- as.data.frame(read_excel("Beijing_2010_HourlyPM2.5.xls",na="-999"))
Beijing_2011_HourlyPM2_5 <- as.data.frame(read_excel("Beijing_2011_HourlyPM2.5.xls",na="-999"))
Beijing_2012_HourlyPM2_5 <- as.data.frame(read_excel("Beijing_2012_HourlyPM2.5.xls",na="-999"))
Beijing_2013_HourlyPM2_5 <- as.data.frame(read_excel("Beijing_2013_HourlyPM2.5.xls",na="-999"))
Beijing_2014_HourlyPM2_5 <- as.data.frame(read_excel("Beijing_2014_HourlyPM2.5.xls",na="-999"))
Beijing_2015_HourlyPM2_5 <- as.data.frame(read_excel("Beijing_2015_HourlyPM2.5.xls",na="-999"))
Beijing_2016_HourlyPM2_5 <- as.data.frame(read_excel("Beijing_2016_HourlyPM2.5.xls",na="-999"))
Beijing_2017_HourlyPM2_5 <- as.data.frame(read_excel("Beijing_2017_HourlyPM2.5.xls",na="-999"))
pm2.5<-
  rbind(Beijing_2008_HourlyPM2_5,Beijing_2009_HourlyPM2_5,Beijing_2010_HourlyPM2_5,Beijing_2011_HourlyPM2_5,
        Beijing_2012_HourlyPM2_5,Beijing_2013_HourlyPM2_5,Beijing_2014_HourlyPM2_5,Beijing_2015_HourlyPM2_5,Beijin
        g_2016_HourlyPM2_5,Beijing_2017_HourlyPM2_5)
attach(pm2.5)
names(pm2.5)
pm2.5$Date<-as.Date(pm2.5$Date, format=c('y-m-d','h:m:s'))
plot(pm2.5$Value ~ pm2.5$Date,axt = "n", type = "l")
data <- read.table("monthly2.txt",header=TRUE)
pm2.5ts <- ts(data,start=c(2010,1))
plot(pm2.5ts)
library(forecast)
library(astsa)
library(tseries)
adf.test(pm2.5ts)
install.packages("aTSA")
library(aTSA)
acf2(pm2.5ts)
library(zoo)
library(xts)
library(trend)
pm2.5timeseries <- ts(data,frequency=12,start=c(2010,1))
plot(pm2.5timeseries)
pm2.5decom <- decompose(pm2.5timeseries)
plot(pm2.5decom)
pm2.5noseason <- pm2.5timeseries-pm2.5decom$seasonal
plot(pm2.5noseason)
mk.test(pm2.5timeseries)
mk.test(pm2.5noseason)
adf.test(pm2.5noseason)
pp.test(pm2.5noseason)
stationary.test(pm2.5noseason, method = "pp") # same as pp.test(x)
stationary.test(pm2.5noseason, method = "kpss") # same as kpss.test(x)
fit1 <- auto.arima(pm2.5timeseries,seasonal = TRUE, approximation = FALSE, stepwise = FALSE)
fit2 <- forecast(fit1,h=12)
plot(fit2)
plot_ly(data=new,x=~date)%>%
  add_lines(y=~pm2.5values,name = "Measured by US Embassy")%>%
  add_lines(y=~pm2.5c, name = "Measured by China")
fit1 <- auto.arima(pm2.5values,seasonal = TRUE, approximation = FALSE, stepwise = FALSE)
fit1
fit2 <- forecast(fit1,h=12)
plot(fit2)
newdata <- read.table("monthly3.txt",header=TRUE)
pm2.5ts <- ts(newdata,frequency=12,start=c(2010,1))
pm2.5decom1 <- decompose(pm2.5ts)
plot(pm2.5decom1)
pm2.5noseason1 <- pm2.5ts-pm2.5decom1$seasonal
plot(pm2.5noseason)
plot(pm2.5decom1$trend)
adf.test(pm2.5noseason1)
pp.test(pm2.5noseason1)
attach(newdata)
fit3 <- auto.arima(pm2.5values,seasonal = TRUE, approximation = FALSE, stepwise = FALSE)
fit3
fit4 <- forecast(fit4,h=12)
plot(fit4)
data <- read.csv("NEW13-17.csv", header = TRUE)
library(ggplot2)
library(plotly)
data1 <- data[c(90:94),] #2014/03/01~2014/03/15
data1 <- data.frame(data1)
data2 <- data[c(95:99),] #2014/03/16~2014/03/30
data2 <- data.frame(data2)
pm2.5b <- data2$DAILYM
data3 <- data[c(:364),] #2013/11/16-2013/11/30
data3 <- data.frame(data3)
pm2.5c <- data3$DAILYM
data4 <- data[c(335:349),] #2013/11/01-2013/11/15
data4 <- data.frame(data4)
pm2.5d <- data4$DAILYM
pm2.5 <- cbind(data1,pm2.5b,pm2.5c,pm2.5d)
plot_ly(data=pm2.5) %>%
  add_boxplot(y=~DAILYM, name = "03/01/15-03/15/15 ¹©Å¯", boxmean = TRUE) %>%
  add_boxplot(y=~pm2.5b, name = "03/16/15-03/30/15", boxmean = TRUE)%>%
  add_boxplot(y=~pm2.5c, name = "11/16/14-11/30/14 ¹©Å¯", boxmean = TRUE) %>%
  add_boxplot(y=~pm2.5d, name = "11/01/14-11/15/14", boxmean = TRUE)
setwd("C:/Users/derri/Onedrive/r")
data <- read.table("monthly2.txt",header=TRUE)
x <- ts(data,frequency=12,start=c(2010,1))
newdata <- read.table("new.txt",header = TRUE)
y <- ts(newdata,frequency=12,start=c(2013,12))
plot(x)
plot(y)
require(graphics)
ts.plot(x, y,
        gpars=list(xlab="date", ylab="pm2.5values", lty=c(1:3)))
library(ggplot2)
attach(data)
pm2.5old <- pm2.5values[47,90]
pm2.5new <- newdata$pm2.5c[1,43]
time1=seq(from=as.Date("2010-01-01"),to = as.Date("2017-06-01"),by='month')
data1=data.frame(date=time1,pm2.51=data)
time2=seq(from=as.Date("2013-12-01"),to = as.Date("2019-04-01"),by='month')
data2=data.frame(date=time2,pm2.52=newdata)

new=merge(data1,data2,by='date',all=TRUE)
library(plotly)
plot_ly(data=new,x=~date)%>%
  add_lines(y=~pm2.5values,
            add_lines(y=~pm2.5c)
            
            pm2.5 <- read.csv("AQIDATA.csv", header = TRUE)
            data1 <- pm2.5[c(1915:1929),] #2019/03/01~2019/03/15
            data1 <- data.frame(data1)
            data2 <- pm2.5[c(1930:1944),] #2019/03/16~2019/03/30
            data2 <- data.frame(data2)
            pm2.5b <- data2$PM2.5
            data3 <- pm2.5[c(1810:1824),] #2018/11/16-2018/11/30
            data3 <- data.frame(data3)
            pm2.5c <- data3$PM2.5
            data4 <- pm2.5[c(1795:1809),] #2018/11/01-2018/11/15
            data4 <- data.frame(data4)
            pm2.5d <- data4$PM2.5
            data <- cbind(data1,pm2.5b,pm2.5c,pm2.5d)
            plot_ly(data=data) %>%
              add_boxplot(y=~PM2.5, name = "2019/03/01-2019/03/15 ¹©Å¯") %>%
              add_boxplot(y=~pm2.5b, name = "2019/03/16-2019/03/30 ²»¹©Å¯")%>%
              add_boxplot(y=~pm2.5c, name = "2018/11/16-2018/11/30 ¹©Å¯") %>%
              add_boxplot(y=~pm2.5d, name = "2018/11/01-2018/11/15 ²»¹©Å¯")
            data5 <- pm2.5[c(1550:1564),] #2018/03/01~2018/03/15
            data5 <- data.frame(data5)
            data6 <- pm2.5[c(1565:1579),] #2018/03/16~2018/03/30
            data6 <- data.frame(data6)
            pm2.5e <- data6$PM2.5
            data7 <- pm2.5[c(1445:1459),] #2017/11/16-2017/11/30
            data7 <- data.frame(data7)
            pm2.5f <- data7$PM2.5
            data8 <- pm2.5[c(1430:1444),] #2017/11/01-2017/11/15
            data8 <- data.frame(data8)
            pm2.5g <- data8$PM2.5
            datan <- cbind(data5,pm2.5e,pm2.5f,pm2.5g)
            plot_ly(data=datan) %>%
              add_boxplot(y=~PM2.5, name = "03/01/18-03/15/18 ¹©Å¯") %>%
              add_boxplot(y=~pm2.5e, name = "03/16/18-03/30/18")%>%
              add_boxplot(y=~pm2.5f, name = "11/16/17-11/30/17 ¹©Å¯") %>%
              add_boxplot(y=~pm2.5g, name = "11/01/17-11/15/17")
            data9 <- pm2.5[c(1185:1199),] #2017/03/01~2017/03/15
            data9 <- data.frame(data9)
            data10 <- pm2.5[c(1200:1214),] #2017/03/16~2017/03/30
            data10 <- data.frame(data10)
            pm2.5h <- data10$PM2.5
            data11 <- pm2.5[c(1080:1094),] #2016/11/16-2016/11/30
            data11 <- data.frame(data11)
            pm2.5i <- data11$PM2.5
            data12 <- pm2.5[c(1065:1079),] #2016/11/01-2016/11/15
            data12 <- data.frame(data12)
            pm2.5j <- data12$PM2.5
            datam <- cbind(data9,pm2.5h,pm2.5i,pm2.5j)
            plot_ly(data=datam) %>%
              add_boxplot(y=~PM2.5, name = "03/01/17-03/15/17 ¹©Å¯") %>%
              add_boxplot(y=~pm2.5h, name = "03/16/17-03/30/17")%>%
              add_boxplot(y=~pm2.5i, name = "11/16/16-11/30/16 ¹©Å¯") %>%
              add_boxplot(y=~pm2.5j, name = "11/01/16-11/15/16")
            data13 <- pm2.5[c(821:835),] #2016/03/01~2016/03/15
            data13 <- data.frame(data13)
            data14 <- pm2.5[c(836:850),] #2016/03/16~2016/03/30
            data14 <- data.frame(data14)
            pm2.5k <- data14$PM2.5
            data15 <- pm2.5[c(715:729),] #2015/11/16-2015/11/30
            data15 <- data.frame(data15)
            pm2.5l <- data15$PM2.5
            data16 <- pm2.5[c(700:714),] #2015/11/01-2015/11/15
            data16 <- data.frame(data16)
            pm2.5m <- data12$PM2.5
            datap <- cbind(data13,pm2.5k,pm2.5l,pm2.5m)
            plot_ly(data=datap) %>%
              add_boxplot(y=~PM2.5, name = "03/01/16-03/15/16 ¹©Å¯") %>%
              add_boxplot(y=~pm2.5k, name = "03/16/16-03/30/16")%>%
              add_boxplot(y=~pm2.5l, name = "11/16/15-11/30/15 ¹©Å¯") %>%
              add_boxplot(y=~pm2.5m, name = "11/01/15-11/15/15")
            apec <- read.csv("apec.csv", header = TRUE)
            plot_ly(data=apec) %>%
              add_boxplot(x=~ï»¿A A, name = "2014", boxmean = TRUE, marker = list(color = 'rgb(7,40,89)'),
                          line = list(color = 'rgb(7,40,89)')) %>%
              add_boxplot(x=~BB, name = "2013", boxmean = TRUE, marker = list(color = 'rgb(9,56,125)'),
                          line = list(color = 'rgb(9,56,125)')) %>%
              add_boxplot(x=~CC, name = "2012", boxmean = TRUE, marker = list(color = 'rgb(8,81,156)'),
                          line = list(color = 'rgb(8,81,156)')) %>%
              add_boxplot(x=~DD, name = "2011", boxmean = TRUE, marker = list(color = 'rgb(107,174,214)'),
                          line = list(color = 'rgb(107,174,214)')) %>%
              add_boxplot(x=~EE, name = "2010", boxmean = TRUE)
            t.test(apec$ï»¿A A, apec$BB)
            t.test(apec$ï»¿A A, apec$CC)
            t.test(apec$ï»¿A A, apec$DD)
            t.test(apec$ï»¿A A, apec$EE)
            apec3 <- read.csv("apec3.csv", header = TRUE)
            plot_ly(data=apec3) %>%
              add_boxplot(x=~ï»¿A A, name = "2014", boxmean = TRUE, marker = list(color = 'rgb(7,40,89)'),
                          line = list(color = 'rgb(7,40,89)')) %>%
              add_boxplot(x=~BB, name = "2013", boxmean = TRUE, marker = list(color = 'rgb(9,56,125)'),
                          line = list(color = 'rgb(9,56,125)')) %>%
              add_boxplot(x=~CC, name = "2012", boxmean = TRUE, marker = list(color = 'rgb(8,81,156)'),
                          line = list(color = 'rgb(8,81,156)')) %>%
              add_boxplot(x=~DD, name = "2011", boxmean = TRUE, marker = list(color = 'rgb(107,174,214)'),
                          line = list(color = 'rgb(107,174,214)')) %>%
              add_boxplot(x=~EE, name = "2010", boxmean = TRUE)
            t.test(apec3$ï»¿A A, apec3$BB)
            t.test(apec3$ï»¿A A, apec3$CC)
            t.test(apec3$ï»¿A A, apec3$DD)
            t.test(apec3$ï»¿A A, apec3$EE)