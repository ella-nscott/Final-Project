#install.packages(c("lubridate", "dplyr", "ggplot2"))
#install.packages("reshape2")
#install.packages("forecast")
#install.packages("zoo")
#install.packages("tidyr")
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(forecast)
library(zoo)
library(tidyr)

#project part one: forecast future snowfall and temperature patterns using 
#historical data from two locations in minnesota: minneapolis (southern 
#centralminnesota) and grand rapids (northern central minnesota)

#monthly total snowfall (inches) in minneapolis: 1883-2024
snow <- read.csv("/cloud/project/hist.snowfall.csv")

#data cleaning
#restructure table so that all data points are in the same column
re.snow <- melt(snow, id="X")
#reformat dates
re.snow$startyear <- c(2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 
                       2013, 2011, 2012, 2010, 2009, 2008, 2007, 2006, 2005, 2004, 
                       2003, 2002, 2001, 2000, 1999, 1998, 1997, 1996, 1995, 1994, 
                       1993, 1992, 1991, 1990, 1989, 1988, 1987, 1986, 1985, 1984, 
                       1983, 1982, 1981, 1980, 1979, 1978, 1977, 1976, 1975, 1974, 
                       1973, 1972, 1971, 1970, 1969, 1968, 1967, 1966, 1965, 1964, 
                       1963, 1962, 1961, 1960, 1959, 1958, 1957, 1956, 1955, 1954, 
                       1953, 1952, 1951, 1950, 1949, 1948, 1947, 1946, 1945, 1944, 
                       1943, 1942, 1941, 1940, 1939, 1938, 1937, 1936, 1935, 1934, 
                       1933, 1932, 1931, 1930, 1929, 1928, 1927, 1926, 1925, 1924, 
                       1923, 1922, 1921, 1920, 1919, 1918, 1917, 1916, 1915, 1914, 
                       1913, 1912, 1911, 1910, 1909, 1908, 1907, 1906, 1905, 1904, 
                       1903, 1902, 1901, 1900, 1899, 1898, 1897, 1896, 1895, 1894, 
                       1893, 1892, 1891, 1890, 1889, 1888, 1887, 1886, 1885, 1884, 
                       1883)

re.snow$endyear <- c(2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 
                     2014, 2013, 2011, 2012, 2010, 2009, 2008, 2007, 2006, 2005, 
                     2004, 2003, 2002, 2001, 2000, 1999, 1998, 1997, 1996, 1995, 
                     1994, 1993, 1992, 1991, 1990, 1989, 1988, 1987, 1986, 1985, 
                     1984, 1983, 1982, 1981, 1980, 1979, 1978, 1977, 1976, 1975, 
                     1974, 1973, 1972, 1971, 1970, 1969, 1968, 1967, 1966, 1965, 
                     1964, 1963, 1962, 1961, 1960, 1959, 1958, 1957, 1956, 1955, 
                     1954, 1953, 1952, 1951, 1950, 1949, 1948, 1947, 1946, 1945, 
                     1944, 1943, 1942, 1941, 1940, 1939, 1938, 1937, 1936, 1935, 
                     1934, 1933, 1932, 1931, 1930, 1929, 1928, 1927, 1926, 1925, 
                     1924, 1923, 1922, 1921, 1920, 1919, 1918, 1917, 1916, 1915, 
                     1914, 1913, 1912, 1911, 1910, 1909, 1908, 1907, 1906, 1905, 
                     1904, 1903, 1902, 1901, 1900, 1899, 1898, 1897, 1896, 1895, 
                     1894, 1893, 1892, 1891, 1890, 1889, 1888, 1887, 1886, 1885, 
                     1884)

#assign correct year to each month
#note: the months below are selected based on the formatting of the original data
#table, which shows data from July-June rather than January-December
re.snow$trueyear <- ifelse(re.snow$variable == "Jul"| re.snow$variable == "Aug"|
                             re.snow$variable == "Sep"| re.snow$variable =="Oct" |
                             re.snow$variable == "Nov"|
                             re.snow$variable == "Dec", 
                           re.snow$startyear, re.snow$endyear )
#re-format months into usable format
#create reference table (idea from stack overflow)
month.name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                "Oct", "Nov", "Dec")
month.num <- c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12)
month.table <- data.frame(name = month.name,
                          number = month.num)
#use reference table to create numeric month column
new.snow <- re.snow #copy of original data frame
new.snow$month <- month.table$number[match(unlist(re.snow$variable), month.table$name)]

#combine month and year into single column
new.snow$Date <- ym(paste0(new.snow$trueyear,"-",new.snow$month))
new.snow$monthYear <- year(new.snow$Date)+((month(new.snow$Date)-1)/12)
#re-format trace and NA values in data
new.snow$snow.in <- as.numeric(ifelse(re.snow$value=="T", 0, re.snow$value))

#remove excess rows from df that were transferred over with melt function
new.snow <- new.snow %>%
  filter(!variable=="Total")

#arrange dates in ascending order
new.snow <- new.snow %>%
  arrange(monthYear)

#remove rows without observations
snow.no.na <- new.snow.2 %>%
  filter(is.na(snow.in)==FALSE)

#set up autoregressive model
#time series and decompositions
#snowfall data time series: 1884-present
snow_ts <- ts(snow.no.na$snow.in,
              start = c(1884, 7),
              frequency= 12)
plot(snow_ts)
#decomposition: 1884-present
snow_dec_1884 <- decompose(na.omit(snow_ts))
plot(snow_dec)

#look at trends within smaller time frames
#subset data from 2020-present 
snow.2020 <- new.snow %>%
  filter(trueyear>="2020") %>%
  arrange(monthYear)
#create time series for subset
snow_ts_2020 <- ts(snow.2020$snow.in,
                   start = c(2020, 1),
                   frequency = 12)
plot(snow_ts_2020)
#decompose time series for subset
snow_dec_2020 <- decompose(na.omit(snow_ts_2020))
plot(snow_dec_2020)

#subset data from 2000-present
snow.2000 <- new.snow %>%
  filter(trueyear >= "2000") %>%
  arrange(monthYear)
#time series for subset
snow_ts_2000 <- ts(snow.2000$snow.in,
                   start = c(2000, 1),
                   frequency = 12)
plot(snow_ts_2000)
#decompose time series for subset
snow_dec_2000 <- decompose(na.omit(snow_ts_2000))
plot(snow_dec_2000)

#autocorrelation: 2020-present
acf(na.omit(snow_ts_2020),
    lag.max = 24)

#partial acf
pacf.plot <- pacf(na.omit(snow_ts_2020))

#arima
snow_y_2020 <- na.omit(snow_ts_2020)
snw.model1 <- arima(snow_y_2020,
                order = c(1,0,0))
snw.model1
snw.model3 <- arima(snow_y_2020,
                    order = c(3,0,0))
snw.model4 <- arima(snow_y_2020,
                order = c(4,0,0))

#calculate fit
snw_AR_fit_3 <- snow_y_2020 - residuals(snw.model3)
snw_AR_fit_4 <- snow_y_2020 - residuals(snw.model4)

#plot data
plot(snow_y_2020)
#plot fit
points(snw_AR_fit_3, type="l", col = "tomato3", lty=2, lwd=2)
points(snw_AR_fit_4, type="l", col = "green3", lty=2, lwd=2)
legend("topleft", c("data","AR3","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","green3"),
       bty="n")

#forecast future data

fut.snow <- forecast(snw.model4)
fut.snow

#set up data frame
fut.snowF <- data.frame(fut.snow)
#set up dates
years <- c(rep(2024,6), rep(2025,12), rep(2026, 6))
month <- c(seq(7,12), seq(1,12), seq(1,6))
fut.snowF$date <- ymd(paste(years,"/", month,"/",1))

#make a plot with data and forecast
ggplot() +
  geom_line(data = snow.2020, aes(x = ymd(Date), y = snow.in))+
  xlim(ymd(snow.2020$Date[1]),fut.snowF$date[24])+ 
  geom_line(data = fut.snowF, aes(x = date, y = Point.Forecast),
            col="red") +  
  geom_ribbon(data=fut.snowF, 
              aes(x=date,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="Year", y="Snowfall (in)")+
  ggtitle("2 Year Forecast of Minneapolis Snowfall")


#monthly average temperature in minneapolis 1872-2024
avg.temp <- read.csv("/cloud/project/mn.avg.temp.csv")

#data cleaning
#restructure table so that all data points are in the same column
re.avg.temp <- melt(avg.temp, id="Year")

#re format months from names to numbers
#use reference table created to re format snowfall data
new.temp <- re.avg.temp #copy of original data frame
new.temp$Month <- month.table$number[match(unlist(re.avg.temp$variable), month.table$name)]

#combine month and year into single column
new.temp$Date <- ym(paste0(new.temp$Year,"-",new.temp$Month))
new.temp$monthYear <- year(new.temp$Date)+((month(new.temp$Date)-1)/12)

#re-format trace and NA values in data
new.temp$avg_temp <- as.numeric(ifelse(re.avg.temp$value=="T", 
                                          0, re.avg.temp$value))
#remove "annual" rows transferred over with melt function
new.temp <- new.temp %>%
  filter(!variable=="Annual")

#create df without na values for time series
temp.no.na <- new.temp %>%
  filter(!avg_temp=="NA")

#arrange dates in ascending order
temp.no.na<- temp.no.na %>%
  arrange(monthYear)

#set up ar model
#time series and decompositions
#avg temp time series
temp_ts <- ts(temp.no.na$avg_temp,
              start = c(1872,10),
              frequency= 12)
#decompose time series
temp_dec <- decompose(temp_ts)
plot(temp_dec)

#look at decompositions for shorter time frames
#subset original df to only include data from 2020-present
temp.2020 <- new.temp %>%
  filter(Year>=2020)%>%
  arrange(monthYear)

#create time series for subset
temp_ts_2020 <- ts(temp.2020$avg_temp,
                   start = c(2020, 1),
                   frequency = 12)
plot(temp_ts_2020)
#decompose subset time series
temp_dec_2020 <- decompose(temp_ts_2020)
plot(temp_dec_2020)

#subset original df to include data from 2000-present
temp.2000 <- new.temp %>%
  filter(Year>=2000)%>%
  arrange(monthYear)

#create time series for subset
temp_ts_2000 <- ts(temp.2000$avg_temp,
                   start = c(2000, 1),
                   frequency = 12)
plot(temp_ts_2000)
#decompose subset time series
temp_dec_2000 <- decompose(temp_ts_2000)
plot(temp_dec_2000)

#autocorrelation: 2000-present
acf(na.omit(temp_ts_2000),
    lag.max = 24)
#partial ar
temp.pacf <- pacf(na.omit(temp_ts_2000))
#arima
temp_y_2000 <- na.omit(temp_ts_2000)
model1.t <- arima(temp_y_2000,
                order = c(1,0,0))
model4.t <- arima(temp_y_2000,
                  order = c(4,0,0))
#calculate fit
t.ar.fit4 <- temp_y_2000 - residuals(model4.t)

#plot data  
plot(temp_y_2000)
#plot fit
points(t.ar.fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)

#create forecast
fut.temp <- forecast(model4.t)
fut.temp

#make new df
fut.tempF <- data.frame(fut.temp)
#set up dates for df
yearT <- c(rep(2024,2),rep(2025, 12), rep(2026,10))
monthT <- c(seq(11,12), seq(1,12), seq(1,10))
fut.tempF$Date <- ymd(paste(yearT,"/",monthT,"/",1))

#make a plot with historical data and forecast
ggplot() +
  geom_line(data = temp.2000, aes(x = ymd(Date), y = avg_temp))+
  xlim(ymd(temp.2000$Date[1]),fut.tempF$Date[24])+ 
  geom_line(data = fut.tempF, aes(x = Date, y = Point.Forecast),
            col="red") +  
  geom_ribbon(data=fut.tempF, 
              aes(x=Date,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="Year", y="Average Temperature (F)")+
  ggtitle("2 Year Forecast of Temperature in Minneapolis")



#monthly total snowfall in grand rapids (1947-present)
gr_snow <- read.csv("/cloud/project/grand_rapids_snow.csv")

#data cleaning, formatting data frame
#get rid of empty row 78
gr_snow <- gr_snow %>%
  filter(!Season=="")
#restructure table so that months are not separate columns 
re_gr_snow<- melt(gr_snow, id="Season")
#reformat years so each row only has one year
re_gr_snow$startyear <- c(2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 
                        2013, 2011, 2012, 2010, 2009, 2008, 2007, 2006, 2005, 2004, 
                        2003, 2002, 2001, 2000, 1999, 1998, 1997, 1996, 1995, 1994, 
                        1993, 1992, 1991, 1990, 1989, 1988, 1987, 1986, 1985, 1984, 
                        1983, 1982, 1981, 1980, 1979, 1978, 1977, 1976, 1975, 1974, 
                        1973, 1972, 1971, 1970, 1969, 1968, 1967, 1966, 1965, 1964, 
                        1963, 1962, 1961, 1960, 1959, 1958, 1957, 1956, 1955, 1954, 
                        1953, 1952, 1951, 1950, 1949, 1948, 1947)
re_gr_snow$endyear <- c(2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 
                       2013, 2011, 2012, 2010, 2009, 2008, 2007, 2006, 2005, 2004, 
                       2003, 2002, 2001, 2000, 1999, 1998, 1997, 1996, 1995, 1994, 
                       1993, 1992, 1991, 1990, 1989, 1988, 1987, 1986, 1985, 1984, 
                       1983, 1982, 1981, 1980, 1979, 1978, 1977, 1976, 1975, 1974, 
                       1973, 1972, 1971, 1970, 1969, 1968, 1967, 1966, 1965, 1964, 
                       1963, 1962, 1961, 1960, 1959, 1958, 1957, 1956, 1955, 1954, 
                       1953, 1952, 1951, 1950, 1949, 1948)


#assign correct year to each month
re_gr_snow$trueyear <- ifelse(re_gr_snow$variable=="Jul"|re_gr_snow$variable=="Aug"|
                                re_gr_snow$variable=="Sep"| re_gr_snow$variable=="Oct"|
                                re_gr_snow$variable=="Nov"| re_gr_snow$variable=="Dec",
                              re_gr_snow$startyear, re_gr_snow$endyear)
#use month name/number reference table created earlier in script to create 
#numeric month column
gr_snowF <- re_gr_snow #copy of original data frame
gr_snowF$month <- month.table$number[match(unlist(re_gr_snow$variable), month.table$name)]

#combine month and year into a single column
gr_snowF$Date <- ym(paste0(gr_snowF$trueyear,"-",gr_snowF$month))
gr_snowF$monthYear <- year(gr_snowF$Date)+((month(gr_snowF$Date)-1)/12)
#re-format trace and NA values in data
gr_snowF$snow.in <- as.numeric(ifelse(gr_snowF$value=="T", 0, gr_snowF$value))

#remove excess rows from df that were transferred over with melt function
gr_snowF <- gr_snowF %>%
  filter(!variable=="Season.1")

#arrange dates in ascending order
gr_snowF <- gr_snowF %>%
  arrange(monthYear)

#remove rows without observations
gr.no.na <- gr_snowF%>%
  filter(is.na(snow.in)==FALSE %>%
           arrange(monthYear))

#set up ar model 
#time series and decompositions
#time series: 1947-present
gr_snow_ts <- ts(gr.no.na$snow.in, 
                 start=c(1947, 7),
                 frequency= 12)
#decomposition: 1947-present
gr_dec_1947 <- decompose(na.omit(gr_snow_ts))
plot(gr_dec_1947)

#trends within smaller time frame
#subset 2020-present
gr_snow_2020 <- gr_snowF %>%
  filter(trueyear>="2020") %>%
  filter(!is.na(snow.in)==TRUE)%>%
  arrange(monthYear)
#time series 2020-present
gr_ts_2020 <- ts(gr_snow_2020$snow.in,
                 start = c(2020,3),
                 frequency = 12)
#decomposition of new time series
gr_dec_2020 <- decompose(na.omit(gr_ts_2020))
plot(gr_dec_2020)

#subset 2000-present
gr_snow_2000 <- gr_snowF %>%
  filter(trueyear>="2000") %>%
  filter(is.na(snow.in)==FALSE) %>%
  arrange(monthYear)
#time series 2000-present
gr_ts_2000 <- ts(gr_snow_2000$snow.in,
                 start = c(2000,4),
                 frequency = 12)
#decomposition of new time series
gr_dec_2000 <- decompose(na.omit(gr_ts_2020))
plot(gr_dec_2000)

#autocorrelation: 2020-present
acf(na.omit(gr_ts_2020),
    lag.max = 24)

#partial acf
gr.pacf.plot <- pacf(na.omit(gr_ts_2020))
#arima
gr_y_2020 <- na.omit(gr_ts_2020)
gr.model1 <- arima(gr_y_2020,
                    order = c(1,0,0))
gr.model3 <- arima(gr_y_2020,
                   order = c(3,0,0))
gr.model4 <- arima(gr_y_2020,
                   order = c(4,0,0))

#calculate fit 
gr_AR_fit1 <- gr_y_2020 - residuals(gr.model1)
gr_AR_fit3 <- gr_y_2020 - residuals(gr.model3)
gr_AR_fit4 <- gr_y_2020 - residuals(gr.model4)

#plot data
plot(gr_y_2020)

#plot fit
points(gr_AR_fit1, type="l", col = "darkgoldenrod4", lty=2, lwd=2)
points(gr_AR_fit3, type="l", col = "tomato3", lty=2, lwd=2)
points(gr_AR_fit4, type="l", col = "lightblue", lty=2, lwd=2)
legend("topleft", c("data","AR3"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3"),
       bty="n")

#forecast future data

fut.snow.gr <- forecast(gr.model4)
fut.snow.gr

#set up new data frame
fut.snow.grF <- data.frame(fut.snow.gr)

#create dates for data frame
years <- c(rep(2024,6), rep(2025,12), rep(2026, 6))
month <- c(seq(7,12), seq(1,12), seq(1,6))
fut.snow.grF$date <- ymd(paste(years,"/", month,"/",1))

#make a plot with original data and forecast
ggplot() +
  geom_line(data = gr_snow_2020, aes(x = ymd(Date), y = snow.in))+
  xlim(ymd(gr_snow_2020$Date[1]),fut.snow.grF$date[24])+ 
  geom_line(data = fut.snow.grF, aes(x = date, y = Point.Forecast),
            col="red") +  
  geom_ribbon(data=fut.snow.grF, 
              aes(x=date,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="Year", y="Snowfall (in)")+
  ggtitle("2 Year Forecast of Grand Rapids Snowfall")


#monthly average temperature in grand rapids 1915-present
gr_temp <- read.csv("/cloud/project/grand_rapids_temp.csv")

#data cleaning
#restructure table
re.gr.temp <- melt(gr_temp, id="Year")
#reformat months using reference table
gr.tempF <- re.gr.temp
gr.tempF$month <- month.table$number[match(unlist(re.gr.temp$variable), month.table$name)]

#combine month and year into a single column
gr.tempF$Date <- ym(paste0(gr.tempF$Year,"-",gr.tempF$month))
gr.tempF$monthYear <- year(gr.tempF$Date)+((month(gr.tempF$Date)-1)/12)

#re-format trace, na, missing values 
gr.tempF$avg.temp <- as.numeric(ifelse(re.gr.temp$value=="T",0, re.gr.temp$value))

#remove rows that don't correspond to months
gr.tempF <- gr.tempF %>%
  filter(!variable=="Annual")

#arrange rows in ascending order
gr.tempF <- gr.tempF %>%
  arrange(monthYear)


#create df without NA values
gr.temp.no.na <- gr.tempF <- gr.tempF %>%
  filter(is.na(avg.temp)==FALSE)

#set up ar model
#time series and decompositions
#temp time series: grand rapids 1915-present
gr_temp_ts <- ts(gr.temp.no.na$avg.temp,
                 start = c(1915, 8),
                 frequency = 12)
#decomposition: 1915-present
gr.temp.dec.1915 <- decompose(na.omit(gr_temp_ts))
plot(gr.temp.dec.1915)

#more specific time frames
#subset data from 2020-present
gr_temp2020 <- gr.tempF %>% 
  filter(Year>="2020") %>%
  arrange(monthYear)

#create time series
gr_temp_ts2020 <- ts(gr_temp2020$avg.temp,
                     start = c(2020, 1),
                     frequency = 12)
plot(gr_temp_ts2020)
#decompose new time series
gr_temp_dec2020 <- decompose(na.omit(gr_temp_ts2020))
plot(gr_temp_dec2020)

#subset data from 2000-present
gr_temp2000 <- gr.tempF %>% 
  filter(Year>="2000") %>%
  arrange(monthYear)

#create time series
gr_temp_ts2000 <- ts(gr_temp2000$avg.temp,
                     start = c(2000, 9),
                     frequency = 12)
plot(gr_temp_ts2000)
#decompose new time series
gr_temp_dec2000 <- decompose(na.omit(gr_temp_ts2000))
plot(gr_temp_dec2000)

#autocorrelation: 2020-present
acf(na.omit(gr_temp_ts2000),
    lag.max = 24)
#partial acf
pacf.plot <- pacf(na.omit(gr_temp_ts2000))

#arima
gr_temp_y2000 <- na.omit(gr_temp_ts2000)
gr.temp.model1 <- arima(gr_temp_y2000,
                    order = c(1,0,0))
gr.temp.model3 <- arima(gr_temp_y2000,
                        order = c(3,0,0))
gr.temp.model4 <- arima(gr_temp_y2000,
                        order = c(4,0,0))
#calculate fit
gr.temp.ar3 <- gr_temp_y2000 - residuals(gr.temp.model3)
gr.temp.ar4 <- gr_temp_y2000 - residuals(gr.temp.model4)

#plot data
plot(gr_temp_y2000)

#plot fit
points(gr.temp.ar3, type="l", col = "tomato3", lty=2, lwd=2)
points(gr.temp.ar4, type="l", col = "green3", lty=2, lwd=2)
legend("topleft", c("data","AR3","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","green3"),
       bty="n")

#forecast future data
fut.gr.temp <- forecast(gr.temp.model4)
fut.gr.temp

#set up data frame
#make new df
fut.gr.tempF <- data.frame(fut.gr.temp)
#set up dates for df
gr.yearT <- c(rep(2024,1),rep(2025, 12), rep(2026,11))
gr.monthT <- c(12, seq(1,12), seq(1,11))
fut.gr.tempF$Date <- ymd(paste(gr.yearT,"/",gr.monthT,"/",1))

#make a plot with historical data and forecast
ggplot() +
  geom_line(data = gr_temp2000, aes(x = ymd(Date), y = avg.temp))+
  xlim(ymd(gr_temp2000$Date[1]),fut.gr.tempF$Date[24])+ 
  geom_line(data = fut.gr.tempF, aes(x = Date, y = Point.Forecast),
            col="red") +  
  geom_ribbon(data=fut.gr.tempF, 
              aes(x=Date,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="Year", y="Average Temperature (F)")+
  ggtitle("2 Year Forecast of Temperature in Grand Rapids")

#project part 2: visualize and analyze trends in historical data
#this is done to better understand patterns that lead to forecasts

#visualization 1: trend in snowfall data from the twin cities

ggplot(data=new.snow, aes(x = ymd(Date), y=snow.in))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  labs(x="Year", y="Snowfall (inches)")

#run regression 
model1 <- lm(snow.in ~ Date, data = new.snow)
summary(model1)
#find slope of trend line
slope1 <- model1$coefficients[["Date"]] 
slope1


#visualization 2: trend in temperature data from the twin cities

ggplot(data=new.temp, aes(x = ymd(Date), y=avg_temp))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  labs(x="Year", y="Average Temperature (F)")

#run regression
model2 <- lm(avg_temp ~ Date, data = new.temp)
summary(model2)
#find slope of trend line
slope2 <- model2$coefficients[["Date"]] 
slope2

#visualization 3: trend in snowfall data from grand rapids

ggplot(data=gr_snowF, aes(x = ymd(Date), y=snow.in))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  labs(x="Year", y="Snowfall (inches)")

#run regression
model3 <- lm(snow.in ~ Date, data = gr_snowF)
summary(model3)
#find slope of trend line
slope3 <- model3$coefficients[["Date"]] 
slope3


#visualization 4: trend in temperature data from grand rapids

ggplot(data=gr.tempF, aes(x = ymd(Date), y=avg.temp))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  labs(x="Year", y="Average Temperature (F)")

#run regression 
model4 <- lm(avg.temp ~ Date, data = gr.tempF)
summary(model4)
#find slope of trend line
slope4 <- model4$coefficients[["Date"]] 
slope4

#look at change in range of snowfall and temp over time
#snowfall range: minneapolis
#find max snowfall for each year
max_snow <- new.snow %>%
  group_by(trueyear) %>%
  summarise(max_snow_in=max(snow.in, na.rm=TRUE)) %>%
  filter (!trueyear=="1883")
#assume that min snowfall for each year is zero
#visualize change in range
ggplot(data=max_snow, aes(x = trueyear, y=max_snow_in))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  labs(x="Year", y= "Annual Maximum Snowfall (inches)")+
  ggtitle("Annual Range in Snowfall in Minneapolis Increases Over Time")
#run regression, find slope
snow.range <- lm(max_snow_in ~ trueyear, data = max_snow)
summary(snow.range)
slope.snow.max <- snow.range$coefficients[["trueyear"]]

#snowfall range: grand rapids
max_snow_gr <- gr_snowF %>%
  group_by(trueyear) %>%
  summarise(max_snow_in = (max(snow.in, na.rm=TRUE))) %>%
  filter (!trueyear=="1947")
#assume that min snowfall for each year is zero
#visualize change in range
ggplot(data=max_snow_gr, aes(x = trueyear, y=max_snow_in))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  labs(x="Year", y= "Annual Maximum Snowfall (inches)")+
  ggtitle("Annual Range in Snowfall in Grand Rapids Consistent Over Time")

#run regression, find slope
gr.snow.range <- lm(max_snow_in ~ trueyear, data = max_snow_gr)
summary(gr.snow.range)
slope.gr.max <- gr.snow.range$coefficients[["trueyear"]]

#find frequency of no snow

#minneapolis 
no.snow <- new.snow %>%
  filter(snow.in == 0) %>%
  arrange(monthYear)

no.snow.tot <- no.snow %>%
  group_by(trueyear) %>%
  summarise(num_months = sum(snow.in == 0, na.rm = TRUE))

#visualize
ggplot(data = no.snow.tot, aes(x = trueyear, y = num_months)) + 
  geom_bar(stat = "identity", color="royalblue4") + 
  ggtitle("Frequency of Months Without Snow in Minneapolis")+
  theme_classic()+
  labs(x="Year", y="Number of Months With No Snow")

#grand rapids
gr.no.snow <- gr_snowF %>%
  filter(snow.in == 0) %>%
  arrange(monthYear)

gr.no.snow.tot <- gr.no.snow %>%
  group_by(trueyear) %>%
  summarise(num_months = sum(snow.in == 0, na.rm = TRUE))

#visualize
ggplot(data = gr.no.snow.tot, aes(x = trueyear, y = num_months)) + 
  geom_bar(stat = "identity", color="royalblue4") + 
  ggtitle("Frequency of Months Without Snow in Grand Rapids")+
  theme_classic()+
  labs(x="Year", y="Number of Months With No Snow")

#regressions
#minneapolis 
reg.no.snow <- lm(num_months ~ trueyear, data = no.snow.tot)
summary(reg.no.snow)

#grand rapids
reg.gr.no <- lm(num_months ~ trueyear, data = gr.no.snow.tot)
summary(reg.gr.no)
