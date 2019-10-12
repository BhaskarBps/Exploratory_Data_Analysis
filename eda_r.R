
##################  EXPLORATORY DATA ANALYSIS  ###################

install.packages("dplyr")
library(dplyr)
chicago<-readRDS("C:\\Users\\Puneeth\\Downloads\\chicago_data\\chicago.rds")
fix(chicago)
dim(chicago)
str(chicago)
names(chicago)
subset <- select(chicago, ends_with("2"))
str(subset)
subset <- select(chicago, starts_with("d"))
str(subset)
chic.f <- filter(chicago, pm25tmean2 > 30)
str(chic.f)
fix(chic.f)
summary(chic.f$pm25tmean2)
chic.f <- filter(chicago, pm25tmean2 > 30 & tmpd > 80)
select(chic.f, date, tmpd, pm25tmean2)
chicago <- arrange(chicago, date)
fix(chicago)
chicago <- arrange(chicago, desc(date))
fix(chicago)
chicago <- rename(chicago, dewpoint = dptp, pm25 = pm25tmean2)
head(chicago[, 1:5], 3)
chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
head(chicago)
chicago <- mutate(chicago, year = as.POSIXlt(date)$year + 1900)
years <- group_by(chicago, year)
summarize(years, pm25 = mean(pm25, na.rm = TRUE), 
                     o3 = max(o3tmean2, na.rm = TRUE), 
                     no2 = median(no2tmean2, na.rm = TRUE))

qq <- quantile(chicago$pm25, seq(0, 1, 0.2), na.rm = TRUE)
chicago <- mutate(chicago, pm25.quint = cut(pm25, qq))
quint <- group_by(chicago, pm25.quint)
summarize(quint, o3 = mean(o3tmean2, na.rm = TRUE), 
                     no2 = mean(no2tmean2, na.rm = TRUE))

library(readr)
ozone <- read_csv("D:\\DOWNLOADS\\hourly_44201_2014.csv",col_types="ccccinnccccccncncccccc")
head(ozone)
names(ozone) <- make.names(names(ozone))
nrow(ozone)
ncol(ozone)
str(ozone)
head(ozone[, c(6:7, 10)])
tail(ozone[, c(6:7, 10)])
table(ozone$Time.Local)

library(dplyr)
filter(ozone, Time.Local == "13:00") %>% 
           select(State.Name, County.Name, Date.Local, 
                                   Time.Local, Sample.Measurement)

select(ozone, State.Name) %>% unique %>% nrow
unique(ozone$State.Name)
summary(ozone$Sample.Measurement)


ranking <- group_by(ozone, State.Name, County.Name) %>%
           summarize(ozone = mean(Sample.Measurement)) %>%
           as.data.frame %>%
           arrange(desc(ozone))
head(ranking, 10)
tail(ranking, 10)
