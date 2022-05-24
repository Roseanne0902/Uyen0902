# Introduction to Data Science UG

# Uyen Nguyen

# U3206201

# Assignment 1_ Data Wrangling and exploration

#Set directory

setwd("C:/Users/User/Desktop/ITDS")
dir()
knitr::stitch('U3206201_Assignment_1.r')


###############################PART-A###########################################

rm(list=ls())
install.packages('tidyverse')
install.packages("dplyr")
library(tidyverse)
library(lubridate)
library(dplyr)
library(dbplyr)
library('plyr') 

csv_files1 <- c("~/data/201808.csv","~/data/201809.csv","~/data/201810.csv","~/data/201811.csv",
               "~/data/201812.csv","~/data/201901.csv","~/data/201902.csv","~/data/201903.csv",
               "~/data/201904.csv","~/data/201905.csv","~/data/201906.csv","~/data/201907.csv",
               "~/data/201908.csv", "~/data/201909.csv","~/data/201910.csv","~/data/201911.csv","~/data/201912.csv",
               "~/data/202001.csv","~/data/202002.csv")


weather_canberra_table1 <- NULL;

for (i in 1:length(csv_files1)){
  
  data1 <- read_csv(csv_files1[i], skip = 7)
  
  weather_canberra_table1 <- rbind(weather_canberra_table1,data1)
  
}
weather_canberra_table1

csv_files2 <- c("~/data/202007.csv","~/data/202008.csv","~/data/202009.csv","~/data/202010.csv",
                "~/data/202011.csv","~/data/202012.csv","~/data/202101.csv","~/data/202102.csv","~/data/202103.csv",
               "~/data/202104.csv","~/data/202105.csv","~/data/202106.csv","~/data/202107.csv","~/data/202108.csv")


#defining our main dataframe where all the data will be concatenated
weather_canberra_table2 <- NULL;

#reading the csv files one by one and appending the read data to the main.data.frame
for (i in 1:length(csv_files2)){
  
  #variable to temporarily hold the data extracted from the csv file.
  data2 <- read_csv(csv_files2[i], skip = 7)
  
  #concanating the data from current csv file to the main data frame
  weather_canberra_table2 <- rbind.fill(weather_canberra_table2, data2)
  
}

weather_canberra_table2$Date
weather_canberra_table2$Date<-format(as.Date(weather_canberra_table2$Date, '%m/%d/%Y'), "%d/%m/%Y")
weather_canberra_table2$Date

weather_table <- rbind.fill(weather_canberra_table1,weather_canberra_table2)
weather_table
###############################PART-B###########################################


# Q1: Replace cell contains calm values with NAs

weather_table[weather_table=="Calm"]<- NA

# Q2: remove the variables, which have no data at all (NAs)

weather_table <- weather_table[ , colSums(is.na(weather_table)) < nrow(weather_table)]  # Remove rows with NA only

weather_table_row_count <- nrow(weather_table)

# Q3: Drop the variables, where NAs > 90% of the number of records in these variables)

weather_table <- weather_table[ , colSums(is.na(weather_table)) < (0.9 * weather_table_row_count)]

# Or:

weather_table_new <- weather_table[ , colSums(is.na(weather_table)) < (0.9 * nrow(weather_table))]

# Q4: Change the column name to have no spaces between the words, with "_" character

names(weather_table) <- gsub(" ", "_", names(weather_table))

write.csv(weather_table, "~/data/weather_table.csv", row.names = TRUE)

# Q5: Change Date columns to Date data type

weather_table$Date = as.Date(weather_table$Date, "%d/%m/%Y")

# Q6: Add 2 new columns for months and years throughout 33 files

library(lubridate)

weather_table <- weather_table %>% 
  
  mutate(month= month(Date), year=year(Date))



# And: 

weather_table <- add_column(weather_table, month_char=month(weather_table$Date), year_char=year(weather_table$Date))

# Q7: Change the type of 'Month' and 'Year' from character to Ordinal with levels as the # of month in a year and number of years

weather_table$month <- as.factor(weather_table$month)

weather_table$year <- factor(weather_table$year, levels = sort(unique(weather_table$year)), labels = seq_along(sort(unique(weather_table$year))))


# Q8: As for the numeric values, replace the NAs values by the median of the equivalent columns, if exists

numeric_column_in_weather_table <- weather_table[,unlist(lapply(weather_table, is.numeric))]

numeric_column_na <- numeric_column_in_weather_table[,colSums(is.na(numeric_column_in_weather_table))>0]

if(ncol(numeric_column_na) > 0){
  
  median_na <- apply(numeric_column_na, 2, FUN = function(x){
    
    median(x,na.rm = TRUE)
  })
  
for( i in 1:length(numeric_column_na)){
    indx <- is.na(numeric_column_na[,i])
    numeric_column_na[indx, i] <-  median_na[i]
    weather_table[names(numeric_column_na[i])] <- numeric_column_na[i]
  }
  
}

###############################PART-C###########################################

#Q1: Summary of minimum tem, 9am tem, speed of maximum wind gust km/h


summaryOfcolumn <- c('Minimum_temperature','9am_Temperature',"Speed_of_maximum_wind_gust_(km/h)")

print("Summary of Minimum_temperature, 9am_Temperature and Speed_of_maximum_wind_gust_(km/h)")
for (i in 1:length(summaryOfcolumn)){
  print(summary(weather_table[summaryOfcolumn[i]]))
}


#Q2: Min average temperature by month and year

average_min_tem_ordinal <- weather_table %>% 
  group_by(month, year) %>% 
  dplyr::summarise(month= month.abb[first(month)], num_days=n(), ave_month= mean(Minimum_temperature))

average_min_tem_ordinal

average_min_tem_month_ordinal <- weather_table %>% 
  group_by(month) %>% 
  dplyr::summarise(month= month.abb[first(month)], num_days=n(), ave_month= mean(Minimum_temperature))

average_min_tem_month_ordinal

average_min_tem_year_ordinal <- weather_table %>% 
  group_by(year) %>% 
  dplyr::summarise(num_days=n(), ave_year= mean(Minimum_temperature))

average_min_tem_year_ordinal

####

ave_temp_by_month_char <- weather_table %>% 
  group_by(month_char) %>% 
  dplyr::summarise(num_days=n(), mean = mean(Minimum_temperature))

ave_temp_by_month_char

average_min_tem_year <- weather_table %>% 
  group_by(year_char) %>% 
  dplyr::summarise(num_days=n(), ave_year= mean(Minimum_temperature))

average_min_tem_year

####

average_min_tem_char <- weather_table %>% 
  group_by(year_char, month_char) %>% 
  dplyr::summarise(num_days=n(), ave_year_month_char= mean(Minimum_temperature))

average_min_tem_char


#Q3: Extracting the average of speed of maximum wind gust by direction of maximum wind gust
detach(package:plyr)

weather_table$`Speed_of_maximum_wind_gust_(km/h)`

average_speed_max_wind_by_direction<- weather_table %>% 
  group_by(`Direction_of_maximum_wind_gust`) %>% 
  summarise(mean_speed_of_each_directions = mean(`Speed_of_maximum_wind_gust_(km/h)`))

average_speed_max_wind_by_direction

#Q4: Which month was dry, if any, (no rainfall at all),and in which year?
dry_month <- weather_table %>% 
  group_by(month_char, year_char) %>% 
  dplyr::summarise(count=n(), rainfall_sum=sum(`Rainfall_(mm)`))
  

dry_month

# For the character months: 


num=0
indx=NULL

for (i in 1:length(dry_month$rainfall_sum)){
  if(dry_month$rainfall_sum[i] == 0){
    num = num + 1
    indx[num] <-  i
  }
}

num

if (num > 0){
  print('These are the driest months: ')
  for(i in 1:length(num)){
    print(month_char.abb[indx[i]], year[indx[i]])
  }
}else{
  print('There are no dry month with 0 rainfall')
}

# For the ordinal months:

dry_month_1 <- weather_table %>% 
  group_by(month, year) %>% 
  summarise(month= month.abb[first(month)],count=n(), rainfall_sum=sum(`Rainfall_(mm)`),
            mean_rain=mean(`Rainfall_(mm)`))


dry_month_1

Ave_rain <- dry_month_1 %>% 
  group_by(month) %>% 
  summarise(rainfall_sum_ave=sum(mean_rain))
Ave_rain

num_1=0
indx_1=NULL

for (i in 1:length(dry_month_1$rainfall_sum)){
  if(dry_month_1$rainfall_sum[i] == 0){
    num_1 = num_1 + 1
    indx_1[num] <-  i
  }
}

num_1

if (num_1 > 0){
  print('These are the driest months: ')
  for(i in 1:length(num_1)){
    print(month.abb[indx_1[i]], year[indx[i]])
  }
}else{
  print('There are no dry month with 0 rainfall')
}

# Or using any for working out if there is any months equal to 0 rainfall:

any(dry_month_1$rainfall_sum %in% c(0))
dry_month_1$rainfall_sum
dry_month$rainfall_sum

# Or

Rainfall_month <- dry_month_1 %>% 
  filter(rainfall_sum == 0) %>% 
  group_by(month, year) 
Rainfall_month


#Q5: Whats about the humidity, which month in the ACT has the highest humidity level in 2019?

# using char Date

highest_char_month_2019 <- weather_table %>% 
  filter(year_char %in% c(2019)) %>% 
  group_by(month_char) %>% 
  summarise( count=n(),`average_humidity(%)` = (mean(`9am_relative_humidity_(%)`) + mean(`3pm_relative_humidity_(%)`))/2) %>% 
  summarise(`2019_highest_humidity_char_month` = month_char[which.max(`average_humidity(%)`)])

highest_char_month_2019

# using Ordinal Date

highest_month_2019 <- weather_table %>% 
  filter(year %in% c(2)) %>% 
  group_by(month) %>% 
  summarise(month= month.abb[first(month)], count=n(),`average_humidity(%)` = (mean(`9am_relative_humidity_(%)`) + mean(`3pm_relative_humidity_(%)`))/2) %>% 
  summarise("2019_highest_humidity_month" = month.abb[which.max(`average_humidity(%)`)])

highest_month_2019

#Q6: For 2019, extract the minimum, maximum and average temperature, wind speed and humidity per month and per quarter in 2019 only
#Q7: Plot the histograms/bar charts for each variable of the previous question

# Per month 
# Temperature
monthly_summary_tem_2019 <- weather_table %>% 
  filter(year %in% c(2)) %>% 
  group_by(month) %>% 
  summarise( month= month.abb[first(month)], count=n(), minimum_tem= min(`Minimum_temperature`), maximum_tem= max(`Maximum_temperature`),
             mean_min_tem= mean(`Minimum_temperature`), mean_max_tem=mean(`Maximum_temperature`),
             average_temperature = (mean(`9am_Temperature`) + mean(`3pm_Temperature`))/2) 

monthly_summary_tem_2019

ggplot(data= monthly_summary_tem_2019, aes(x= month, y= minimum_tem, fill=month)) + 
  geom_bar(stat="identity")
ggplot(data= monthly_summary_tem_2019, aes(x= month, y= mean_min_tem, fill=month)) + 
  geom_bar(stat="identity")
ggplot(data= monthly_summary_tem_2019, aes(x= month, y= maximum_tem, fill=month)) + 
  geom_bar(stat="identity")
ggplot(data= monthly_summary_tem_2019, aes(x= month, y= mean_max_tem, fill=month)) + 
  geom_bar(stat="identity")
ggplot(data= monthly_summary_tem_2019, aes(x= month, y= average_temperature, fill=month)) + 
  geom_bar(stat="identity")

Max_min_tem <- monthly_summary_tem_2019 %>% 
  summarise( `2019_strongest_tem` = month.abb[which.max(`maximum_tem`)], 
             `2019_lightest_tem` = month.abb[which.min(`minimum_tem`)],
             `2019_strongest_min_tem` = month.abb[which.max(`average_temperature`)], 
             `2019_lightest_max_tem` = month.abb[which.min(`average_temperature`)])

Max_min_tem

# Wind speed

monthly_summary_wind_2019 <- weather_table %>% 
  mutate(`9am_wind_speed_(km/h)`= as.numeric(as.character(`9am_wind_speed_(km/h)`))) %>%
  mutate(`3pm_wind_speed_(km/h)` = as.numeric(as.character(`3pm_wind_speed_(km/h)`))) %>%
  filter(year %in% c(2)) %>%
  group_by(month) %>% 
  summarise(month= month.abb[first(month)], count=n(),
            max_wind_gust=mean(`Speed_of_maximum_wind_gust_(km/h)`), 
            mean_wind_speed= (mean(`9am_wind_speed_(km/h)`,na.rm = TRUE) + mean(`3pm_wind_speed_(km/h)`, na.rm = TRUE))/2)
monthly_summary_wind_2019

ggplot(data= monthly_summary_wind_2019, aes(x= month, y= max_wind_gust, fill=month)) + 
  geom_bar(stat="identity")
ggplot(data= monthly_summary_wind_2019, aes(x= month, y= mean_wind_speed, fill=month)) + 
  geom_bar(stat="identity")

knot_speed <- monthly_summary_wind_2019 %>% 
  group_by(month) %>% 
  summarise(knot_mean= (mean_wind_speed*1.852), knot_gust= (max_wind_gust*1.852))

knot_speed
  

Max_min_wind <- monthly_summary_wind_2019 %>% 
  summarise( wind_gust_max = month.abb[which.max(`max_wind_gust`)],
            `2019_strongest_wind` = month.abb[which.max(`mean_wind_speed`)], 
            `2019_lightest_wind` = month.abb[which.min(`mean_wind_speed`)])

# humidity

monthly_summary_humidity_2019 <- weather_table %>% 
  filter(year %in% c(2)) %>%
  group_by(month) %>% 
  summarise(month= month.abb[first(month)], count=n(),
            `average_humidity(%)` = (mean(`9am_relative_humidity_(%)`) + mean(`3pm_relative_humidity_(%)`))/2) 

ggplot(data= monthly_summary_humidity_2019, aes(x= month, y= `average_humidity(%)`, fill=month)) + 
  geom_bar(stat="identity")

monthly_summary_humidity_2019

max_min_humidity<- monthly_summary_humidity_2019 %>% 
  summarise(`2019_highest_humidity_month` = month.abb[which.max(`average_humidity(%)`)],
            `2019_lowest_humidity_month` = month.abb[which.min(`average_humidity(%)`)])

max_min_humidity

# Per Quarter
#(1,2,3),(4,5,6),(7,8,9),(10,11,12)

library(magrittr) 
library(dplyr)    
library(ggplot2)

Q <- weather_table %>%
  
  mutate(qtr = quarter(Date, with_year = T))
Q

# Temperature

quarterly_summary_tem_2019 <- Q %>% 
  filter(year %in% c(2))%>% 
  group_by(qtr)%>% 
  summarise( count=n(), minimum_Q_tem= min(`Minimum_temperature`), maximum_Q_tem= max(`Maximum_temperature`),
             mean_min_Q_tem= mean(`Minimum_temperature`), mean_max_Q_tem=mean(`Maximum_temperature`),
             average_Q_temperature = (mean(`9am_Temperature`) + mean(`3pm_Temperature`))/2) 
 
quarterly_summary_tem_2019

ggplot(data= quarterly_summary_tem_2019, aes(x= qtr, y= minimum_Q_tem, fill=qtr)) + 
  geom_bar(stat="identity")
ggplot(data= quarterly_summary_tem_2019, aes(x= qtr, y= mean_min_Q_tem, fill=qtr)) + 
  geom_bar(stat="identity")
ggplot(data= quarterly_summary_tem_2019, aes(x= qtr, y= maximum_Q_tem, fill=qtr)) + 
  geom_bar(stat="identity")
ggplot(data= quarterly_summary_tem_2019, aes(x= qtr, y= mean_max_Q_tem, fill=qtr)) + 
  geom_bar(stat="identity")
ggplot(data= quarterly_summary_tem_2019, aes(x= qtr, y= average_Q_temperature, fill=qtr)) + 
  geom_bar(stat="identity")

Max_min_Q_tem <- quarterly_summary_tem_2019 %>% 
  summarise( `2019_Q_strongest_tem` = qtr[which.max(`maximum_Q_tem`)], 
             `2019_Q_lightest_tem` = qtr[which.min(`minimum_Q_tem`)],
             `2019_Q_strongest_min_tem` = qtr[which.max(`average_Q_temperature`)], 
             `2019_Q_lightest_max_tem` = qtr[which.min(`average_Q_temperature`)])

Max_min_Q_tem


# Wind speed

quarterly_summary_wind_2019 <- Q %>% 
  mutate(`9am_wind_speed_(km/h)`= as.numeric(as.character(`9am_wind_speed_(km/h)`))) %>%
  mutate(`3pm_wind_speed_(km/h)` = as.numeric(as.character(`3pm_wind_speed_(km/h)`))) %>%
  filter(year %in% c(2)) %>%
  group_by(qtr) %>% 
  summarise(count=n(),
            max_Q_wind_gust= mean(`Speed_of_maximum_wind_gust_(km/h)`), 
            mean_Q_wind_speed= (mean(`9am_wind_speed_(km/h)`,na.rm = TRUE) + mean(`3pm_wind_speed_(km/h)`, na.rm = TRUE))/2)

quarterly_summary_wind_2019

ggplot(data= quarterly_summary_wind_2019, aes(x= qtr, y= max_Q_wind_gust, fill=qtr)) + 
  geom_bar(stat="identity")

ggplot(data= quarterly_summary_wind_2019, aes(x= qtr, y= mean_Q_wind_speed, fill=qtr)) + 
  geom_bar(stat="identity")


Max_min_Q_wind <- quarterly_summary_wind_2019 %>% 
  summarise( wind_gust_Q_max = qtr[which.max(`max_Q_wind_gust`)],
             `2019_strongest_Q_wind` = qtr[which.max(`mean_Q_wind_speed`)], 
             `2019_lightest_Q_wind` = qtr[which.min(`mean_Q_wind_speed`)])

Max_min_Q_wind

# humidity

quarterly_summary_humidity_2019 <- Q %>% 
  filter(year %in% c(2)) %>%
  group_by(qtr) %>% 
  summarise(count=n(),
            `average_Q_humidity(%)` = (mean(`9am_relative_humidity_(%)`) + mean(`3pm_relative_humidity_(%)`))/2) 


quarterly_summary_humidity_2019

ggplot(data= quarterly_summary_humidity_2019, aes(x= qtr, y=`average_Q_humidity(%)`, fill=qtr)) + 
  geom_bar(stat="identity")

max_min_Q_humidity<- quarterly_summary_humidity_2019 %>% 
  summarise(`2019_highest_humidity_qtr` = qtr[which.max(`average_Q_humidity(%)`)],
            `2019_lowest_humidity_qtr` = qtr[which.min(`average_Q_humidity(%)`)])

max_min_Q_humidity

###############################PART-D###########################################

# Insights

# Data can be used for drawing meaningful information out of a specific set of dataset. As for the weather data which is given above, there can be several questions which needed to be answered by the data information. The first question would be:
# 1.	 “Based on the given data, which would be the ideal times for watering, preventing disease and mold, keeping the land moisture at the right level in agriculture?  (Sigfox.com, 2018)”
# Insights from the data: Figure Ave_rain
# Depending on the average rain sum for each month as above, we can pinpoint that the months which have the large amounts of rainfall (mm) would be August, February, March, November and October, which means the farmers and agricultural professionals
# do not have to water much in those months to keep the land protected from diseases and moist and drowning the crops. On the other hand, it is indispensable that the farmers should focus on providing sufficient water amount for the crops as well as the land in April, July, May and September. In the other months, the amount of water can be adjusted according to the statistics in the Ave_rain table. 
# 2.	“According to the data analysis, when can we expect the strong wind, Gale force, Storm force, Hurricane force warning issued (www.bom.gov.au, n.d.)?”
# Figure Knot_speed
# This table is transferred from the wind speed table, by using 1 international knot= 1.852 kilometers per hour (Wikipedia, 2021). 
# Figure 3 (www.bom.gov.au, n.d.)
# According to the given information, we can expect strong wind warning issued in February, January, June, July, March, May. As for the Gale force, they are expected to happen in August, December, November, October, September. However, there are some days in every months which could result in extremely strong Hurricanes over 100 knots, storm forces over 67 knots, as indicated in the knot speed table. 
# References:
# Sigfox.com. (2018). How Rainfall Affects Crop Health. [online] Available at: https://www.sigfox.com/en/news/how-rainfall-affects-crop-health.
# Wikipedia. (2021). Knot (speed). [online] Available at: https://simple.wikipedia.org/wiki/Knot_(speed) [Accessed 3 Oct. 2021].
# www.bom.gov.au. (n.d.). Wind - Reference material - Marine Knowledge Centre. [online] Available at: http://www.bom.gov.au/marine/knowledge-centre/reference/wind.shtml.















