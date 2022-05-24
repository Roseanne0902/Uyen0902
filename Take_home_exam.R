# Introduction to Data Science UG

# Uyen Nguyen

# U3206201

# Assignment 2_ Take home Exam

#Set directory

setwd("C:/Users/User/Desktop/ITDS")
dir()
knitr::stitch('Assignment2_TakeHomeExam')

####################################### PART-B ##################################

#------------------------TASK-1: DATA PREPARATION AND WRANGLING-----------------#


rm(list=ls())
install.packages('tidyverse')
install.packages("dplyr")
test <- require(tidyverse)

library(modelr)
library(broom)
library(tidypredict)
library(dplyr)
library(tidyverse)

# Task 1: Data Preparation and Wrangling: (20 marks)
# 1. Load and read the data from the CSV files and store them into dataframes named
# appropriately.

countries <- read_csv("data/Countries.csv")

covid19 <- read_csv("data/Covid19.csv")

recovered <- read_csv("data/Recovered.csv")

tests <- read_csv("data/Tests.csv")

# 2. Tidy up the dataframe driven from the file “Recovered.csv” to be compatible with the
# dataframe driven from the file “Covid19.csv”, i.e. every observation should have a
# record of recovered patients in one country in a single day.

recovered <- recovered %>% 
  gather(date, recovered, -Country.Region)

recovered <- recovered %>% 
  arrange(Country.Region)

recovered

# 3. Change the column names in the dataframes were loaded from the following files
# accordingly.
# File Name
# Ordered New Column Names
# Covid19.csv Code, Country, Continent, Date, NewCases,
# NewDeaths
# Tests.csv Code, Date, NewTests
# Countries.csv Code, Country, Population, GDP, GDPCapita
# Recovered.csv Country, Date, Recovered


names(covid19) <- c('Code', 'Country', 'Continent', 'Date', 'NewCases', 'NewDeaths')

names(tests) <- c('Code', 'Date', 'NewTests')

names(countries) <- c('Code', 'Country', 'Population', 'GDP', 'GDPCapita')

names(recovered) <- c('Country', 'Date', 'Recovered')

# 4. Ensure that all dates variables are of date data type and with the same format across the
# dataframes.
# 5. Considering the master dataframe is the one loaded from file “Covid19.csv”, add new 5
# variables to it from other files (Recovered.csv, Tests.csv, Countries.csv). The 5 new
# added variables should be named (“Recovered”, “NewTests”, “Population”, “GDP”,
#                                  “GDPCapita”) accordingly.
# [Hint: you can use the merge function to facilitate the alignment of the data in the different
#  dataframes.]

covid19 <- covid19 %>% 
  arrange(Code)

covid19 <- merge(x=covid19, y=tests, by=c("Code","Date"), all.x = TRUE)


str(recovered)

recovered$Date <- as.Date(recovered$Date, "%Y.%m.%d")

str(recovered$Date)


covid19 <- merge(x=covid19, y=recovered, by=c("Country", "Date"), all.x=TRUE)



covid19 <- merge(x=covid19, y=countries, by=c("Code", "Country"), all.x = TRUE)


# 6. Check for Nas in all dataframes and change them to Zero.

is.na(covid19)

covid19$NewTests[is.na(covid19$NewTests)]<-0

covid19$Recovered[is.na(covid19$Recovered)]<-0


  

# 7. Using existing “Date” variable; add month and week variables to the master dataframe.
# [Hint: you may use functions from lubridate package]
# [Hint: To ensure that this task has been finished correctly, when you run head(covid19_data), you
#  should get results such as in the below image]


library(lubridate)

covid19 <- covid19 %>% 
  
  mutate(month= month(Date), week=week(Date))

head(covid19)

#------------------------TASK-2: EXPLORATORY DATA ANALYSIS----------------------#

# 1. Add four new variables to the master dataframe (“CumCases”, “CumDeaths”,
# “CumRecovered”, “CumTests”) These variables should reflect the cumulative relevant
# data up to the date of the observation, i.e CumCases for country “X” at Date “Y” should
# reflect the total number of cases in country “X” since the beginning of recording data till
# the date “Y”.
# [Hint: first arrange by date and country, then for each new variable to be added you need to
#  group by country and mutate the new column using the cumsum function]

covid19_cum <- covid19

covid19_cum <- covid19_cum %>% 
  arrange(Date, Country) %>% 
  group_by(Country) %>% 
  mutate(CumCases=cumsum(NewCases), CumDeaths=cumsum(NewDeaths), 
         CumRecovered=cumsum(Recovered), CumTests=cumsum(NewTests))

covid19_cum

# 2. Add two new variables to the master dataframe (“Active”, “FatalityRate”). Active
# variable should reflect the infected cases that has not been closed yet (by either recovery
# or death), and it could be calculated from (CumCases – (CumDeaths + CumRecovered)).
# On the other hand, FatalityRate variable should reflect the percentages of death to the
# infected cases up to date and it could be calculated from (CumDeaths / CumCases).

library("dplyr")

covid19_cum <- covid19_cum %>% 
  arrange(Date, Country) %>% 
  group_by(Country) %>% 
  mutate(Active=(CumCases - (CumDeaths + CumRecovered)), FatalityRate=(CumDeaths / CumCases))

covid19_cum

# 3. Add four new variables to the master dataframe (“Cases_1M_Pop”, “Deaths_1M_Pop”,
# “Recovered_1M_Pop”, “Tests_1M_Pop”) These variables should reflect the cumulative
# relevant rate per one million of the corresponding country population, (i.e Cases_1M_Pop
# for country “X” at Date “Y” should reflect the total number of new cases up to date “Y”
# per million people of country “X” population)
# [Hint: Cases_1M_Pop = CumCases*(10^6) / Population)]


covid19_cum <- covid19_cum %>% 
  arrange(Date, Country) %>% 
  group_by(Country) %>% 
  mutate(Cases_1M_Pop=(CumCases*(10^6) / Population), 
         Deaths_1M_Pop=(CumDeaths*(10^6) / Population),
         Recovered_1M_Pop=(CumRecovered*(10^6) / Population),
         Tests_1M_Pop=(CumTests*(10^6) / Population))

covid19_cum

# 4. Find the day with the highest reported death toll across the world. Print the date and the
# death toll of that day.

covid19_Max_cul_Deaths_toll <- covid19_cum %>% 
  group_by(Date) %>% 
  summarise(cul_deaths_toll_per_date=sum(CumDeaths)) %>% 
  summarise(Date = Date[which.max(cul_deaths_toll_per_date)], 
            max_cul_deaths_toll=max(cul_deaths_toll_per_date))

covid19_Max_cul_Deaths_toll

# This is for calculating the highest deaths record date across the world

covid19_Max_Deaths_toll <- covid19_cum %>% 
  group_by(Date) %>% 
  summarise(deaths_toll_per_date=sum(NewDeaths)) %>% 
  summarise(Date = Date[which.max(deaths_toll_per_date)], 
            max_deaths_toll_per_day=max(deaths_toll_per_date))

covid19_Max_Deaths_toll



# 5. Build a graph to show how the cumulative data of (Infected Cases, Deaths, Recovered,
# Tests) change over the time for the whole world collectively.
# [Hint: Use geom_line, use log for Y axis for better presentation, Use different colour to
#  distinguish between new cases, deaths, and recovered]




max_cum_by_country_month <- covid19_cum %>% 
  group_by(month, Country) %>% 
  summarise(highest_cumcase=max(CumCases), highest_cumdeaths=max(CumDeaths),
            highest_cumrecovered=max(CumRecovered), highest_cumtest=max(CumTests))

max_cum_by_country_month



covid_graph <- max_cum_by_country_month %>% 
  group_by(month) %>% 
  summarise(ww_cum_cases=sum(highest_cumcase), ww_Deaths=sum(highest_cumdeaths),
            ww_cum_Recovered=sum(highest_cumrecovered), ww_cum_Tests=sum(highest_cumtest))

covid_graph


require(dplyr)

require(scales)

covid_graph <- covid_graph %>% 
  gather(ww_cum_data, cum_value, -month)

ggplot(covid_graph, aes(x=month, y=cum_value, group=ww_cum_data, color=ww_cum_data)) +
  theme_bw() +
  geom_line() +
  geom_point()+
  ggtitle("The world wide cumulative data for cases, deaths, recovered and Tests number") +
  scale_y_log10()

 # Another way (Group by Date)

max_cum_by_country_Date <- covid19_cum %>% 
  select(Date, Country, CumCases, CumDeaths, CumRecovered, CumTests) %>% 
  group_by(Date) %>% 
  summarise(case_date= sum(CumCases), test_date= sum(CumTests), 
            recovered_date=sum(CumRecovered), death_date=sum(CumDeaths))
  
max_cum_by_country_Date 


require(dplyr)

require(scales)

max_cum_by_country_Date <- max_cum_by_country_Date %>% 
  gather(ww_cum_data, cum_value, -Date)

ggplot(max_cum_by_country_Date, aes(x=Date, y=cum_value, group=ww_cum_data, color=ww_cum_data)) +
  theme_bw() +
  geom_line() +
  ggtitle("The world wide cumulative data for cases, deaths, recovered and Tests number (Date)") +
  scale_y_log10()



# 6. Extract the last day (05/05/2020) data and save it in a separate dataframe called
# “lastDay_data”.
# [Hint: use filter function with Date = "2020-05-05"]

Last_day_data <- covid19_cum %>% 
  filter(Date == "2020-05-05")

Last_day_data

# 7. Based on the last day data, extract the whole records of the top 10 countries worldwide
# that have current active cases, total confirmed cases, and fatality rate in separate
# dataframes (i.e. top10activeW, top10casesW, top10fatalityW, top10testsMW).
# [Hint: you can use head(arranged_data, n=10) to get the top 10 records]

# Current active case

top10activeW <- Last_day_data %>% 
  arrange(desc(Active)) %>% 
  head(top10activeW, n=10)

top10activeW 

# Total confirmed cases

top10casesW <- Last_day_data %>% 
  arrange(desc(CumCases)) %>% 
  head(top10casesW, n=10)

top10casesW

# Fatality rate

top10fatalityW <- Last_day_data %>% 
  arrange(desc(FatalityRate)) %>% 
  head(top10fatalityW, n=10)

top10fatalityW

# Total tests

top10testsW <- Last_day_data %>% 
  arrange(desc(CumTests)) %>% 
  head(top10testsW, n=10)

top10testsW


# 8. Based on the last day data, print the up to date confirmed, death, recovered cases as well
# as the tests for every continent.

up_to_date_data <- Last_day_data %>% 
  group_by(Continent) %>% 
  summarise(utd_confirmed_cases=sum(CumCases), utd_deaths=sum(CumDeaths),
            utd_recovered=sum(CumRecovered), utd_tests=sum(CumTests))


up_to_date_data


# 9. Build a graph to show the total number of cases over the time for the top 10 countries that
# have been obtained in question 7 (Use log for Y axis for better presentation).
# [Hint: first you need to get the data of the top-10 countries and then plot their lines]


top10casesW$Country

all_time_top10_data <- covid19_cum %>% 
  filter(Country %in% top10casesW$Country)  
  
all_time_top10_data

unique(all_time_top10_data[c("Country")])

all_time_top10_cases <- all_time_top10_data[ , c("Country", "Date", "NewCases", "CumCases", "month")]

all_time_top10_cases <- all_time_top10_cases %>% 
  group_by(month, Country) %>% 
  summarise(highest_cumcases_top10=max(CumCases), newcases_top10=sum(NewCases))

all_time_top10_cases
  
ggplot(all_time_top10_cases, aes(x=month, y=highest_cumcases_top10, group=Country, color=Country)) +
  theme_bw() +
  geom_line() +
  geom_point() +
  ggtitle("The world wide cumulative cases for top 10 countries") +
  scale_y_log10()

ggplot(all_time_top10_cases, aes(x=month, y=newcases_top10, group=Country, color=Country)) +
  theme_bw() +
  geom_line() +
  geom_point() +
  ggtitle("The world wide new cases for top 10 countries") +
  scale_y_log10()


# 10. Build a graph for the top 10 countries with current highest active cases which was
# obtained previously in question 7. The graph should have one subgraph (i.e. using facet 
# function) for each of these countries, every subgraph should show how the new cases, new
# deaths, and new recovered cases were changing over time (Use log for Y axis for better
# presentation, Use different colour to distinguish between new cases, deaths, and
# recovered).
# [hint: geom_line function with date on x_axis and each of the values of the variables in y_axis]

top10activeW$Country

all_time_top10active_data <- covid19_cum %>% 
  filter(Country %in% top10activeW$Country)  

all_time_top10active_data

all_time_top10active_cases <- all_time_top10active_data[ , c("Country", "Date", "NewCases", "NewDeaths", "Recovered","month")]

all_time_top10active_cases <- all_time_top10active_cases %>% 
  group_by(month, Country) %>% 
  summarise(newcases_top10active=sum(NewCases), newdeaths_top10active=sum(NewDeaths),
            recovered_top10active=sum(Recovered))

all_time_top10active_cases <- all_time_top10active_cases %>% 
  gather(Top10active_Data, value, -month, -Country)

all_time_top10active_cases

top10activeW %>% 
  ggplot(aes(x=Country, y=Active, group=Country, color=Country, fill=Country)) +
  geom_bar(stat='identity')+
  theme_bw()
  
all_time_top10active_cases %>% 
  ggplot(aes(x=month, y=value, group=Top10active_Data, color=Top10active_Data)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~Country, scale="free") +
  scale_y_log10()

#------------------------TASK-3:DATA-DRIVEN MODELLING--------------------------#
library(modelr)
library(broom)
# 1. Based on the data of the last day, that you have extracted in the previous task, create a
# separate dataframe named "cor_data" with the data of these variables
# (CumCases, CumTests, Population, GDP, GDPCapita).
# [Hint: you can use select function on the lastday_data dataframe]

cor_data <- Last_day_data[ , c("CumCases", "CumTests", "Population", "GDP", "GDPCapita")]
 
cor_data 

# 2. Compute the correlation matrix between the variables of the “cor_data” and visualise
# this correlation matrix.

cor(cor_data)
library(GGally)
ggcorr(cor_data, label=TRUE, label_alpha = TRUE)

# 3. Divide the cor_data into training and testing, where training data represent 65%
# of the number of rows.

sample_idx <- sample(c(TRUE, FALSE), nrow(cor_data), replace = T, prob = c(0.65,0.35))
train <- cor_data[sample_idx, ]
test  <- cor_data[!sample_idx, ]

train

test

# 4. Train a linear regression model to predict cumulative cases from the GDP of the
# countries. Then, evaluate this model on the test data and print the root mean
# square error value.

single_model <- lm(CumCases ~ GDP, data = train)
print(single_model)

summary(single_model)


#Test model on the test set

test$Predicted_1 <- predict(single_model, test)

# Compute the residual mean square error (RMSE) to evaluate the output of the model.

actuals <- test$CumCases
predictions <- test$Predicted_1

sqrt(mean((predictions-actuals)^2))


plot(single_model)

# 5. Train another linear regression model to predict cumulative cases from all the
# other variables. Then, evaluate this model on the test data and print the root
# mean square error value.

multi_model <- lm(CumCases ~ ., data = train)
print(multi_model)

summary(multi_model)

# Test the second model on the testing data and evaluate its performance using RMSE metrics

test$predicted_2 <- predict(multi_model, test)

actuals <- test$CumCases
predictions <- test$predicted_2

sqrt(mean((predictions-actuals)^2))

plot(multi_model)

ggplot(cor_data, aes(GDP)) +
  geom_histogram(aes(y = ..density..), fill = "aquamarine3") +
  geom_density(color = "red")

ggplot(cor_data, aes(GDPCapita)) +
  geom_histogram(aes(y = ..density..), fill = "aquamarine3") +
  geom_density(color = "red")
ggplot(cor_data, aes(CumTests)) +
  geom_histogram(aes(y = ..density..), fill = "aquamarine3") +
  geom_density(color = "red")
ggplot(cor_data, aes(Population)) +
  geom_histogram(aes(y = ..density..), fill = "aquamarine3") +
  geom_density(color = "red")



# For analysis:

top10casesW %>% 
  ggplot(aes(x=Country, y=CumCases, group=Country, color=Country, fill=Country)) +
  geom_bar(stat='identity')+
  theme_bw()

top10casesW %>% 
  ggplot(aes(x=Country, y=CumCases, group=Country, color=Country, fill=Country)) +
  geom_bar(stat='identity')+
  theme_bw()

top10fatalityW %>% 
  ggplot(aes(x=Country, y=FatalityRate, group=Country, color=Country, fill=Country)) +
  geom_bar(stat='identity')+
  theme_bw()

top10testsW %>% 
  ggplot(aes(x=Country, y=CumTests, group=Country, color=Country, fill=Country)) +
  geom_bar(stat='identity')+
  theme_bw()
