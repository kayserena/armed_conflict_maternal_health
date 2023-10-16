### assignment 1
## Write an R script that creates a Table 1 using your favorite package 
## baseline, all countries, 2000, stratified by binary armed conflict

# libraries
library(here)
library(tidyverse)
library(data.table)
library(states)
library(countrycode)
library(dplyr)
library(tidyr)

# read in final data
finaldata <- read.csv(here("cleandata", "finaldata.csv"), header = TRUE)

# subset only baseline data
baseline <- finaldata %>% 
  filter(Year == 2000)

# create table 1 
# label variables with names
label(baseline$popdens)       <- "Population density"
label(baseline$urban)       <- "Urban residence"
label(baseline$agedep)     <- "Age dependency ratio"
label(baseline$GDP)       <- "GDP per capita"
label(baseline$male_edu)       <- "Male education"
label(baseline$temp)     <- "Mean temperature"
label(baseline$matmort)     <- "Maternal mortality rate"
label(baseline$neomort)     <- "Neonatal mortality rate"
label(baseline$under5mort)     <- "Under 5 mortality rate"
label(baseline$infantmort)     <- "Infant mortality rate"
label(baseline$totdeath)     <- "Total number of deaths"
label(baseline$binconf)     <- "Armed conflict status"

baseline$drought <- factor(baseline$drought, levels = c(0,1), labels = c("No drought", "Drought"))
baseline$earthquake <- factor(baseline$earthquake, levels = c(0,1), labels = c("No earthquake", "Earthquake"))
baseline$OECD <- factor(baseline$OECD, levels = c(0,1), labels = c("No OECD", "OECD"))
baseline$binconf <- factor(baseline$binconf, levels = c(0,1), labels = c("No armed conflict", "Armed conflict"))
label(baseline$drought)       <- "Drought"
label(baseline$earthquake)       <- "Earthquake"

table1( ~ GDP + OECD + popdens + urban + agedep + male_edu + temp + totdeath + drought + earthquake + matmort + neomort + under5mort + infantmort | binconf, data = baseline,
        caption = "Table 1. Average Overall Demographics by Armed Conflict Status",render.continuous = c(.="Median [Min, Max]"))

#notes for myself
# finaldata %>% filter(Year == 2000) - could try this instead of subsetting
# default render has mean
# Displaying different statistics for different variables - there are instructions online

### assignment 2
## Write an R script that creates a figure that shows the trend in maternal 
## mortality for countries that had an increase from 2000 to 2017
## Select only the countries that had an increase in maternal 
## mortality from 2000 to 2017 (why 2017? - no matmor data after 2017)
## Hint: This code will create a new variable diffmatmor that shows the 
## difference between maternal mortality of each year and maternal 
## mortality in 2000
## Create a line graph with maternal mortality on the y-axis, year on the 
## x-axis, and a unique color for each country

##Look at my code from last week (EDA) on how to create a line graph 
## using ggplot2

#create diff variable
finaldata <- finaldata |>
  dplyr::select(country_name, ISO, Year, matmort) |>
  dplyr::filter(Year < 2018) |>
  arrange(ISO, Year) |>
  group_by(ISO) |>
  mutate(diffmatmor = matmort - matmort [1L]) 

# select only those with an increase (therefore 2017-2000 is positive)
# vector for only countries with increase
inccountry <- finaldata %>%
  filter(Year == 2017 & diffmatmor > 0) %>% 
  select(ISO) %>%
  pull(ISO)

inccountry

figdata <- finaldata %>% 
  filter(ISO %in% inccountry)

figure <- figdata %>%
  ggplot(aes(x=Year, y=matmort, col=country_name)) + geom_line(aes(group=country_name)) + labs(y = "Maternal mortality rate per 100 000", x = "Year", color="Country", title = "Maternal mortality rates over time for countries with increased maternal mortality from 2017 to 2000")
figure

### assignment 3
## peer review, edits, and commit changes