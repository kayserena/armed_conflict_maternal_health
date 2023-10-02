#libraries
library(here)
library(tidyverse)
library(data.table)
library(states)
library(countrycode)
library(dplyr)
library(tidyr)

###cleaning year, merging, and adding country code

#read in data
rawinf <- read.csv(here("original", "infantmortality.csv"), header = TRUE)

#cleaning function
cleanyear <- function(rawdat,mortalitytype){
rawdat %>% select(Country.Name, 'X2000':'X2019') %>%
  pivot_longer(cols=c('X2000':'X2019'),
               names_to='Year',
               values_to=mortalitytype,
               names_prefix = 'X',
               values_transform = list(Year=as.integer)) %>%
    mutate(Year = as.numeric(Year)) %>%
    arrange(Country.Name, Year)}
cleaninfant <- cleanyear(rawdat = rawinf,mortalitytype = "infantmort")

#repeat function for neonatal
rawneo <- read.csv(here("original", "neonatalmortality.csv"), header = TRUE)
cleanneo <- cleanyear(rawdat = rawneo,mortalitytype = "neomort")

#repeat function for under5
rawunder <- read.csv(here("original", "under5mortality.csv"), header = TRUE)
cleanunder5 <- cleanyear(rawdat = rawunder,mortalitytype = "under5mort")

#repeat function for mat
rawmat <- read.csv(here("original", "maternalmortality.csv"), header = TRUE)
cleanmat <- cleanyear(rawdat = rawmat,mortalitytype = "matmort")

#make a list
list_mort <- list(cleanmat, cleanneo, cleanunder5,cleaninfant)
lapply(list_mort, FUN = summary)

#concatenate
allmort <- reduce(list_mort, full_join)
head(allmort)

#add country code
# specify the variable that includes the country names first
# use origin option to specify the format of the country names
# use destination to specify the format you want to convert the countries into
allmort$ISO <- countrycode(allmort$Country.Name, 
                             origin = "country.name", 
                             destination = "iso3c")

#drop country variable
allmortISO <- select(allmort, -1)

#change year to integer
#allmortISO$Year = as.integer(allmortISO$Year)

#writing new csv with clean data
#write.csv(allmortISO, here("cleandata", "cleanallmortality.csv"), row.names = FALSE)
