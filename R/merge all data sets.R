### merge all data sets
library(here)
library(tidyverse)
library(data.table)
library(states)
library(countrycode)
library(dplyr)
library(tidyr)

here()
covariates <- read.csv(here("original", "covariates.csv"), header = TRUE)
source(here("R","clean mort add country.R"))
source(here("R","derive binary arm conf.R"))
source(here("R","reading in disaster.R"))

# rename year to Year in covariates
names(covariates)[names(covariates) == "year"] <- "Year"


#list of all data sets
master <- list(covariates, allmortISO,armconf,cleandis)

# merge all
master |> reduce(left_join, by = c('ISO', 'Year')) -> finaldata

# need to fill in NAs with 0's for binconf, drought, earthquake, best
finaldata <- finaldata |>
  mutate(binconf = replace_na(binconf, 0),
         drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         best = replace_na(best, 0))

write.csv(finaldata, file = here("cleandata", "finaldata.csv"), row.names = FALSE)

dim(finaldata)
names(finaldata)
length(unique(finaldata$ISO))
