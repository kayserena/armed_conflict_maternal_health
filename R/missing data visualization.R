### missing data visualization for maternal mortality data

# load libraries
library(naniar)
library(VIM)
library(finalfit)
library(here)
library(UpSetR)

# load in data
finaldata <- read.csv(here("cleandata", "finaldata.csv"), header = TRUE)

## heatplot missingness across dataframe
naniar::vis_miss(finaldata)

# small amount of missing data in GDP, popdens, urban, male edu, temp, neomort, under5mort, infantmort
# more missing data in matmort

## aggregation plot
VIM::aggr(finaldata, numbers = TRUE, prop = c(TRUE, FALSE))

# matmort is most missing

gg_miss_upset(finaldata)

# matmort usually missing alone

gg_miss_upset(finaldata, 
              nsets = 10,
              nintersects = 50)

gg_miss_case(finaldata)
# most cases are only missing 1 variable

gg_miss_case(finaldata, facet = Year)
# most missing data occurs in 2018 and 2019

gg_miss_fct(x = finaldata, fct = Year)
