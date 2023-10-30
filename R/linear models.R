### linear models

library(here)

## modelling 4 outcomes: matmort, under5mort, infantmort, neomort (4 models)

## need to re-scale the coefficients: GDP was scaled up by 1000, need to scale it down
## population density needs to be rescaled from 0 to 1

# read in data
finaldata <- read.csv(here("cleandata", "finaldata.csv"), header = TRUE)

# rescale
finaldata$GDP <- finaldata$GDP / 1000
finaldata$popdens <- finaldata$popdens / 100
