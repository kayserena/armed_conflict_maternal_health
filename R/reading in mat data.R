#reading in data and cleaning

library(here)
here()
rawdat <- read.csv(here("original", "maternalmortality.csv"), header = TRUE)
rawdat

library(dplyr)
library(tidyr)
cleandat <- rawdat %>% select(Country.Name, 'X2000':'X2019') %>%
pivot_longer(cols=c('X2000':'X2019'),
                    names_to='Year',
                    values_to='MatMor',
                    names_prefix = 'X',
                    values_transform = list(value=as.numeric)) 
