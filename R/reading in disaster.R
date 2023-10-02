#reading in and cleaning disaster data
library(here)
library(magrittr)
library(dplyr)
here()
rawdis <- read.csv(here("original", "disaster.csv"), header = TRUE)
rawdis
cleandis <- rawdis %>% 
  dplyr::filter(between(Year,2000,2019)) %>%
  dplyr::filter(Disaster.Type %in% c("Drought", "Earthquake")) %>%
  select(Year, ISO, Disaster.Type)

cleandis$drought <- ifelse(cleandis$Disaster.Type == "Drought", 1, 0)
cleandis$earthquake <- ifelse(cleandis$Disaster.Type == "Earthquake", 1, 0)

cleandis <- cleandis %>%
  group_by(Year, ISO) %>%
  summarize(drought = max(drought), earthquake = max(earthquake)) %>%
  ungroup()


