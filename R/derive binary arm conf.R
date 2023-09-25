### derive armed conflict
armconf <- read.csv(here("original", "conflictdata.csv"), header = TRUE)
# sum best
armconf <- armconf %>%
  group_by(year, ISO) %>%
  summarize(best = sum(best)) 

# derive
armconf$binconf <- ifelse(armconf$best < 25, 0, 1)

names(armconf)[names(armconf) == "year"] <- "Year"

