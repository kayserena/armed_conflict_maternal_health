### derive armed conflict
armconf <- read.csv(here("original", "conflictdata.csv"), header = TRUE)
# sum best
armconf <- armconf %>%
  group_by(year, ISO) %>%
  summarize(totdeath = sum(best)) %>%
  mutate(year = year + 1) %>%
  ungroup()

# derive
armconf$binconf <- ifelse(armconf$totdeath < 25, 0, 1)

names(armconf)[names(armconf) == "year"] <- "Year"

