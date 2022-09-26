library(tidyverse)

team_tourn_results = read_csv("https://raw.githubusercontent.com/bbwieland/sasl-cbb/main/data/team_tournament_results.csv")
team_recruit_ranks = read_csv("https://raw.githubusercontent.com/bbwieland/sasl-cbb/main/data/team_recruit_ranks.csv")

teams_join = left_join(team_tourn_results, team_recruit_ranks, by = c("team","to" = "year")) %>%
  mutate(avg_rating = ifelse(is.na(avg_rating),70,avg_rating)) # imputing NAs
