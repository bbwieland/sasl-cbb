library(tidyverse)

team_tourn_results = read_csv("https://raw.githubusercontent.com/bbwieland/sasl-cbb/main/data/team_overall_results.csv")
team_recruit_ranks = read_csv("https://raw.githubusercontent.com/bbwieland/sasl-cbb/main/data/team_recruit_ranks.csv")

teams_join = left_join(team_tourn_results, team_recruit_ranks, by = c("team","season" = "year"), suffix = c("","_recruit")) %>%
  mutate(avg_rating = ifelse(is.na(avg_rating),70,avg_rating)) %>% # imputing NAs
  mutate(conf = ifelse(conf == "P10","P12",conf)) #fixing pac-10 vs. pac-12

teams_join_P6 = teams_join %>%
  filter(conf_recruit %in% c("B10","B12","ACC","BE","SEC","P12"))

ggplot(teams_join_P6 %>% filter(avg_rating > 70), aes(x = avg_rating, y = win_pct)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~conf)

ggplot(teams_join_P6 %>% filter(avg_rating > 70), aes(x = rank_recruit, y = win_pct)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~conf)

ggplot(teams_join_P6 %>% filter(avg_rating > 70), aes(x = five_stars + four_stars, y = win_pct)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~conf)

ggplot(teams_join_P6 %>% filter(avg_rating > 70), aes(x = avg_rating, y = off_ppp)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~conf)

ggplot(teams_join_P6 %>% filter(avg_rating > 70), aes(x = avg_rating, y = def_ppp)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~conf)

ggplot(teams_join_P6 %>% filter(avg_rating > 70), aes(x = avg_rating, y = off_to)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~conf)

ggplot(teams_join_P6 %>% filter(avg_rating > 70), aes(x = avg_rating, y = def_to)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~conf)

ggplot(teams_join_P6 %>% filter(avg_rating > 70), aes(x = avg_rating, y = off_ftr)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~conf)
