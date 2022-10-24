library(tidyverse)
library(broom)

team_results = read_csv("https://raw.githubusercontent.com/bbwieland/sasl-cbb/main/data/team_overall_results.csv")
recruit_ranks = read_csv("https://raw.githubusercontent.com/bbwieland/sasl-cbb/main/data/team_recruit_ranks.csv")
tourney_results = read_csv("https://raw.githubusercontent.com/bbwieland/sasl-cbb/main/data/team_tournament_results.csv")
tourney_results
team_results
team_data = team_results %>%
  select(team, conf, season)

tourney_data = tourney_results %>%
  select(team, w_percent, season = to)

recruit_data = recruit_ranks %>%
  select(team, avg_rating, season = year)

team_data_lead = tourney_data %>%
  left_join(recruit_data, by = c("team","season")) %>%
  left_join(team_data, by = c("team", "season")) %>%
  group_by(team) %>%
  arrange(season) %>%
  ungroup() %>%
  filter(!is.na(avg_rating) & season <= 2019)
team_data_lead

#2 or more wins
teams_join_notP6_good_2 = team_data_lead %>%
  filter(!(conf %in% c("B10","B12","ACC","BE","SEC","P12", "P10"))) %>%
  filter(w_percent > 0.5)

teams_join_notP6_bad_2 = team_data_lead %>%
  filter(!(conf %in% c("B10","B12","ACC","BE","SEC","P12", "P10"))) %>%
  filter(w_percent <= 0.5)

#teams_join_notP6_good_2
#teams_join_notP6_bad_2
twoWins = colMeans(teams_join_notP6_good_2[,"avg_rating"]) - colMeans(teams_join_notP6_bad_2[,"avg_rating"])
twoWins

#1 or more wins
teams_join_notP6_good_1 = team_data_lead %>%
  filter(!(conf %in% c("B10","B12","ACC","BE","SEC","P12", "P10"))) %>%
  filter(w_percent >= 0.5)

teams_join_notP6_bad_1 = team_data_lead %>%
  filter(!(conf %in% c("B10","B12","ACC","BE","SEC","P12", "P10"))) %>%
  filter(w_percent < 0.5)

#teams_join_notP6_good_1
#teams_join_notP6_bad_1
oneWin = colMeans(teams_join_notP6_good_1[,"avg_rating"]) - colMeans(teams_join_notP6_bad_1[,"avg_rating"])
oneWin

#3 or more wins
teams_join_notP6_good_3 = team_data_lead %>%
  filter(!(conf %in% c("B10","B12","ACC","BE","SEC","P12", "P10"))) %>%
  filter(w_percent >= 0.7)

teams_join_notP6_bad_3 = team_data_lead %>%
  filter(!(conf %in% c("B10","B12","ACC","BE","SEC","P12", "P10"))) %>%
  filter(w_percent < 0.5)

#teams_join_notP6_good_3
#teams_join_notP6_bad_3
threeWins = colMeans(teams_join_notP6_good_3[,"avg_rating"]) - colMeans(teams_join_notP6_bad_3[,"avg_rating"])
threeWins

oneWin
twoWins
threeWins

# In season N
ggplot(team_data_lead, aes(x = avg_rating, y = win_pct)) +
  geom_point() +
  geom_smooth(method = "lm")

lm(formula = win_pct ~ avg_rating,data = team_data_lead) %>%
  summary()

# In season N + 1
ggplot(team_data_lead, aes(x = avg_rating, y = next_win_pct_1)) +
  geom_point() +
  geom_smooth(method = "lm")

lm(formula = next_win_pct_1 ~ avg_rating,data = team_data_lead) %>%
  summary()

# In season N + 2
ggplot(team_data_lead, aes(x = avg_rating, y = next_win_pct_2)) +
  geom_point() +
  geom_smooth(method = "lm")

lm(formula = next_win_pct_2 ~ avg_rating,data = team_data_lead) %>%
  summary()

# In season N + 3
ggplot(team_data_lead, aes(x = avg_rating, y = next_win_pct_3)) +
  geom_point() +
  geom_smooth(method = "lm")

lm(formula = next_win_pct_3 ~ avg_rating,data = team_data_lead) %>%
  summary()

# Average of seasons N+1 through N+3
ggplot(team_data_lead, aes(x = avg_rating, y = avg_next_3_win)) +
  geom_point() +
  geom_smooth(method = "lm")

lm(formula = avg_next_3_win ~ avg_rating,data = team_data_lead) %>%
  summary()
