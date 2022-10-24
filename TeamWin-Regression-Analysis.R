library(tidyverse)
library(broom)

team_results = read_csv("data/team_overall_results.csv")
recruit_ranks = read_csv("data/team_recruit_ranks.csv")

team_data = team_results %>%
  select(team, win_pct, season)

recruit_data = recruit_ranks %>%
  select(team, avg_rating, season = year)

team_data_lead = team_data %>%
  left_join(recruit_data, by = c("team","season")) %>%
  group_by(team) %>%
  arrange(season) %>%
  mutate(next_season_1 = lead(season),
         next_win_pct_1 = lead(win_pct),
         next_season_2 = lead(season,2),
         next_win_pct_2 = lead(win_pct,2),
         next_season_3 = lead(season,3),
         next_win_pct_3 = lead(win_pct,3)) %>%
  ungroup() %>%
  mutate(avg_next_3_win = (next_win_pct_1 + next_win_pct_2 + next_win_pct_3)/3) %>%
  filter(!is.na(avg_rating) & season <= 2019)

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
