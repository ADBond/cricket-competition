library(glue)
library(dplyr)
library(readr)

competition <- "mens_t20_world_cup_2022"

project_dir <- glue("competitions/{competition}/")
data_dir <- glue("{project_dir}/data/")

comp_config <- yaml::read_yaml(glue("{project_dir}/points-allocation.yaml"))

constraints <- comp_config[["restrictions"]]

df_teams <- read_csv(glue("{data_dir}/teams.csv"))
df_participant_shares <- read_csv(glue("{data_dir}/participant_shares.csv"))
df_participants <- read_csv(glue("{data_dir}/participants.csv"))

df_part_summary <- df_participant_shares %>%
  left_join(
    df_participants %>% select(id, display_name) %>% rename(participant_name = display_name),
    by=c("participant_id"="id")
  ) %>%
  left_join(df_teams, by=c("team_code"="code")) %>%
  group_by(participant_id, participant_name, team_code) %>%
  summarise(
    n_shares = length(first_round_team),
    fr_count = sum(first_round_team),
  ) %>%
  group_by(participant_id, participant_name) %>%
  summarise(
    total_shares = sum(n_shares),
    number_group_stage_teams = sum(fr_count),
    max_shares_per_team = max(n_shares)
  )

df_too_many_per_team <- df_part_summary %>%
  mutate(constraint_max_shares_per_team = constraints[["max-shares-per-team"]]) %>%
  filter(max_shares_per_team > constraint_max_shares_per_team)

df_wrong_number_of_shares <- df_part_summary %>%
  mutate(constraint_total_shares = constraints[["total-shares"]]) %>%
  filter(total_shares != constraint_total_shares)

df_not_enough_fr_teams <- df_part_summary %>%
  mutate(constraint_min_number_group_stage_teams = constraints[["min-number-group-stage-teams"]]) %>%
  filter(number_group_stage_teams < constraint_min_number_group_stage_teams)

error <- FALSE
err_msg <- ""

if (nrow(df_too_many_per_team) > 0) {
  print(df_too_many_per_team)
  error <- TRUE
  err_msg <- glue("{err_msg}\nFound participants with too many shares per team")
}

if (nrow(df_wrong_number_of_shares) > 0) {
  print(df_wrong_number_of_shares)
  error <- TRUE
  err_msg <- glue("{err_msg}\nFound participants with wrong number of total shares")
}

if (nrow(df_not_enough_fr_teams) > 0) {
  print(df_not_enough_fr_teams)
  error <- TRUE
  err_msg <- glue("{err_msg}\nFound participants with too few first round team shares")
}
if (error) {
  stop(err_msg)
}
