# stub
library(glue)
library(dplyr)

data_dir <- "./data/mens_t20_world_cup_2022"

comp_config <- yaml::read_yaml(glue("{data_dir}/points-allocation.yaml"))

print("reading data...")
df_matches <- readr::read_csv(glue("{data_dir}/matches.csv"))
df_match_teams <- readr::read_csv(glue("{data_dir}/match_teams.csv"))
df_participant_shares <- readr::read_csv(glue("{data_dir}/participant_shares.csv"))

team_shares <- df_participant_shares %>%
  group_by(team_code) %>%
  summarise(
    total_shares = length(share_id),
  )

print("combine and process...")
df <- df_matches %>%
  right_join(df_match_teams, by="match_number") %>%
  mutate(result = tidyr::replace_na(result, "yet_to_play")) %>%
  mutate(result_points = comp_config[["match-result-points"]][result] %>% unlist) %>%
  mutate(multiplier = comp_config[["stage-multipliers"]][stage] %>% unlist) %>%
  mutate(team_points = multiplier * result_points)

df

df %>%
  group_by(team) %>%
  summarise(
    total_points = sum(team_points),
  )

print("Done!")
