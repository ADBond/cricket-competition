library(glue)
library(dplyr)

# basic, no checking
table_to_lookup <- function(df, from_to=names(df)[1:2]){
  lookup <- as.list(df[[from_to[[2]]]])
  names(lookup) <- df[[from_to[[1]]]]
  return(lookup)
}

data_dir <- "./data/mens_t20_world_cup_2021"

comp_config <- yaml::read_yaml(glue("{data_dir}/points-allocation.yaml"))

points_config <- comp_config[["points"]]

print("reading data...")
df_teams <- readr::read_csv(glue("{data_dir}/teams.csv"))
df_matches <- readr::read_csv(glue("{data_dir}/matches.csv"))
df_match_teams <- readr::read_csv(glue("{data_dir}/match_teams.csv"))
df_participant_shares <- readr::read_csv(glue("{data_dir}/participant_shares.csv"))

team_shares <- df_participant_shares %>%
  group_by(team_code) %>%
  summarise(
    total_shares = length(share_id),
  ) %>%
  right_join(df_teams, by=c("team_code"="code")) %>%
  mutate(total_shares = tidyr::replace_na(total_shares, 0))

shares_lookup <- table_to_lookup(team_shares)

print("combine and process...")
df <- df_matches %>%
  right_join(df_match_teams, by="match_number") %>%
  mutate(result = tidyr::replace_na(result, "yet_to_play")) %>%
  mutate(result_points = points_config[["match-result-points"]][result] %>% unlist) %>%
  mutate(multiplier = points_config[["stage-multipliers"]][stage] %>% unlist) %>%
  mutate(team_points = multiplier * result_points)

df

df %>%
  filter(result != "yet_to_play") %>%
  group_by(team) %>%
  summarise(
    total_points = sum(team_points),
    .groups = "drop",
  ) %>%
  mutate(total_shares = shares_lookup[team] %>% unlist()) %>%
  mutate(points_per_share = ifelse(total_shares != 0, total_points / total_shares, 0))

print("Done!")
