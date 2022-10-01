library(glue)
library(dplyr)

# basic, no checking
table_to_lookup <- function(df, from_to=names(df)[1:2]){
  lookup <- as.list(df[[from_to[[2]]]])
  names(lookup) <- df[[from_to[[1]]]]
  return(lookup)
}
divide_or_zero <- function(numerator, denominator){
  if(denominator != 0){
    return(numerator/denominator)
  }
  return(0)
}

data_dir <- "./data/mens_t20_world_cup_2022"

comp_config <- yaml::read_yaml(glue("{data_dir}/points-allocation.yaml"))

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
  mutate(result_points = comp_config[["match-result-points"]][result] %>% unlist) %>%
  mutate(multiplier = comp_config[["stage-multipliers"]][stage] %>% unlist) %>%
  mutate(team_points = multiplier * result_points)

df

df %>%
  group_by(team) %>%
  summarise(
    total_points = sum(team_points),
    .groups = "drop",
  ) %>%
  mutate(total_shares = shares_lookup[team] %>% unlist()) %>%
  mutate(points_per_share = divide_or_zero(total_points, total_shares))

print("Done!")
