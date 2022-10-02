library(glue)
library(dplyr)
library(readr)

# basic, no checking
table_to_lookup <- function(df, from_to=names(df)[1:2]){
  lookup <- as.list(df[[from_to[[2]]]])
  names(lookup) <- df[[from_to[[1]]]]
  return(lookup)
}
replace_null <- function(x, with=NA){
  x %>%
    replace(.=="NULL", with)
}
self_join <- function(x, ...){
  x %>%
    left_join(x, ...)
}
# crickety utilities
overs_to_raw_balls <- function(x){
  # vectorising hack
  if(length(x) > 1){
    return(sapply(x, overs_to_raw_balls))
  }
  over_details <- if_else(stringr::str_detect(x, "\\."), glue("{x}"), glue("{x}.0")) %>%
    stringr::str_split("\\.") %>%
    purrr::pluck(1) %>%
    as.numeric()
  return(6*over_details[[1]] + over_details[[2]])
}
raw_balls_to_overs <- function(x){
  full_overs <- floor(x/6)
  balls <- x %% 6
  return(glue("{full_overs}.{balls}"))
}


project_dir <- "./mens_t20_world_cup_2021/"
data_dir <- glue("{project_dir}/data/")

comp_config <- yaml::read_yaml(glue("{project_dir}/points-allocation.yaml"))

points_config <- comp_config[["points"]]

df_teams <- read_csv(glue("{data_dir}/teams.csv"))
df_matches <- read_csv(glue("{data_dir}/matches.csv"))
df_match_teams <- read_csv(glue("{data_dir}/match_teams.csv"))
df_participant_shares <- read_csv(glue("{data_dir}/participant_shares.csv"))
df_bonus_points <- read_csv(glue("{data_dir}/bonus_points.csv")) %>%
  mutate(
    bonus_points = points_config[["bonus-points"]][bonus_event] %>%
      replace_null() %>%
      unlist() %>%
      tidyr::replace_na(0)
  )

team_shares <- df_participant_shares %>%
  group_by(team_code) %>%
  summarise(
    total_shares = length(share_id),
  ) %>%
  right_join(df_teams, by=c("team_code"="code")) %>%
  mutate(total_shares = tidyr::replace_na(total_shares, 0))

shares_lookup <- table_to_lookup(team_shares)

# TODO:
# can catch some input errors by checking toss/batting details match with something from scores
# can also check that scores are consistent with result
# ~~also compute group tables as a cross-check~~
# TODO:
# duckworth-lewis-stern. Maybe only if needs be/low-priority

df <- df_matches %>%
  right_join(df_match_teams, by="match_number") %>%
  # mutate(result = tidyr::replace_na(result, "yet_to_play")) %>%
  filter(!is.na(result)) %>%
  mutate(result_points = points_config[["match-result-points"]][result] %>% unlist) %>%
  mutate(multiplier = points_config[["stage-multipliers"]][stage] %>% unlist) %>%
  mutate(team_points = multiplier * result_points)

df

# table showing opponent information by match
df_head_to_head <- df %>%
  select(match_number, stage, group, team, runs, wickets, overs, result, team_points) %>%
  self_join(team, by=c("match_number", "stage", "group"), suffix=c("_main", "_opponent")) %>%
  filter(team_main != team_opponent) %>%
  mutate(
    overs_faced_eff = overs_to_raw_balls(
      if_else(wickets_main == 10, glue("20.0"), glue("{overs_main}"))
    ) %>% raw_balls_to_overs()
  ) %>%
  mutate(
    overs_bowled_eff = overs_to_raw_balls(
      if_else(wickets_opponent == 10, glue("20.0"), glue("{overs_opponent}"))
    ) %>% raw_balls_to_overs()
  ) %>%
  mutate(
    nrr = runs_main/(overs_to_raw_balls(overs_faced_eff)/6) - runs_opponent/(overs_to_raw_balls(overs_bowled_eff)/6)
  ) %>%
  # TODO: probably should be categories rather than bool
  mutate(close_loss = (result_main == "lose") & nrr > -1)

# where are points from?
df_full_points <- df_head_to_head %>%
  filter(team_main != team_opponent) %>%
  # TODO: need to get these into main logic
  mutate(result_main = if_else(close_loss, "close_loss", result_main)) %>%
  mutate(team_points_main = if_else(close_loss, 0.2, as.double(team_points_main))) %>%
  #
  mutate(event = glue("{result_main}_{team_opponent}_{stage}")) %>%
  rename(team = team_main, points = team_points_main) %>%
  select(team, points, event) %>%
  bind_rows(df_bonus_points %>% rename(points = bonus_points, event = bonus_event) %>% select(team, points, event))

df_team_points <- df_full_points %>%
  # filter(result != "yet_to_play") %>%
  group_by(team) %>%
  summarise(
    matches_played = length(points),
    total_points = sum(points),
    .groups = "drop",
  ) %>%
  mutate(total_shares = shares_lookup[team] %>% unlist()) %>%
  mutate(points_per_share = if_else(total_shares != 0, total_points / total_shares, 0)) %>%
  arrange(desc(points_per_share))

df_team_points

df_participant_points_by_share <- df_participant_shares %>%
  left_join(df_team_points, by=c("team_code"="team"))

df_participant_scores <- df_participant_points_by_share %>%
  group_by(participant_id) %>%
  summarise(
    total_points = sum(points_per_share),
  ) %>%
  arrange(desc(total_points))


# group tables
df_group_tables <- df_head_to_head %>%
  filter(!is.na(group)) %>%
  group_by(stage, group, team_main) %>%
  summarise(
    matches = length(result_main),
    wins = sum(result_main == "win"),
    losses = sum(result_main == "lose"),
    ties = sum(result_main == "tie"),
    nr = sum(result_main == "no_result"),
    points = 2*wins + 1*ties + 1*nr,
    runs_scored = sum(runs_main),
    overs_faced = sum(overs_to_raw_balls(overs_main)) %>% raw_balls_to_overs(),
    overs_faced_eff = sum(
      overs_to_raw_balls(
        if_else(wickets_main == 10, glue("20.0"), glue("{overs_main}"))
      )
    ) %>% raw_balls_to_overs(),
    runs_against = sum(runs_opponent),
    overs_bowled = sum(overs_to_raw_balls(overs_opponent)) %>% raw_balls_to_overs(),
    overs_bowled_eff = sum(
      overs_to_raw_balls(
        if_else(wickets_opponent == 10, glue("20.0"), glue("{overs_opponent}"))
      )
    ) %>% raw_balls_to_overs(),
    nrr = runs_scored/(overs_to_raw_balls(overs_faced_eff)/6) - runs_against/(overs_to_raw_balls(overs_bowled_eff)/6)
  ) %>%
  rename(team = team_main) %>%
  arrange(desc(points), desc(nrr), .by_group = TRUE)

df_group_tables %>%
  select(stage, group, team, matches, wins, losses, nr, points, nrr)

if(Sys.getenv("WRITE") != ""){
  write_csv(df_participant_points_by_share, glue("./{data_dir}/generated/participant-scores-by-share.csv"))
  write_csv(df_participant_scores, glue("./{data_dir}/generated/participant-scores.csv"))
  write_csv(df_team_points, glue("./{data_dir}/generated/team-points.csv"))
  write_csv(df_group_tables, glue("./{data_dir}/generated/group-tables.csv"))
}

