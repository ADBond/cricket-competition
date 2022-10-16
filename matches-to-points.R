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

competition <- "mens_t20_world_cup_2022"

project_dir <- glue("competitions/{competition}/")
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
      tidyr::replace_na(0) %>%
      # replace null again to cater for empty
      replace_null()
  ) %>%
  mutate(richer_event = if_else(is.na(details), glue("{bonus_event}"), glue("{bonus_event} ({details})")))

team_shares <- df_participant_shares %>%
  group_by(team_code) %>%
  summarise(
    total_shares = length(share_id),
  ) %>%
  right_join(df_teams, by=c("team_code"="code")) %>%
  mutate(total_shares = tidyr::replace_na(total_shares, 0))

shares_lookup <- table_to_lookup(team_shares)
team_lookup <- table_to_lookup(df_teams, from_to = c("code", "display_name"))

df_participant_shares %>%
  group_by(team_code) %>%
  summarise(
    shares = length(share_id),
  )

# TODO:
# can catch some input errors by checking toss/batting details match with something from scores
# can also check that scores are consistent with result
# ~~also compute group tables as a cross-check~~
# TODO:
# duckworth-lewis-stern. Maybe only if needs be/low-priority

df <- df_matches %>%
  right_join(df_match_teams, by="match_number") %>%
  mutate(result = tidyr::replace_na(result, "yet_to_play")) %>%
  # filter(!is.na(result)) %>%
  mutate(result_points = points_config[["match-result-points"]][result] %>% unlist) %>%
  mutate(multiplier = points_config[["stage-multipliers"]][stage] %>% unlist) %>%
  mutate(team_points = multiplier * result_points)

df

# table showing opponent information by match
df_head_to_head <- df %>%
  select(match_number, stage, multiplier, group, team, runs, wickets, overs, result, team_points) %>%
  self_join(team, by=c("match_number", "stage", "group", "multiplier"), suffix=c("_main", "_opponent")) %>%
  filter(team_main != team_opponent) %>%
  # replacing missing
  mutate(runs_main = tidyr::replace_na(runs_main, 0)) %>%
  mutate(wickets_main = tidyr::replace_na(wickets_main, 0)) %>%
  mutate(overs_main = tidyr::replace_na(overs_main, 0)) %>%
  mutate(runs_opponent = tidyr::replace_na(runs_opponent, 0)) %>%
  mutate(wickets_opponent = tidyr::replace_na(wickets_opponent, 0)) %>%
  mutate(overs_opponent = tidyr::replace_na(overs_opponent, 0)) %>%
  #
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
  # automatic bonuses
  mutate(bonus_close_loss = (result_main == "lose") & nrr > -1) %>%
  mutate(bonus_bowled_out_opponent = (wickets_opponent == 10)) %>%
  mutate(bonus_no_wickets_lost = (wickets_main == 0 & result_main != "yet_to_play"))

df_auto_bonus <- df_head_to_head %>%
  select(stage, multiplier, team_main, team_opponent, starts_with("bonus")) %>%
  tidyr::pivot_longer(starts_with("bonus"), names_to="bonus", values_to="status") %>%
  mutate(simple_event = stringr::str_replace(bonus, "bonus_", "")) %>%
  mutate(display_opponent = recode(team_opponent, !!!team_lookup)) %>%
  mutate(richer_event = glue("{simple_event} (vs {display_opponent}, in {stage})")) %>%
  filter(status) %>%
  mutate(raw_points = points_config[["match-result-bonuses"]][simple_event] %>% unlist()) %>%
  #mutate(points = raw_points * multiplier)
  mutate(points = raw_points * 1)

# where are points from?
df_full_points <- df_head_to_head %>%
  filter(team_main != team_opponent) %>%
  mutate(display_opponent = recode(team_opponent, !!!team_lookup)) %>%
  mutate(event = glue("{result_main} in match vs {display_opponent} in {stage}")) %>%
  rename(team = team_main, points = team_points_main) %>%
  select(team, points, event) %>%
  bind_rows(df_bonus_points %>% rename(points = bonus_points, event = richer_event) %>% select(team, points, event)) %>%
  bind_rows(df_auto_bonus %>% rename(team = team_main, event = richer_event) %>% select(team, points, event)) %>%
  left_join(df_teams, by=c("team"="code")) %>%
  arrange(team)

append_missing_teams <- function(df){
  df_missing <- df_teams %>%
    filter(!code %in% df$team)
  if (nrow(df_missing) == 0){
    return(df)
  }
  return(
    df %>%
      bind_rows(
        df_missing %>%
          select(display_name, code) %>%
          rename(team = code) %>%
          mutate(matches_played = 0) %>%
          mutate(total_points = 0)
      )
  )
}

df_team_points <- df_full_points %>%
  # filter(result != "yet_to_play") %>%
  group_by(team, display_name) %>%
  summarise(
    matches_played = length(points),
    total_points = sum(points),
    .groups = "drop",
  ) %>%
  append_missing_teams() %>%
  mutate(total_shares = shares_lookup[team] %>% unlist()) %>%
  mutate(points_per_share = if_else(total_shares != 0, total_points / total_shares, 0)) %>%
  arrange(desc(points_per_share))

df_team_points

df_participant_points_by_share <- df_participant_shares %>%
  left_join(df_team_points, by=c("team_code"="team")) %>%
  mutate(matches_played = tidyr::replace_na(matches_played, 0))

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
    matches = length(result_main[result_main != "yet_to_play"]),
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
  gen_dir <- glue("./{data_dir}/generated")
  if (!dir.exists(gen_dir)) {
    dir.create(gen_dir, recursive = TRUE)
  }
  write_csv(df_participant_points_by_share, glue("./{gen_dir}/participant-scores-by-share.csv"))
  write_csv(df_participant_scores, glue("./{gen_dir}/participant-scores.csv"))
  write_csv(df_team_points, glue("./{gen_dir}/team-points.csv"))
  write_csv(df_group_tables, glue("./{gen_dir}/group-tables.csv"))
  df_full_points %>%
    select(display_name, event, points) %>%
    rename(team = display_name) %>%
    write_csv(glue("./{gen_dir}/team-points-breakdown.csv"))
}
