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
list_from <- function(base_list, indices){
  logical_indices_not_in_base_list <- !indices %in% names(base_list)
  if (any(logical_indices_not_in_base_list)) {
    msg_string <- paste0(indices[logical_indices_not_in_base_list], collapse=', ')
    stop(
      glue(
        "Could not find bonus points for event(s): {msg_string}"
      )
    )
  }
  return(base_list[indices])
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
runs_wickets_to_score <- function(r, w){
  trailing <- if_else(as.numeric(w) == 10, glue(""), glue("/{w}"))
  return(
    glue("{r}{trailing}")
  )
}
over_subtract <- function(o1, o2){
  # o1 - o2, in over format
  (overs_to_raw_balls(o1) - overs_to_raw_balls(o2)) %>%
    raw_balls_to_overs()
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
    bonus_points = list_from(points_config[["bonus-points"]], bonus_event) %>%
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
# duckworth-lewis-stern - check logic

df <- df_matches %>%
  right_join(df_match_teams, by="match_number") %>%
  mutate(result = tidyr::replace_na(result, "yet_to_play")) %>%
  # filter(!is.na(result)) %>%
  mutate(result_points = points_config[["match-result-points"]][result] %>% unlist) %>%
  mutate(multiplier = points_config[["stage-multipliers"]][stage] %>% unlist) %>%
  mutate(team_points = multiplier * result_points)

df

# TODO: man this code is ugly.

# table showing opponent information by match
df_head_to_head <- df %>%
  select(
    match_number, stage, multiplier, group, team,
    runs, wickets, overs, result,
    team_points, team_batting_first,
    dls_par_score, dls_overs,
  ) %>%
  self_join(
    team,
    by=c("match_number", "stage", "group", "multiplier", "team_batting_first"),
    suffix=c("_main", "_opponent")
  ) %>%
  filter(team_main != team_opponent) %>%
  # replacing missing
  mutate(runs_main = tidyr::replace_na(runs_main, 0)) %>%
  mutate(wickets_main = tidyr::replace_na(wickets_main, 0)) %>%
  mutate(overs_main = tidyr::replace_na(overs_main, 0)) %>%
  mutate(runs_opponent = tidyr::replace_na(runs_opponent, 0)) %>%
  mutate(wickets_opponent = tidyr::replace_na(wickets_opponent, 0)) %>%
  mutate(overs_opponent = tidyr::replace_na(overs_opponent, 0)) %>%
  # dls
  mutate(
    dls_status =
      if_else(
        (!is.na(dls_par_score_main) | !is.na(dls_par_score_opponent)) &
          (is.na(dls_overs_main) & is.na(dls_overs_opponent)),
        "abandoned",
        if_else(
          (!is.na(dls_par_score_main) | !is.na(dls_par_score_opponent)),
          "interrupted",
          "none"
        )
      )
  ) %>%
  #
  mutate(
    overs_faced_eff = overs_to_raw_balls(
      if_else(
        result_main == "no_result",
        glue("0.0"),
        if_else(
          (dls_status == "abandoned") & !is.na(dls_par_score_opponent),
          # TODO: does this change if team is bowled out?
          glue("{overs_opponent}"),
          if_else(
            (dls_status == "interrupted") & !is.na(dls_par_score_opponent),
            glue("{dls_overs_opponent}"),
            if_else(wickets_main == 10, glue("20.0"), glue("{overs_main}"))
          )
        )
      )
    ) %>% raw_balls_to_overs()
  ) %>%
  mutate(
    overs_bowled_eff = overs_to_raw_balls(
      if_else(
        result_main == "no_result",
        glue("0.0"),
        if_else(
          (dls_status == "abandoned") & !is.na(dls_par_score_main),
          glue("{overs_main}"),
          if_else(
            (dls_status == "interrupted") & !is.na(dls_par_score_main),
            glue("{dls_overs_main}"),
            if_else(wickets_opponent == 10, glue("20.0"), glue("{overs_opponent}"))
          )
        )
      )
    ) %>% raw_balls_to_overs()
  ) %>%
  mutate(
    runs_scored_eff = if_else(
      result_main == "no_result",
      0,
      if_else(
        (dls_status == "abandoned") & !is.na(dls_par_score_opponent),
        dls_par_score_opponent,
        if_else(
          (dls_status == "interrupted") & !is.na(dls_par_score_opponent),
          dls_par_score_opponent - 1,
          runs_main
        )
      )
    )
  ) %>%
  mutate(
    runs_conceded_eff = if_else(
      result_main == "no_result",
      0,
      if_else(
        (dls_status == "abandoned") & !is.na(dls_par_score_main),
        dls_par_score_main,
        if_else(
          (dls_status == "interrupted") & !is.na(dls_par_score_main),
          dls_par_score_main - 1,
          runs_opponent
        )
      )
    )
  ) %>%
  mutate(
    nrr = runs_scored_eff / (overs_to_raw_balls(overs_faced_eff) / 6) -
      runs_conceded_eff / (overs_to_raw_balls(overs_bowled_eff) / 6)
  ) %>%
  # automatic bonuses
  mutate(bonus_close_loss = (result_main == "lose") & nrr > -1) %>%
  mutate(bonus_bowled_out_opponent = (wickets_opponent == 10) & (result_main != "no_result")) %>%
  mutate(bonus_no_wickets_lost = (wickets_main == 0 & !(result_main %in% c("no_result", "yet_to_play"))))

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
  filter(result_main != "yet_to_play") %>%
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
    # TODO: this needs an outside table to get info
    # matches_played = length(unique(id)),
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
    runs_scored_eff = sum(runs_scored_eff),
    overs_faced_eff = sum(
      overs_to_raw_balls(
        overs_faced_eff
      )
    ) %>% raw_balls_to_overs(),
    runs_against = sum(runs_opponent),
    overs_bowled = sum(overs_to_raw_balls(overs_opponent)) %>% raw_balls_to_overs(),
    runs_conceded_eff = sum(runs_conceded_eff),
    overs_bowled_eff = sum(
      overs_to_raw_balls(
        overs_bowled_eff
      )
    ) %>% raw_balls_to_overs(),
    nrr = runs_scored_eff / (overs_to_raw_balls(overs_faced_eff) / 6) -
      runs_conceded_eff / (overs_to_raw_balls(overs_bowled_eff) / 6)
  ) %>%
  rename(team = team_main) %>%
  arrange(desc(points), desc(nrr), .by_group = TRUE)

df_group_tables %>%
  select(stage, group, team, matches, wins, losses, nr, points, nrr)

# results table - head to head in a more processed form
df_results <- df_head_to_head %>%
  filter(result_main != "yet_to_play") %>%
  mutate(team_score = glue("{runs_wickets_to_score(runs_main, wickets_main)} ({overs_main})")) %>%
  mutate(opp_score = glue("{runs_wickets_to_score(runs_opponent, wickets_opponent)} ({overs_opponent})")) %>%
  # tmp cols - incorrect values for tie/nr but we don't care
  mutate(overs_win = if_else(result_main == "win", overs_main, overs_opponent)) %>%
  mutate(wickets_win = if_else(result_main == "win", wickets_main, wickets_opponent)) %>%
  mutate(
    team_win = if_else(
      result_main == "win",
      team_main,
      if_else(
        result_opponent == "win",
        team_opponent,
        ""
      )
    )
  ) %>%
  mutate(
    team_lose = if_else(
      result_main == "lose",
      team_main,
      if_else(
        result_opponent == "lose",
        team_opponent,
        ""
      )
    )
  ) %>%
  mutate(wickets_win = if_else(result_main == "win", wickets_main, wickets_opponent)) %>%
  mutate(
    win_lose_by = if_else(
      # team batting first (tbf) wins
      (team_batting_first == team_win),
      # tbf win desc,
      glue("by {abs(runs_main - runs_opponent)} runs"),
      if_else(
        # tbf loses,
        (team_batting_first == team_lose),
        # tbf lose outcome
        glue("by {10 - wickets_win} wickets (with {over_subtract('20.0', overs_win)} overs left)"),
        # tie/no-result
        glue("")
      )
    )
  ) %>%
  select(stage, group, team_main, team_opponent, result_main, team_score, opp_score, win_lose_by)

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
  write_csv(df_results, glue("./{gen_dir}/results.csv"))
}
