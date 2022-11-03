# check consistency of match info, where possible
library(glue)
library(dplyr)
library(readr)

competition <- "mens_t20_world_cup_2022"

project_dir <- glue("competitions/{competition}/")
data_dir <- glue("{project_dir}/data/")

df_matches <- read_csv(glue("{data_dir}/matches.csv"))
df_match_teams <- read_csv(glue("{data_dir}/match_teams.csv"))

df_too_many_wickets <- df_match_teams %>%
  filter(wickets > 10)
df_too_many_overs <- df_match_teams %>%
  filter(overs > 20)


error <- FALSE
err_msg <- ""

if (nrow(df_too_many_wickets) > 0) {
  print(df_too_many_wickets)
  error <- TRUE
  err_msg <- glue("{err_msg}\nFound innings(') with more than 10 wickets")
}
if (nrow(df_too_many_overs) > 0) {
  print(df_too_many_overs)
  error <- TRUE
  err_msg <- glue("{err_msg}\nFound innings(') of more than 20 overs")
}

if (error) {
  stop(err_msg)
}

