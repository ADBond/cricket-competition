library(glue)
library(dplyr)
library(readr)
library(stringr)

odds_string_to_odds_value <- function(x) {
  if (str_to_lower(x) == "evens") {
    return(1)
  }
  x <- str_split(x, "/")[[1]] %>%
    as.numeric()
  if (length(x) == 1) {
    return(x)
  }
  if (length(x) == 2) {
    return(x[[1]] / x[[2]])
  }
  stop("Invalid odds string")
}
odds_to_probs <- function(x) {
  return(x / (1 + x))
}
scale_probs <- function(x) {
  return(x / sum(x))
}

competition <- "mens_t20_world_cup_2022"

project_dir <- glue("competitions/{competition}/")
misc_dir <- glue("{project_dir}/misc/")

df_teams_odds <- read_csv(glue("{misc_dir}/teams_odds.csv")) %>%
  rename_with(.fn= ~ str_replace(., "odds_", "oddsagainst_"), .cols=starts_with("odds")) %>%
  # interpret slash-odds as numerical odds
  mutate(
    across(
      .cols=starts_with("oddsagainst"),
      .fns= ~ sapply(., odds_string_to_odds_value)
    )
  ) %>%
  mutate(
    oddsagainst_oa_mean = rowMeans(select(., starts_with("oddsagainst")))
  ) %>%
  # reciprocal as we don't want odds against
  mutate(
    across(
      .cols=starts_with("oddsagainst"),
      .fns= ~ sapply(., (function(x)(1/x))),
      .names="odds_{.col}"
    )
  ) %>%
  rename_with(.fn= ~ str_replace(., "oddsagainst_", ""), .cols=starts_with("odds_")) %>%
  mutate(
    odds_o_mean = rowMeans(select(., starts_with("odds_")))
  ) %>%
  # to naive probabilities
  mutate(
    across(
      .cols=starts_with("odds_"),
      .fns= ~ sapply(., odds_to_probs),
      .names="probs_{.col}"
    )
  ) %>%
  rename_with(.fn= ~ str_replace(., "odds_", ""), .cols=starts_with("probs")) %>%
  # and a simple uniform scaling to nominal 'true' probabilities, for reference
  mutate(
    across(
      .cols=starts_with("probs"),
      .fns=scale_probs,
      .names="probsreal_{.col}"
    )
  ) %>%
  rename_with(.fn= ~ str_replace(., "probs_", ""), .cols=starts_with("probsreal"))

df_teams_odds %>%
  summarise(across(contains("probs"), ~ sum(.x, na.rm = TRUE)))

# strategy:
# sample one at a time, in proportion to w/e columns
# if we get to three of one, delete from list
# if we get to nine with no first-round, then sample from only those
sample_shares <- function(column, verbose=TRUE){
  team_info <- df_teams_odds %>%
    select(display_name, team_code, first_round, (!!column))
  shares <- c()
  fr_teams <- team_info[["team_code"]][team_info[["first_round"]]]
  opts <- team_info[[column]]
  names(opts) <- team_info[["team_code"]]

  while(length(shares) < 10) {
    candidate <- sample(names(opts), 1, prob = opts/sum(opts))
    shares <- c(shares, candidate)
    if (length(shares[shares == candidate]) == 3) {
      if (verbose) {
        message(glue("max number of {candidate}, deleting..."))
        message("\t", paste0(shares, collapse = ", "))
      }
      opts <- opts[names(opts) != candidate]
    }
    if (length(shares) == 9 & !any(fr_teams %in% shares)) {
      if (verbose) {
        message("need min number of first-round teams! deleting big teams...")
        message("\t", paste0(shares, collapse = ", "))
      }
      opts <- opts[fr_teams]
    }
  }
  return(shares)
}

sample_shares("odds_o_mean")
sample_shares("probsreal_o_mean")
sample_shares("oddsagainst_oa_mean")

clipr::write_clip(sample_shares("odds_o_mean"))
clipr::write_clip(sample_shares("oddsagainst_oa_mean"))
