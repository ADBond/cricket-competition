# stub
library(glue)
library(dplyr)

data_dir <- "./data"

print("processing matches...")
df_matches <- readr::read_csv(glue("{data_dir}/matches.csv"))
df_match_teams <- readr::read_csv(glue("{data_dir}/match_teams.csv"))

df <- df_matches %>%
  right_join(df_match_teams, by="match_number")
print("Done!")
df
