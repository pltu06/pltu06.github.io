source("R-Prep.R")
library(ghibli)

jordan_adv <- bref_players_stats(
  seasons = 1985,
  tables = c("advanced"),
  assign_to_environment = FALSE,
  nest_data = FALSE
)

jordan_totals <- bref_players_stats(
  seasons = 1985,
  tables = c("totals"),
  assign_to_environment = FALSE,
  nest_data = FALSE
)

jordan_adv <- bref_players_stats(
  seasons = 1985,
  tables = c("per_game"),
  assign_to_environment = FALSE,
  nest_data = FALSE
)

jordan_filter <- c("Michael Jordan")

adv_jordan_data <- jordan %>%
  filter(namePlayer %in% jordan_filter)

bref_players_stats(
  seasons = 2004,
  tables = c("advanced", "totals", "per game")
)

lebron_filter <- c("LeBron James")

adv_lebron_data <- dataBREFPlayerAdvanced %>%
  filter(namePlayer %in% lebron_filter)


