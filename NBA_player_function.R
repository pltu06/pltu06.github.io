#load packages----
library(nbastatR) #This is for loading NBA data
library(dplyr) #This is for manipulating data sets
library(ggplot2) #This is for graphing data
library(tidyr) # functions for tidying data
library(Hmisc) # loads the %nin% filter
library(ghibli) #for additional colors
#necessary for loading in NBA data----
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)


player_comparison <- function(
  player1 = "Nikola Jokic",
  player2 = "Joel Embiid", 
  table = c("per_game", "advanced"),
  season = 2022
)
{
  player_filter <- c(player1, player2)
  this_data <- vector(mode = "list", length(table)) # initialization
  
  for (i in 1:length(table)) {
    this_data[[i]] <- 
      bref_players_stats(
        seasons = season,
        tables = table[i],
        assign_to_environment = FALSE
      ) %>%
      # Filtering for players
      filter(namePlayer %in% player_filter)
  }
  
  # Converting data into long format:
  
  # ADVANCED DATA
  if (sum(table == "advanced")>0) {
    this_index <- which(table == "advanced")
    
    adv_player_data_long <- 
      this_data[[this_index]] %>%
      select(namePlayer, yearSeason, minutes:ratioVORP) %>%
      pivot_longer(cols = c(-namePlayer, -yearSeason)) %>%
      separate(col = namePlayer, into = c("First", "Last"), remove = FALSE)
    
    adv_player_graphs <- 
      ggplot(
        data = adv_player_data_long %>% filter(name%nin%"ratioVORP"), 
        aes(x = Last, y = value, fill = namePlayer)
      ) +
      geom_bar(stat = "identity") +
      facet_wrap(~name, scales = "free") +
      labs(title = "Advanced Data", x = "Player", y = "Value") +
      theme_minimal() +
      scale_fill_manual(values = ghibli_palettes$YesterdayMedium[c(4,7)]) +
      theme(legend.position = "none")
  }
  
  # PER GAME DATA
  if (sum(table == "per_game")>0) {
    this_index <- which(table == "per_game")
    
    per_player_data_long <- 
      this_data[[this_index]] %>%
      select(namePlayer, 
             yearSeason, 
             countGames, 
             pctFG:pctFT, 
             minutesPerGame:ptsPerGame) %>%
      pivot_longer(cols = c(-namePlayer, -yearSeason)) %>%
      separate(col = namePlayer, into = c("First", "Last"), remove = FALSE)
    
    per_player_graphs <- 
      ggplot(
        data = per_player_data_long, 
        aes(x = Last, y = value, fill = namePlayer)
      ) +
      geom_bar(stat = "identity") +
      facet_wrap(~name, scales = "free") +
      labs(title = "Per Game Data", x = "Player", y = "Value") +
      theme_minimal() +
      scale_fill_manual(values = ghibli_palettes$YesterdayMedium[c(4,7)]) +
      theme(legend.position = "none")
  }
  
  
  #Printing graphs
  print(adv_player_graphs); print(per_player_graphs)
}