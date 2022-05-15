source("R-Prep.R")
library(ghibli) #for additional colors

sum_numbers <- function(x = numbers, y = number2){
  y <- x^2
  z <- y^2
  return(list(y, z))
} 

list_of_numbers <- c(1, 2) 

result <- sum_numbers(x = list_of_numbers)

# Write a function that takes a list of numbers and concatenate a letter to them
my_function(c(1, 2, 3))

letter_number <- function(x = numbers){
 y <- paste0(x, letters[1:length(x)]) 
 return(y)
}

letter_number(x = c(5,4,30,4567,1,701))

###


player1 <- "Nikola Jokic"; player2 <- "Joel Embiid"
table <- c("per_game", "advanced")
season <- 2022

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

player_comparison(
  player1 = "Darius Garland", 
  player2 = "Russell Westbrook",
  table = c("advanced", "per_game"),
  season = 2021
  )

# Assignment
# 1. Accomodate function for per game stats
# 2. Accomodate function for different years (user enters year = "2001")
# 3. Comment your code so that it is understandable