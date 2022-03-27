#Compare 2021-2022 season of Joel Embiid to Nikola Jokic
#MARCH 13, 2022
#1. Find a way to get the stats we need into R, either by the package or downloading data from a website
#2. Figure out what you are going to do with the data
library(tidyverse)
library(nbastatR)
library(Hmisc)
library(ghibli)

#MARCH 27, 2022
#1. Create a R Markdown on this script
#2. Highlight in the script to plug in players

game_logs(seasons = 2021,
          league = "NBA",
          result_types = "player",
          season_types = "Regular Season"
          )
players_of_interest <- df_nba_player_dict %>%
  filter(namePlayer %in% c("Nikola Jokic", "Joel Embiid"))
box_scores(game_ids = )

#data_players <- players_tables(
#  player_ids = players_of_interest$idPlayer[1],
#  tables = c("passes"),
#  #measures = "Base",
#  seasons = 2021,
  #modes = c("Totals"),
  #season_types = "Regular Season"
#)

bref_players_stats(
  seasons = 2022,
  tables = c("advanced", "totals", "per_game"),
  )

player_filter <- c("DeMar DeRozan", "LeBron James", "Nikola Jokic", "Joel Embiid")

#ADVANCED STATS
adv_player_data <- dataBREFPlayerAdvanced %>%
  filter(namePlayer %in% player_filter)

adv_player_data_long <- adv_player_data %>%
  select(namePlayer, yearSeason, minutes:ratioVORP) %>%
  pivot_longer(cols = c(-namePlayer, -yearSeason))

ggplot(data = adv_player_data_long %>%
         filter(name%nin%"ratioVORP"), aes(x = namePlayer, 
                                           y = value, 
                                           fill = namePlayer))+
  geom_bar(stat = "identity")+
  facet_wrap(~name, scales = "free")+
  labs(x = "Player", y = "Value")+
  theme_minimal()+
  scale_fill_manual(values = ghibli_palettes$YesterdayMedium)+
  theme(legend.position = "none")+
  coord_flip()
  

#TOTALS STATS
tot_player_data <- dataBREFPlayerTotals %>%
  filter(namePlayer %in% player_filter)

tot_player_data_long <- tot_player_data %>%
  select(namePlayer, yearSeason, minutesTotals:ptsTotals) %>%
  pivot_longer(cols = c(-namePlayer, -yearSeason))

ggplot(data = tot_player_data_long, aes(x = namePlayer, 
                                           y = value, 
                                           fill = namePlayer))+
  geom_bar(stat = "identity")+
  facet_wrap(~name, scales = "free")+
  labs(x = "Player", y = "Value")+
  theme_minimal()+
  scale_fill_manual(values = ghibli_palettes$YesterdayMedium[c(4, 7)])+
  theme(legend.position = "none")

#PER GAME STATS
per_player_data <- dataBREFPlayerPerGame %>%
  filter(namePlayer %in% player_filter)

per_player_data_long <- per_player_data %>%
  select(namePlayer, yearSeason, countGames, pctFG:pctFT, minutesPerGame:ptsPerGame) %>%
  pivot_longer(cols = c(-namePlayer, -yearSeason))

ggplot(data = per_player_data_long, aes(x = namePlayer, 
                                           y = value, 
                                           fill = namePlayer))+
  geom_bar(stat = "identity")+
  facet_wrap(~name, scales = "free")+
  labs(x = "Player", y = "Value")+
  theme_minimal()+
  scale_fill_manual(values = ghibli_palettes$YesterdayMedium[c(4, 7)])+
  theme(legend.position = "none")
