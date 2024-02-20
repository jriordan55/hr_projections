library(tidyverse)
library(baseballr)
library(gt)
library(gtExtras)
library(mlbplotR)

#2021 HR/PA = 0.03269149
#2022 HR/PA = 0.02864566
#2023 HR/PA = 0.03187329

#loading in data from FanGraphs
leaders_2021 <- fg_batter_leaders(startseason = 2021, endseason = 2021, qual = 300)

leaders_2022 <- fg_batter_leaders(startseason = 2022, endseason = 2022, qual = 300)

leaders_2023 <- fg_batter_leaders(startseason = 2023, endseason = 2023, qual = 300)

#combinding last 3 seasons worth of data
player_data <- rbind(leaders_2021, leaders_2022, leaders_2023, fill = TRUE)

#filtering only the stats we need
player_data <- player_data |>
  select(PlayerName, Season, team_name, Age, playerid, PA, HR) |>
  group_by(playerid)

# Replace values in the "Season" column
player_data$Season <- ifelse(player_data$Season == 2021, "3",
                           ifelse(player_data$Season == 2022, "2",
                                  ifelse(player_data$Season == 2023, "1", NA)))

# Rename the "Season" column to "Year"
names(player_data)[names(player_data) == "Season"] <- "Year"

# Create a new column called LeagueAvgR with corresponding values based on the  HR/PA by Year 
player_data$LeagueAvgR <- ifelse(player_data$Year == "3", 0.03269149,
                                 ifelse(player_data$Year == "2", 0.02864566,
                                        ifelse(player_data$Year == "1", 0.03187329, NA)))

# Create a new column called weights with corresponding values based on the Year for HR rates and PA- earlier first, so recent years are weighted more
player_data$weights <- ifelse(player_data$Year == "1", 5,
                                 ifelse(player_data$Year == "2", 4,
                                        ifelse(player_data$Year == "3", 3, NA)))

# Create a new column called weights_pa with corresponding values based on the Year for HR rates and PA- earlier first, so recent years are weighted more
player_data$weights_pa <- ifelse(player_data$Year == "1", 0.6,
                              ifelse(player_data$Year == "2", 0.3,
                                     ifelse(player_data$Year == "3", 0, NA)))


#adjusting for player age
player_data$player_adj <- ifelse(player_data$Age < 29, 0.008 * (29 - player_data$Age), -0.005 * (player_data$Age - 29))

#calculating a new column called player_rate that is HR per PA
player_data$player_rate <- player_data$HR / player_data$PA

#weighted player avaerage
player_data <- player_data %>%
  group_by(playerid) %>%
  mutate(player_wavg = sum(PA * player_rate * weights) / sum(PA * weights)) %>%
  ungroup()

#weighted league average
player_data <- player_data %>%
  group_by(playerid) %>%
  mutate(league_wavg = sum(PA * LeagueAvgR * weights) / sum(PA * weights)) %>%
  ungroup()

#final player weight
player_data$playerw <- player_data$PA * player_data$weights / (player_data$PA * player_data$weights + 1200)

#final league weight
player_data$leaguew <- 1 - player_data$playerw

#HR probability for each player
player_data <- player_data %>%
  mutate(pred_playerHRprob = (player_wavg * playerw) + (league_wavg * leaguew)) %>%
  group_by(playerid) %>%
  ungroup()

#projecting 2024 PA
player_data <- player_data %>%
  group_by(playerid) %>%
  mutate(pred_playerPA = 200 + sum(PA * weights_pa)) %>%
  ungroup()

#projected HR pre age adjustment
player_data$predicted_playerHR <- player_data$pred_playerPA * player_data$pred_playerHRprob

#final projected HRs
player_data$predicted_playerHR_age_adjusted <- (1 + player_data$player_adj) * player_data$predicted_playerHR

#looks only at players who played last year
player_data <- player_data %>%
  filter(Year == "1")

#saving the data
write.csv(player_data, "2024_HR_Projections.csv", row.names = TRUE)