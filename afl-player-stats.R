if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(fitzRoy)) install.packages("fitzRoy", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(fitzRoy)

# retrieve player data
# lineups <- fetch_lineup()
playerstats <- fetch_player_stats_footywire()
playerdetails <- fetch_player_details_afltables()

# get min and max year in AFL fantasy data
minyear <- min(playerstats$Season)
maxyear <- max(playerstats$Season)

# only need player details for players who played from first season in AFL fantasy data onwards
playerdetails <- playerdetails %>%
  mutate(LastSeason = as.numeric(str_sub(Seasons, str_length(Seasons) - 3, str_length(Seasons)))) %>%
  filter(LastSeason >= minyear) %>%
  mutate(Team = ifelse(Team == "Brisbane Lions", "Brisbane", Team))

# remove unnecessary columns from player stats (only need 'AF' (AFL Fantasy points))
playerfantasypoints <- playerstats %>%
  select(Player, Team, Season, Round, AF)

# get list of teams
teams <- unique(playerstats$Team)

# get player details for players from each team
pd <- fetch_player_details_footywire(team = teams[1])
for(tm in teams){
  if(tm != teams[1]){
    if(tm == "Brisbane"){
      tm <- "Brisbane Lions"
    }
    if(tm == "North Melbourne"){
      tm <- "Kangaroos"
    }
    pd <- bind_rows(pd, fetch_player_details_footywire(team = tm))
  }
}
pd <- pd %>%
  mutate(Player = paste0(first_name, " ", surname))


names <- playerdetails %>%
  select(Player) %>%
  anti_join(playerstats, by = "Player")

# some player names have been shortened in playerstats - need to rename to full names
playerdetails_names <- playerdetails %>%
  filter(LastSeason == maxyear) %>%
  select(Player, Team, Seasons)
playerstats_names <- playerstats %>%
  filter(Season == maxyear) %>%
  group_by(Player, Team) %>%
  summarise(Games = n())

n1 <- 
n2 <- c("Alex N-Bullen", "Jamarra U-Hagan", "Nasiah W-Milera", "Bobby Hill", "Will H-Elliott", "Alastair Lord", "Anthony M-Tipungwuti", "Brandon Z-Thatcher",
        "Darcy B-Jones", "Jason H-Francis", "Sam P-Pepper", "Denver G-Barras", "Callum C-Jones", "Luke D-Uniacke", "Sam P-Seton")

newnames <- tibble(Player = n2, new_name = n1)


# write to csv
write_csv(playerfantasypoints, paste0("footywire-player-fantasy-points-", minyear, "-to-", maxyear, ".csv"))
write_csv(playerdetails, paste0("AFL-player-details-", maxyear, ".csv"))