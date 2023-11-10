#Read & Save Data
library(fplscrapR)
library(tidyverse)

#Set parameters 
start_gw <- 1
end_gw <- 38
player_limit <- 20
league_code <- 583303 #overall league

#Get league info#
league <- fplscrapR::get_league(leagueid = league_code)

#Put key info into a data frame and set a limit on number of FPL players
df<-league$standings$results

#Create mini league data frame
df_ml <- df$entry %>% 
  purrr::map_dfr(~fplscrapR::get_entry_season(.x))


df_ml <- df_ml %>% dplyr::mutate(season = '23/24')
write.csv(df_ml, "Roche_League_2024.csv")
save(df_ml,file = 'Roche_League_2024.Rda')

#########################MERGE WITH PREVIOUS SEASONS##########

staging <- df_ml %>% 
  rename(GW_Score=points,
         Total_Pts=total_points,
         GW_Rank=rank,
         OVR_Rank=overall_rank,
         Gameweek=event,
         Player=name) %>%
  mutate(Manager=str_c(Player," ",season)) %>% 
  mutate(Gameweek=as.character(Gameweek)) %>% 
  select(Gameweek,Player, Manager, GW_Score, Total_Pts, GW_Rank, OVR_Rank)

Roche_League_GW_History <- bind_rows(Roche_League_GW_History, staging)
write.csv(Roche_League_GW_History, "Roche_League_GW_History.csv")
save(Roche_League_GW_History,file = 'Roche_League_GW_History.Rda')

