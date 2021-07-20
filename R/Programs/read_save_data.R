#Read & Save Data
library(fplscrapR)
library(tidyverse)

#Set parameters 
start_gw <- 1
end_gw <- 38
player_limit <- 16
league_code <- 144278 #overall league

#Get league info#
league <- fplscrapR::get_league(leagueid = league_code)

#Put key info into a data frame and set a limit on number of FPL players
df<-league$standings$results

#Create mini league data frame
df_ml <- df$entry %>% 
  purrr::map_dfr(~fplscrapR::get_entry_season(.x))

#Renumber the gameweek numbers which got messed up by the pandemic.
df_ml <- df_ml %>% dplyr::mutate(season = '21/22')
write.csv(df_ml, "Roche_League_2022.csv")
save(df_ml,file = 'Roche_League_2022.Rda')

#########################MERGE WITH PREVIOUS SEASONS##########

staging <- df_ml %>% 
  rename(GW_Score=`GW Score`,
         Total_Pts=`Total Pts`,
         GW_Rank=`GW Rank`,
         OVR_Rank=`Overall Rank`) %>% 
  mutate(Gameweek=str_remove_all(Gameweek, "[GW]")) %>% 
  select(Gameweek,Player, Manager, GW_Score, Total_Pts, GW_Rank, OVR_Rank)

Roche_League_GW_History <- bind_rows(Roche_League_GW_History, staging)
write.csv(Roche_League_GW_History, "Roche_League_GW_History.csv")
save(Roche_League_GW_History,file = 'Roche_League_GW_History.Rda')

