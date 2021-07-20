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
df_ml <- df_ml %>%
  dplyr::filter(event %notin% seq(30,38)) %>%
  dplyr::mutate(event = as.integer(ifelse(event > 38, event -9 ,event)),
                season = '19/20')
write.csv(df_ml, "Roche_League_2021.csv")
save(df_ml,file = 'Roche_League_2021.Rda')

