# Load packages
library(fplscrapR)
library(tidyverse)

# Read data
league_code <- 583303 #overall league
league <- fplscrapR::get_league(leagueid = league_code)
df<-league$standings$results

#Create mini league data frame
df_ml <- df$entry %>% 
  purrr::map_dfr(~fplscrapR::get_entry_season(.x))

#Only select the columns we need
df_ml <- df_ml %>% 
  mutate(net_points = points - event_transfers_cost,
         gross_points = points + points_on_bench) %>%
  select(name, event, points, net_points, gross_points, total_points)

#Create variable to deal with ordering
df_ml_grp<-df_ml %>%
  group_by(event) %>%
  arrange(desc(net_points), desc(points), desc(gross_points), total_points, desc(name)) %>% 
  mutate(prize_pos = row_number(event)) %>% 
  slice_head(n=4) %>% 
  ungroup()
  
weekly_prizes <- df_ml_grp %>% 
  select(name, event, prize_pos) %>% 
  spread(key=prize_pos,value=name)

View(weekly_prizes)
