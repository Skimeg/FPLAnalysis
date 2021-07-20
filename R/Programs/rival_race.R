library(fplscrapR)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(gganimate)

`%notin%` <- Negate(`%in%`)

my_id <- 26713     #Ben Byrne
rival_id <- 327400  #Mike Hall
#rival_id <- 55781 #Hiren Naygandhi


#pull data for self and rival
my_season <- fplscrapR::get_entry_season(entryid = my_id)
rival_season <- fplscrapR::get_entry_season(entryid = rival_id)

#Get name
my_name <- my_season$name[[1]]
rival_name <- rival_season$name[[1]]

#Set title  
chart_title <- paste(my_name," vs.", rival_name)

#create data frame for own data. Add Id number for GW based on row number. Add variable with player name.

df_player <- my_season %>% 
  dplyr::mutate(points = total_points, gameweek =event) %>%
  dplyr::select(name, gameweek, points)

df_rival <- rival_season %>% 
  dplyr::mutate(points = total_points, gameweek =event) %>%
  dplyr::select(name, gameweek, points)

df <- rbind(df_player, df_rival) %>% 
  dplyr::mutate(name = factor(name))


p <- ggplot(df,
            aes(x=gameweek, y=points, group=name, colour=name)) +
  geom_line() + 
  labs(title = "FPL Head-to-Head",
       subtitle = chart_title,
       x = "Gameweek", 
       y = "Total Points") +
  theme(legend.position = c(0.8, 0.15), 
        legend.title = element_blank(), 
        plot.subtitle = element_text(size=14), 
        legend.direction = "vertical") +
  ggthemes::theme_fivethirtyeight() 

p + gganimate::transition_reveal(gameweek)

gganimate::anim_save("rival_race.gif")
