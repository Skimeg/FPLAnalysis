library(fplscrapR)
library(ggplot2)
library(dplyr)
library(gganimate)
library(ggthemes)

#alwayscheating as example
my_id <- 20683
rival_id <- 19490

#pull data for self and rival
my_season <- get_entry_season(entryid = my_id)
rival_season <- get_entry_season(entryid = rival_id)

#Get name
my_name <- my_season$name[[1]]
rival_name <- rival_season$name[[1]]

#Set title  
chart_title <- paste(my_name," vs.", rival_name)

#create data frame for own data. Add Id number for GW based on row number. Add varaible with player name.
df_player <- data.frame(points=my_season$total_points)
df_player <- mutate(df_player, gameweek = as.numeric(rownames(df_player))) %>%
  mutate(name = rep(my_name,nrow(df_player)))

df_rival <- data.frame(points=rival_season$total_points)
df_rival <- mutate(df_rival, gameweek = as.numeric(rownames(df_rival))) %>%
  mutate(name = rep(rival_name,nrow(df_player)))

df <- rbind(df_player, df_rival)
df$name <- factor(df$name)

p <- ggplot(df,
            aes(x=gameweek, y=points, group=name, colour=name)) +
  geom_line() + 
  labs(title = "FPL Head-to-Head",
       subtitle = chart_title,
       x = "Gameweek", 
       y = "Total Points") +
  theme_fivethirtyeight() +
  theme(legend.position = c(0.8, 0.15), 
        legend.title = element_blank(), 
        plot.subtitle = element_text(size=14), 
        legend.direction = "vertical")

p + transition_reveal(gameweek)

anim_save("rival_race.gif")