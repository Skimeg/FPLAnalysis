library(fplscrapR)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(gganimate)

`%notin%` <- Negate(`%in%`)

my_id <- 20683     #Ben Byrne
rival_id <- 19490  #Mike Hall
rival_id2 <- 55781 #Hiren Naygandhi
rival_id3 <- 2199899 #Ryan Finch
rival_id4 <- 68916 #James Penney
rival_id5 <- 669110 #Ed Dunlop


#pull data for self and rival
my_season <- fplscrapR::get_entry_season(entryid = my_id)
rival_season <- fplscrapR::get_entry_season(entryid = rival_id)
rival2_season <- fplscrapR::get_entry_season(entryid = rival_id2)
rival3_season <- fplscrapR::get_entry_season(entryid = rival_id3)
rival4_season <- fplscrapR::get_entry_season(entryid = rival_id4)
rival5_season <- fplscrapR::get_entry_season(entryid = rival_id5)


#Get name
my_name <- my_season$name[[1]]
rival_name <- rival_season$name[[1]]
rival_name2 <- rival2_season$name[[1]]
rival_name3 <- rival3_season$name[[1]]
rival_name4 <- rival4_season$name[[1]]
rival_name5 <- rival5_season$name[[1]]

#Set title  
chart_title <- paste(my_name," vs.", rival_name, ", ", rival_name2, ", ", rival_name3)

#create data frame for own data. Add Id number for GW based on row number. Add variable with player name.

df_player <- my_season %>% 
  dplyr::mutate(points = total_points, gameweek =event) %>%
  dplyr::select(name, gameweek, points)

df_rival <- rival_season %>% 
  dplyr::mutate(points = total_points, gameweek =event) %>%
  dplyr::select(name, gameweek, points)

df_rival2 <- rival2_season %>% 
  dplyr::mutate(points = total_points, gameweek =event) %>%
  dplyr::select(name, gameweek, points)

df_rival3 <- rival3_season %>% 
  dplyr::mutate(points = total_points, gameweek =event) %>%
  dplyr::select(name, gameweek, points)

df <- rbind(df_player, df_rival, df_rival2, df_rival3) %>% 
  dplyr::mutate(name = factor(name)) %>%
  dplyr::filter(gameweek %notin% seq(30,38)) %>%
  dplyr::mutate(gameweek = as.integer(ifelse(gameweek > 38, gameweek -9 , gameweek)))


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

gganimate::anim_save("multiple_rival_race.gif")





###############################################
#### Alternate league race for rivals above ###
df<-df %>%
  dplyr::group_by(gameweek) %>%
  dplyr::mutate(ordering = rank(points,ties.method= "first")) %>%
  dplyr::ungroup() 

#function for barchart race
make_barchart_race <- function(title = "Title",
                               xlab = "x",
                               ylab = "y",
                               fps = 10,
                               end_pause = 100){
  p <- ggplot(data = df, 
              aes(x = ordering, 
                  y = points, 
                  fill = name, 
                  label = name)) +
    geom_bar(stat = "identity", colour = "black") +
    coord_flip(clip = "off", expand = FALSE) +
    geom_text(aes(label = name), hjust = -0.1) +
    labs(title = title,
         subtitle ='GW: {frame_time}',
         x = xlab,
         y = ylab,
         caption = caption) +
    ggthemes::theme_tufte(14) +
    theme(aspect.ratio = 4/3,
          legend.position = "none",
          plot.title = element_text(hjust = -0.1, size = 22),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_blank(),
          plot.margin = unit(c(2, 6, 2, 1),"cm")) +
    gganimate::transition_time(gameweek) +
    gganimate::ease_aes('cubic-in-out')
  
  gganimate::animate(p, 
                     nframes = fpgw * end_gw + end_pause, 
                     fps = fps, end_pause = end_pause, 
                     height = 675, 
                     width = 1200)
  
}

#Run function
make_barchart_race(title = "Title Race", 
                   xlab = xlab, 
                   ylab = ylab, 
                   fps = fps, 
                   end_pause = end_pause)




