library(fplscrapR)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(tweenr)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(animation)

#Set parameters 
start_gw <- 1
end_gw <- 38
player_limit <- 10
league_code <- 39075 #this is the overall league

#set animation paramaters
fpgw <- 10 #frames per GW
fps <- 15 #frames per second
end_pause <- 150

#set labels
xlab <- "FPL Manager"
ylab <- "Points"
caption <- "Top 10 as of GW38"

#Get league infromation
league <- get_league(leagueid=league_code)

#Put the key info into a data frame, keeping to a set limit of FPL players
df<-league$standings$results
df<-df %>%
  slice(1:player_limit)

#Create mini league data frame
df_ind <- data.frame() 
df_ml <- data.frame() #initalise with empty frame

for (i in 1:nrow(df)){
  df_ind <- get_entry_season(df$entry[i])
  df_ml <- rbind(df_ml,df_ind)}

df_ml <- df_ml %>% select(entry, name, event, points, total_points, overall_rank) 

#Create varaible to deal with ordering
df_ml<-df_ml %>%
  group_by(event) %>%
  mutate(ordering = rank(total_points,ties.method= "first")) %>%
  ungroup()

#function for barchart race
make_barchart_race <- function(title="Title",
                               xlab="x",
                               ylab="y",
                               fps=10,
                               end_pause=100){
  
  #Create bar chart race animation
  p<-ggplot(data = df_ml, aes(x = ordering, y = total_points, fill = name, label = name)) +
    geom_bar(stat = "identity", colour = "black") +
    coord_flip(clip = "off", expand = FALSE) +
    geom_text(aes(label = name), hjust = -0.1) +
    labs(title = title,
         subtitle ='GW: {frame_time}', x = xlab, y = ylab) +
    labs(caption = caption) +
    theme_tufte(14,"Avenir") +
    theme(aspect.ratio = 1,legend.position = "none",
          plot.title = element_text(hjust = -0.1, size = 22),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_blank(),
          plot.margin = unit(c(1,6,1,1),"cm")) +
    theme(aspect.ratio = 4/3) +
    transition_time(event) +
    ease_aes('cubic-in-out') 
  
  animate(p, nframes = fpgw * end_gw + end_pause, fps = fps, end_pause = end_pause)
  
}


#Run the function using the paramters we set at the top
make_barchart_race(title = league$league$name, 
                   xlab = xlab, ylab = ylab, fps = fps, end_pause = end_pause)

anim_save("mini_league.gif")