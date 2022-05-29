url <- "https://docs.google.com/spreadsheets/d/1QyUhnICs8utfDn-GKioxiBmoYpgfGcYIMmFpE1k8VOY/edit?usp=sharing"
input_data <- gsheet2tbl(url, sheetid = "21/22_Auction")
# Remove blank rows/columns
input_data <- input_data %>% 
  filter(!is.na(Manager)) %>% 
  select(-starts_with("X"))


library(fplscrapR)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(gifski)

`%notin%` <- Negate(`%in%`)

#Set parameters 
start_gw <- 1
end_gw <- 38
player_limit <- 20

#set animation paramaters
fpgw <- 7 #frames per GW
fps <- 10 #frames per second 
end_pause <- 70

#set labels
xlab <- "FPL Manager"
ylab <- "Points"
caption <- "League Position By Week"

df_ml <- input_data %>%
  select(Manager, Gameweek, GW_Score, Total_Pts,Overall_Rank) %>% 
  mutate(event = str_remove(Gameweek,"GW")) %>% 
  rename(name=Manager,
         points=GW_Score,
         total_points=Total_Pts,
         overall_rank=Overall_Rank) %>% 
  mutate(event=as.numeric(event))

#Create variable to deal with ordering
df_ml <- df_ml %>%
  dplyr::group_by(event) %>%
  dplyr::mutate(ordering = rank(total_points,ties.method= "first")) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(name) %>% 
  dplyr::mutate(priority = max(ordering)) %>% 
  dplyr::ungroup() %>% 
  select(name, name, event, points, total_points, overall_rank,priority)

min_output <- max(df_ml$priority) - 25  
df_ml <- df_ml %>% dplyr::filter(priority > min_output)

df_ml <- df_ml %>%
  dplyr::group_by(event) %>%
  dplyr::mutate(ordering = rank(total_points,ties.method= "first")) %>%
  dplyr::ungroup()

#function for barchart race
make_barchart_race <- function(title = "Title",
                               xlab = "x",
                               ylab = "y",
                               fps = 10,
                               end_pause = 100){
  p <- ggplot(data = df_ml, 
              aes(x = ordering, 
                  y = total_points, 
                  fill = name, 
                  label = name)) +
    geom_bar(stat = "identity", colour = "black") +
    coord_flip(clip = "off", expand = FALSE) +
    geom_text(aes(label = name), hjust = -0.1) +
    labs(title = title,
         subtitle ='GW: {round(frame_time,0)}',
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
    gganimate::transition_time(event) +
    gganimate::ease_aes('cubic-in-out')
  
  gganimate::animate(p, 
                     nframes = fpgw * end_gw + end_pause, 
                     fps = fps, end_pause = end_pause, 
                     height = 675, 
                     width = 1200)
  
}

#Run function 
make_barchart_race(title = "21/22 League", 
                   xlab = xlab, 
                   ylab = ylab, 
                   fps = fps, 
                   end_pause = end_pause)

#save to local directory
gganimate::anim_save("111111.gif")
