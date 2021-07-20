library(gsheet)
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
player_limit <- 4
league_code <- 5794

#set animation paramaters
fpgw <- 5 #frames per GW
fps <- 10 #frames per second 
end_pause <- 100

#set labels
xlab <- "FPL Manager"
ylab <- "Points"
caption <- "League Position By Week"


#####################################################
### Read in initial data & perform pre-processing ###
#####################################################
url <- 'https://docs.google.com/spreadsheets/d/1o3F63h36_J7btEEywRqxVKZKSsy4dhOKVtZGFYLTw8I/edit?usp=sharing'
input_data <- gsheet2tbl(url)
# Remove blank rows/columns
df_ml <- input_data %>% 
  filter(!is.na(event)) %>% 
  select(-starts_with("X"))

#Only select the columns we need
df_ml <- df_ml %>% 
  dplyr::select(name, event, points, total_points, overall_rank)

#Create variable to deal with ordering
df_ml<-df_ml %>%
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
make_barchart_race(title = league$league$name, 
                   xlab = xlab, 
                   ylab = ylab, 
                   fps = fps, 
                   end_pause = end_pause)

#save to local directory
gganimate::anim_save("cl_2020.gif")





