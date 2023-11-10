get_league <- function(leagueid = NULL, leaguetype = "classic"){
  if(is.null(leagueid)) stop("You'll need to input a league ID, mate.")
  if(length(leagueid) != 1) stop("One league at a time, please.")
  {
    league <- jsonlite::fromJSON(paste("https://fantasy.premierleague.com/api/leagues-",leaguetype,"/",leagueid,"/standings/",sep=""))
    return(league)
  }
}



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
league_code <- 583303 #overall league

#set animation paramaters
fpgw <- 9 #frames per GW
fps <- 10 #frames per second 
end_pause <- 100

#set labels
xlab <- "FPL Manager"
ylab <- "Points"
caption <- "League Position By Week"

#Get league info#
league <- fplscrapR::get_league(leagueid = league_code)
#######################################################

#Put key info into a data frame and set a limit on number of FPL players
df<-league$standings$results

df<-df %>%
  head(player_limit)

#Create mini league data frame
df_ml <- df$entry %>% 
  purrr::map_dfr(~fplscrapR::get_entry_season(.x))

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
    gganimate::transition_time(event) +
    gganimate::ease_aes('cubic-in-out')
  
  gganimate::animate(p, 
                     nframes = fpgw * end_gw + end_pause, 
                     fps = fps, end_pause = end_pause, 
                     height = 675, 
                     width = 1200)
  
}

#Run function
plot1 <- make_barchart_race(title = league$league$name, 
                   xlab = xlab, 
                   ylab = ylab, 
                   fps = fps, 
                   end_pause = end_pause)
animate(plot1, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("league_2023.gif"))



#save to local directory
gganimate::anim_save("league_2023.gif")






