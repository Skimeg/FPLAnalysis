## League Captaincy Analysis ##
###############################
library(fplscrapR)
library(tidyverse)
library(ggplot2)

league_code <- 7317 #overall league
#Get league info#
league <- get_league(leagueid = league_code)
#######################################################

#Put key info into a data frame and set a limit on number of FPL players
df<-league$standings$results
entryid <- df %>% select(entry,player_name) %>% 
  rename(name=player_name)

#Create mini league data frame
df_ml <- df$entry %>% 
  purrr::map_dfr(~fplscrapR::get_entry_season(.x)) %>% 
  left_join(entryid,by="name")

#Get_entry_captain will freeze loading more than 1 or 2 players data at once,
#Therefore we pull each players captaincy data individually
captains1 <- fplscrapR:::get_entry_captain(df$entry[1],c(1:38))
captains2 <- fplscrapR:::get_entry_captain(df$entry[2],c(1:38))
captains3 <- fplscrapR:::get_entry_captain(df$entry[3],c(1:38))
captains4 <- fplscrapR:::get_entry_captain(df$entry[4],c(1:38))
captains5 <- fplscrapR:::get_entry_captain(df$entry[5],c(1:38))
captains6 <- fplscrapR:::get_entry_captain(df$entry[6],c(1:38))
captains7 <- fplscrapR:::get_entry_captain(df$entry[7],c(1:38))
captains8 <- fplscrapR:::get_entry_captain(df$entry[8],c(1:38))
captains9 <- fplscrapR:::get_entry_captain(df$entry[9],c(1:38))
captains10 <- fplscrapR:::get_entry_captain(df$entry[10],c(1:38))
captains11 <- fplscrapR:::get_entry_captain(df$entry[11],c(1:38))
captains12 <- fplscrapR:::get_entry_captain(df$entry[12],c(1:38))
captains13 <- fplscrapR:::get_entry_captain(df$entry[13],c(1:38))
captains14 <- fplscrapR:::get_entry_captain(df$entry[14],c(1:38))
captains15 <- fplscrapR:::get_entry_captain(df$entry[15],c(1:38))
captains16 <- fplscrapR:::get_entry_captain(df$entry[16],c(1:38))
captains17 <- fplscrapR:::get_entry_captain(df$entry[17],c(1:38))
captains18 <- fplscrapR:::get_entry_captain(df$entry[18],c(1:38))
captains19 <- fplscrapR:::get_entry_captain(df$entry[19],c(1:38))
captains20 <- fplscrapR:::get_entry_captain(df$entry[20],c(1:38))
captains21 <- fplscrapR:::get_entry_captain(df$entry[21],c(1:38))
captains22 <- fplscrapR:::get_entry_captain(df$entry[22],c(1:38))


df_captains <- rbind(captains1, captains2,
                     captains3, captains4,
                     captains5, captains6,
                     captains7, captains8,
                     captains9, captains10,
                     captains11,captains12,
                     captains13,captains14,
                     captains15,captains16,
                     captains17,captains18,
                     captains19,captains20,
                     captains21,captains22
                     )
df_cpt_sum <- df_captains %>%
  group_by(playername) %>%
  count() %>% 
  arrange(n)

df_cpt_pop <- df_cpt_sum %>% 
  mutate(playername=ifelse(n<10,"Other",playername)) %>% 
  group_by(playername) %>% 
  summarise(n = sum(n)) %>% 
  mutate(order = ifelse(playername=="Other",0,n),
         playername = ifelse(str_detect(playername,"Fernandes"),"Bruno Fernandes",playername),
         playername = ifelse(str_detect(playername,"Cristiano"),"Cristiano Ronaldo",playername))

# Barplot
dev.off()
p <-  ggplot(df_cpt_pop, aes(x="", y=n, fill=reorder(playername,order)))+
  ggtitle("Most Popular Captain Picks")+
  geom_bar(width = 1, stat = "identity")+ 
  coord_polar("y", start=0)+
  geom_text(aes(label = paste(playername), x = 1.3),
            position = position_stack(vjust = 0.5))+
  guides(fill = guide_legend(title = "Captains"))+
  theme_void()
print(p)


Captains_sum <- table(df_captains$playername)
View(Captains_sum)

write.csv(Captains_sum, "Roche_League_Captains_2024.csv")
save(Captains_sum,file = 'Roche_League_Captains_2024.Rda')










