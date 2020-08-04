####################################################
###    Roche Fantasy Premier League Analysis     ###
####################################################


# Load Packages
install.packages("gsheet")
install.packages("tidyverse")
install.packages("directlabels")
install.packages("formattable")
install.packages("expss")
install.packages("DT")
library(gsheet)
library(tidyverse)
library(ggplot2)
library(directlabels)
library(scales)
library(formattable)
library(expss)
library(DT)

#####################################################
### Read in initial data & perform pre-processing ###
#####################################################
url <- 'https://docs.google.com/spreadsheets/d/1v1U9bgTrwkOP5pE19zIq0IqEI4uj3XEzGBPZyHR23u0/edit#gid=413006008'
input_data <- gsheet2tbl(url)
# Remove blank rows/columns
input_data <- input_data %>% 
  filter(!is.na(Manager)) %>% 
  select(-starts_with("X"))


####################################################
### PLot Graphics                                ###
####################################################
############Classic league finishes#############
finishing_positions_wide <- input_data %>% 
  select(c("Manager",ends_with("Position"))) %>% 
  select(c("Manager",starts_with("Classic"))) 
# Additional code to select just managers from the current season
# finishing_positions_wide <- finishing_positions_wide %>% 
#   filter(!is.na(`Classic League 19/20 Position`))
# Transpose classic league finishes  
finishing_positions_long <- finishing_positions_wide %>% 
  gather(key="Seasons",value="Position","Classic League 12/13 Position":rev(names(finishing_positions_wide))[1]) %>% 
  mutate(Season = str_trim(substr(Seasons,15,20)))

# Plot league finishes
g_line_finishes <- ggplot(finishing_positions_long[!is.na(finishing_positions_long$Position), ],
                          aes(x=Season, y=Position, group=Manager, color=Manager))+
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ggtitle("Classic FPL Finishing Positions")+
  scale_y_reverse(breaks=c(1, 3, 6, 10), minor_breaks=c(2,4,8))+
  geom_dl(aes(label = Manager), method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.8))



p_meds <- summarise(group_by(finishing_positions_long[!is.na(finishing_positions_long$Position), ], Manager), MD = median(Position))
g_box_finishes <- ggplot(finishing_positions_long[!is.na(finishing_positions_long$Position), ],
                         aes(x=Manager, y=Position, color=Manager))+
  geom_boxplot()+
  theme(legend.position="none")+
  ggtitle("Classic FPL Finishing Positions")+
  scale_y_reverse(breaks=c(1, 3, 6, 10), minor_breaks=c(2,4,8))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_text(data = p_meds, aes(Manager, MD, label = Manager), 
            position = position_dodge(width = 0.8), size = 2.5, vjust = -0.5)



############Classic league points###########
finishing_points_wide <- input_data %>% 
  select(c("Manager",ends_with("Points"))) %>% 
  select(c("Manager",starts_with("Classic"))) 
# Transpose classic league points  
finishing_points_long <- finishing_points_wide %>% 
  gather(key="Seasons",value="Points","Classic League 12/13 Points":rev(names(finishing_points_wide))[1]) %>% 
  mutate(Season = str_trim(substr(Seasons,15,20)))

# Plot league points
g_line_points <- ggplot(finishing_points_long[!is.na(finishing_points_long$Points), ],
                           aes(x=Season, y=Points, group=Manager, color=Manager))+
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ggtitle("Classic FPL Points")+
  geom_dl(aes(label = Manager), method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.8))

p_meds <- summarise(group_by(finishing_points_long[!is.na(finishing_points_long$Points), ], Manager), MD = median(Points))
g_box_points <- ggplot(finishing_points_long[!is.na(finishing_points_long$Points), ],
                       aes(x=Manager, y=Points, color=Manager))+
  geom_boxplot()+
  theme(legend.position="none")+
  ggtitle("Classic FPL Points")+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(axis.text.x=element_blank())+
  geom_text(data = p_meds, aes(Manager, MD, label = Manager), 
            position = position_dodge(width = 0.8), size = 2.5, vjust = -0.5)





############Classic league profit/loss################
finishing_profit_wide <- input_data %>% 
  select(c("Manager",ends_with("Profit/Loss"))) %>% 
  select(c("Manager",starts_with("Classic"))) 
# Transpose profit/losses  
finishing_profit_long <- finishing_profit_wide %>% 
  gather(key="Seasons",value="Profits","Classic League 12/13 Profit/Loss":rev(names(finishing_profit_wide))[1]) %>% 
  mutate(Season = str_trim(substr(Seasons,15,20)),
         Profit = as.numeric(gsub("?", "",Profits)))

# Plot profits/losses
g_line_profit <- ggplot(finishing_profit_long[!is.na(finishing_profit_long$Profit), ],
                        aes(x=Season, y=Profit, group=Manager, color=Manager))+
  scale_y_continuous("Profit (?)")+
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ggtitle("Classic FPL Profit/Loss")+
  geom_dl(aes(label = Manager), method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.8))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")


p_meds <- summarise(group_by(finishing_profit_long[!is.na(finishing_profit_long$Profit), ], Manager), MD = median(Profit))
g_box_profit <- ggplot(finishing_profit_long[!is.na(finishing_profit_long$Profit), ],
                       aes(x=Manager, y=Profit, color=Manager))+
  geom_boxplot()+
  theme(legend.position="none")+
  ggtitle("Classic FPL Profit/Loss")+
  scale_y_continuous("Profit (?)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_text(data = p_meds, aes(Manager, MD, label = Manager), 
            position = position_dodge(width = 0.8), size = 2.5, vjust = -0.5)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

############Overall Rank################
overall_rank_wide <- input_data %>% 
  select(c("Manager",starts_with("Overall Rank"))) 
# Transpose overall ranks  
overall_rank_long <- overall_rank_wide %>% 
  gather(key="Seasons",value="OR","Overall Rank 2012/2013":rev(names(overall_rank_wide))[1]) %>% 
  mutate(Season = str_trim(paste(substr(Seasons,16,17),"/",substr(Seasons,21,22),sep = "")))

# Plot overall ranks
g_line_or <- ggplot(overall_rank_long[!is.na(overall_rank_long$OR), ],
                    aes(x=Season, y=OR, group=Manager, color=Manager))+
  scale_y_reverse("Overall Rank", labels=comma)+
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ggtitle("Overall Rank")+
  geom_dl(aes(label = Manager), method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.8))

p_meds <- summarise(group_by(overall_rank_long[!is.na(overall_rank_long$OR), ], Manager), MD = median(OR))
g_box_or <- ggplot(overall_rank_long[!is.na(overall_rank_long$OR), ],
                   aes(x=Manager, y=OR, color=Manager))+
  geom_boxplot()+
  theme(legend.position="none")+
  ggtitle("Classic FPL Overall Rank")+
  scale_y_reverse("Overall Rank (OR)", labels=comma)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_text(data = p_meds, aes(Manager, MD, label = Manager), 
            position = position_dodge(width = 0.8), size = 2.5, vjust = -0.5)


############Champions League Finishes#############
champions_positions_wide <- input_data %>% 
  select(c("Manager",ends_with("Position"))) %>% 
  select(c("Manager",starts_with("Champions"))) 
# Transpose champions league finishes  
champions_positions_long <- champions_positions_wide %>% 
  gather(key="Seasons",value="CLPosition","Champions League 15/16 Position":rev(names(champions_positions_wide))[1]) %>% 
  mutate(Season = str_trim(substr(Seasons,18,23)))

# Plot champions league finishes
g_line_clfinishes <- ggplot(champions_positions_long[!is.na(champions_positions_long$CLPosition), ],
                            aes(x=Season, y=CLPosition, group=Manager, color=Manager))+
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ggtitle("Champions League Finishing Positions")+
  scale_y_reverse()+
  geom_dl(aes(label = Manager), method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.8))

p_meds <- summarise(group_by(champions_positions_long[!is.na(champions_positions_long$CLPosition), ], Manager), MD = median(CLPosition))
g_box_clfinishes <- ggplot(champions_positions_long[!is.na(champions_positions_long$CLPosition), ],
                           aes(x=Manager, y=CLPosition, color=Manager))+
  geom_boxplot()+
  theme(legend.position="none")+
  ggtitle("Champions League Finishes")+
  scale_y_reverse("Position", labels=comma)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_text(data = p_meds, aes(Manager, MD, label = Manager), 
            position = position_dodge(width = 0.8), size = 2.5, vjust = -0.5)


############Champions league points###########
champions_points_wide <- input_data %>% 
  select(c("Manager",ends_with("Points"))) %>% 
  select(c("Manager",starts_with("Champions"))) 
# Transpose champions league points  
champions_points_long <- champions_points_wide %>% 
  gather(key="Seasons",value="CLPoints","Champions League 15/16 Points":rev(names(champions_points_wide))[1]) %>% 
  mutate(Season = str_trim(substr(Seasons,18,23),side=c("both")))

# Plot league points
g_line_clpoints <- ggplot(champions_points_long[!is.na(champions_points_long$CLPoints), ],
                             aes(x=Season, y=CLPoints, group=Manager, color=Manager))+
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ggtitle("Champions League Points")+
  geom_dl(aes(label = Manager), method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.8))

p_meds <- summarise(group_by(champions_points_long[!is.na(champions_points_long$CLPoints), ], Manager), MD = median(CLPoints))
g_box_clpoints <- ggplot(champions_points_long[!is.na(champions_points_long$CLPoints), ],
                         aes(x=Manager, y=CLPoints, color=Manager))+
  geom_boxplot()+
  theme(legend.position="none")+
  ggtitle("Champions League Points")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_text(data = p_meds, aes(Manager, MD, label = Manager), 
            position = position_dodge(width = 0.8), size = 2.5, vjust = -0.5)




####################################################
### Summary Data Pre Processing                  ###
####################################################
long_data <- finishing_positions_long[c("Manager","Season","Position")] %>% 
  left_join(finishing_points_long[c("Manager","Season","Points")],by=c("Manager","Season")) %>% 
  left_join(finishing_profit_long[c("Manager","Season","Profit")],by=c("Manager","Season")) %>%
  left_join(overall_rank_long[c("Manager","Season","OR")],by=c("Manager","Season")) %>%
  left_join(champions_positions_long[c("Manager","Season","CLPosition")],by=c("Manager","Season")) %>%
  left_join(champions_points_long[c("Manager","Season","CLPoints")],by=c("Manager","Season"))

seasons <- long_data %>% 
  group_by(Manager) %>%
  filter(!is.na(Position)) %>% 
  summarise(Seasons=n())

titles <- long_data %>% 
  group_by(Manager) %>% 
  filter(Position==1) %>% 
  summarise(Titles=n())

top_3 <- long_data %>% 
  group_by(Manager) %>% 
  filter(Position<=3) %>% 
  summarise(Top_3=n())

top_6 <- long_data %>% 
  group_by(Manager) %>% 
  filter(Position<=6) %>% 
  summarise(Top_6=n())

over_6 <- long_data %>% 
  group_by(Manager) %>% 
  filter(Position>6) %>% 
  summarise(Over_6=n())

top_10ks <- long_data %>% 
  group_by(Manager) %>% 
  filter(OR<=10000) %>% 
  summarise(Top_10k_Finishes=n())

top_100ks <- long_data %>% 
  group_by(Manager) %>% 
  filter(OR<=100000) %>% 
  summarise(Top_100k_Finishes=n())

top_1ms <- long_data %>% 
  group_by(Manager) %>% 
  filter(OR<=1000000) %>% 
  summarise(Top_1m_Finishes=n())

over_1ms <- long_data %>% 
  group_by(Manager) %>% 
  filter(OR>1000000) %>% 
  summarise(Over_1m_Finishes=n())

CL_seasons <- long_data %>% 
  group_by(Manager) %>%
  filter(!is.na(CLPosition)) %>% 
  summarise(CL_seasons=n())

CL_titles <- long_data %>% 
  group_by(Manager) %>% 
  filter(CLPosition==1) %>% 
  summarise(CL_Titles=n())

CL_top_3 <- long_data %>% 
  group_by(Manager) %>% 
  filter(CLPosition<=3) %>% 
  summarise(CL_Top_3=n())

position_data <- seasons %>% 
  left_join(titles,by=c("Manager")) %>% 
  left_join(top_3,by=c("Manager")) %>% 
  left_join(top_6,by=c("Manager")) %>% 
  left_join(over_6,by=c("Manager")) %>% 
  left_join(CL_seasons,by=c("Manager")) %>% 
  left_join(CL_titles,by=c("Manager")) %>% 
  left_join(CL_top_3,by=c("Manager")) %>% 
  replace(is.na(.), 0) %>% 
  filter(!(Manager %in% c("Median","Average"))) %>% 
  arrange(desc(Titles),desc(Top_3),desc(Top_6),Over_6,desc(CL_Titles),desc(CL_Top_3),CL_seasons)

finish_data <- long_data %>% 
  group_by(Manager) %>% 
  summarise(Best_Finish=min(Position,na.rm=TRUE),
            Median_Finish=median(Position,na.rm=TRUE),
            Average_Finish=round(mean(Position,na.rm=TRUE),2),
            Worst_Finish=max(Position,na.rm=TRUE))

points_data <- long_data %>% 
  group_by(Manager) %>% 
  summarise(Most_Points=max(Points,na.rm=TRUE),
            Median_Points=median(Points,na.rm=TRUE),
            Average_Points=round(mean(Points,na.rm=TRUE),2),
            Fewest_Points=min(Points,na.rm=TRUE),
            Total_Points=sum(Points,na.rm=TRUE))

returns_data <- long_data %>% 
  group_by(Manager) %>% 
  summarise(Best_Return=max(Profit,na.rm=TRUE),
            Median_Returns=median(Profit,na.rm=TRUE),
            Average_Return=round(mean(Profit,na.rm=TRUE),0),
            Worst_Return=min(Profit,na.rm=TRUE),
            Total_Returns=sum(Profit,na.rm=TRUE))

OR_data <- long_data %>% 
  group_by(Manager) %>% 
  filter(!is.na(OR)) %>% 
  summarise(Best_Rank=round(min(OR,na.rm=TRUE),0),
            Median_Rank=median(OR,na.rm=TRUE),
            Average_Rank=round(mean(OR,na.rm=TRUE),2),
            Worst_Rank=round(max(OR,na.rm=TRUE),0)) %>% 
  left_join(top_10ks,by=c("Manager")) %>% 
  left_join(top_100ks,by=c("Manager")) %>% 
  left_join(top_1ms,by=c("Manager")) %>% 
  left_join(over_1ms,by=c("Manager")) %>% 
  replace(is.na(.), 0)

summary_data <- position_data %>% 
  left_join(finish_data,by=c("Manager")) %>% 
  left_join(points_data,by=c("Manager")) %>% 
  left_join(returns_data,by=c("Manager")) %>% 
  left_join(OR_data,by=c("Manager")) 

summary_data_labels = c("Manager","Seasons Played","Titles Won","Top 3 Finishes",
                        "Top 6 Finishes","Finishes Outside the Top 6","Champions League Seasons Played",
                        "Champions League Titles Won","Champions League Top 3 Finishes","Best Finish",
                        "Median Finish","Average Finish","Worst Finish","Most Points","Median Points",
                        "Average Points","Fewest Points","Total Points","Best Return","Median Returns",
                        "Average Returns","Worst Return","Overall Profit/Loss","Best Overall Rank",
                        "Median Overall Rank","Average Overall Rank","Worst Overall Rank",
                        "Top 10k Finishes","Top 100k Finishes","Top 1m Finishes","Over 1m Finishes")
attr(summary_data,'label') <- summary_data_labels
###########Output all Summary Data as a Table
datatable(summary_data, colnames = attr(summary_data,'label'))




###########Hall of Fame & Grand Tables
HOF_data <- summary_data %>% 
  select(c('Manager','Titles','Top_3','Top_6','Top_10k_Finishes','Top_100k_Finishes','Top_1m_Finishes','CL_Titles','CL_Top_3')) %>% 
  filter(Top_6>0|Top_1m_Finishes>0) %>% 
  arrange(desc(Titles),desc(Top_3),desc(Top_6),
          desc(Top_10k_Finishes),desc(Top_100k_Finishes),desc(Top_1m_Finishes),
          desc(CL_Titles),desc(CL_Top_3)) %>% 
  rename('Number of Titles'='Titles',
         'Top 3 Finishes'='Top_3',
         'Top 6 Finishes'='Top_6',
         'Top 10k Finishes'='Top_10k_Finishes',
         'Top 100k Finishes'='Top_100k_Finishes',
         'Top 1m Finishes'='Top_1m_Finishes',
         'Champions League Titles'='CL_Titles',
         'Champions League Top 3 Finishes'='CL_Top_3') %>% 
  replace(is.na(.), 0)


HOF_table <- formattable(HOF_data,
                         align =c("l","c","c","c","c","c","c","c","c"),
                         list(`Manager` = formatter("span", style = ~ style(color = "grey",
                                                                            font.weight = "bold"
                         )),
                         `Number of Titles` = formatter("span",
                                                        style = ~ style(
                                                          font.weight = "bold",
                                                          display = "inline-block",
                                                          width = percent(proportion(`Number of Titles`)),
                                                          "background-color" = csscolor(gradient(`Number of Titles`, "green", "green3"))
                                                        )),
                         `Top 3 Finishes` = formatter("span",
                                                      style = ~ style(
                                                        display = "inline-block",
                                                        width = percent(proportion(`Top 3 Finishes`)),
                                                        "background-color" = csscolor(gradient(`Top 3 Finishes`, "olivedrab1", "lightgreen"))
                                                      )),
                         `Top 6 Finishes` = formatter("span",
                                                      style = ~ style(
                                                        display = "inline-block",
                                                        width = percent(proportion(`Top 6 Finishes`)),
                                                        "background-color" = csscolor(gradient(`Top 6 Finishes`, "yellow", "lightgreen"))
                                                      )),
                         `Top 10k Finishes` = formatter("span",
                                                        style = ~ style(
                                                          font.weight = "bold",
                                                          display = "inline-block",
                                                          width = percent(proportion(`Top 10k Finishes`)),
                                                          "background-color" = csscolor(gradient(`Top 10k Finishes`, "green2", "green3"))
                                                        )),
                         `Top 100k Finishes` = formatter("span",
                                                         style = ~ style(
                                                           display = "inline-block",
                                                           width = percent(proportion(`Top 100k Finishes`)),
                                                           "background-color" = csscolor(gradient(`Top 100k Finishes`, "olivedrab1", "lightgreen"))
                                                         )),
                         `Top 1m Finishes` = formatter("span",
                                                       style = ~ style(
                                                         display = "inline-block",
                                                         width = percent(proportion(`Top 1m Finishes`)),
                                                         "background-color" = csscolor(gradient(`Top 1m Finishes`, "yellow", "lightgreen"))
                                                       )),
                         `Champions League Titles` = formatter("span",
                                                               style = ~ style(
                                                                 font.weight = "bold",
                                                                 display = "inline-block",
                                                                 width = percent(proportion(`Champions League Titles`)),
                                                                 "background-color" = csscolor(gradient(`Champions League Titles`, "lightgreen", "green3"))
                                                               )),
                         `Champions League Top 3 Finishes` = formatter("span",
                                                                       style = ~ style(
                                                                         display = "inline-block",
                                                                         width = percent(proportion(`Champions League Top 3 Finishes`)),
                                                                         "background-color" = csscolor(gradient(`Champions League Top 3 Finishes`, "olivedrab1", "lightgreen"))
                                                                       ))
                         ))


overall_rank_table_data <- long_data %>% 
  select(c("Manager", "Season","Position","OR")) %>% 
  filter(!is.na(OR) & !is.na(Position)) %>% 
  arrange(OR,Position,desc(Season)) %>% 
  mutate(OR = round(OR,0),
         Position = round(Position,0)) %>% 
  rename('Overall Rank' = 'OR')

OR_table <- formattable(overall_rank_table_data,
                        align =c("l","c","c","c"),
                        list(`Manager` = formatter("span", 
                                                   style = ~ style(
                                                     color = "grey",
                                                     font.weight = "bold"
                                                   )),
                             `Position` = formatter("span",
                                                    style = ~ style(
                                                      font.weight = "bold",
                                                      display = "inline-block",
                                                      width = percent(proportion(1/log(1000*`Position`))),
                                                      "background-color" = csscolor(gradient(3/`Position`, "transparent", "green"))
                                                    )),
                             `Overall Rank` = formatter("span",
                                                        style = ~ style(
                                                          display = "inline-block",
                                                          width = percent(proportion(1/log(`Overall Rank`))),
                                                          "background-color" = csscolor(gradient(1/log(`Overall Rank`), "transparent", "green"))
                                                        ))
                        ))

grand_points_table_data <- long_data %>% 
  select(c("Manager", "Season","Position","Points")) %>% 
  filter(!is.na(Points) & !is.na(Position)) %>% 
  arrange(desc(Points),Position,desc(Season)) %>% 
  mutate(Points = round(Points,0),
         Position = round(Position,0))

Points_table <- formattable(grand_points_table_data,
                            align =c("l","c","c","c"),
                            list(`Manager` = formatter("span", 
                                                       style = ~ style(
                                                         color = "grey",
                                                         font.weight = "bold"
                                                       )),
                                 `Position` = formatter("span",
                                                        style = ~ style(
                                                          font.weight = "bold",
                                                          display = "inline-block",
                                                          width = percent(proportion(1/log(1000*`Position`))),
                                                          "background-color" = csscolor(gradient(3/`Position`, "transparent", "green"))
                                                        )),
                                 `Points` = formatter("span",
                                                      style = ~ style(
                                                        display = "inline-block",
                                                        width = percent(proportion(`Points`)),
                                                        "background-color" = csscolor(gradient(`Points`, "transparent", "lightgreen"))
                                                      ))
                            ))

grand_clpoints_table_data <- long_data %>% 
  select(c("Manager", "Season","CLPosition","CLPoints")) %>% 
  filter(!is.na(CLPoints) & !is.na(CLPosition)) %>% 
  arrange(desc(CLPoints),CLPosition,desc(Season)) %>% 
  mutate(CLPoints = round(CLPoints,0),
         CLPosition = round(CLPosition,0)) %>% 
  rename("Champions League Points" = "CLPoints",
         "Champions League Position" = "CLPosition")

Champions_points_table <- formattable(grand_clpoints_table_data,
                                      align =c("l","c","c","c"),
                                      list(`Manager` = formatter("span", 
                                                                 style = ~ style(
                                                                   color = "grey",
                                                                   font.weight = "bold"
                                                                 )),
                                           `Champions League Position` = formatter("span",
                                                                                   style = ~ style(
                                                                                     font.weight = "bold",
                                                                                     display = "inline-block",
                                                                                     width = percent(proportion(1/`Champions League Position`)),
                                                                                     "background-color" = csscolor(gradient(3/`Champions League Position`, "transparent", "green"))
                                                                                   )),
                                           `Champions League Points` = formatter("span",
                                                                                 style = ~ style(
                                                                                   display = "inline-block",
                                                                                   width = percent(proportion(`Champions League Points`)),
                                                                                   "background-color" = csscolor(gradient(`Champions League Points`/10, "transparent", "lightgreen"))
                                                                                 ))
                                      ))


####