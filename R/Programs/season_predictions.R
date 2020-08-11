## Various Season Predictions
#Organise data into one long dataset
`%notin%` <- Negate(`%in%`)
f_pos_l <-  finishing_positions_long %>% select('Manager', 'Season', 'Position')
f_pts_l <-  finishing_points_long %>% select('Manager', 'Season', 'Points')
f_prf_l <-  finishing_profit_long %>% select('Manager', 'Season', 'Profits')
o_rnk_l <-  overall_rank_long %>% select('Manager', 'Season', 'OR')
c_pts_l <-  champions_points_long %>% select('Manager', 'Season', 'CLPoints')

dfl <- f_pos_l %>% 
  left_join(f_pts_l, by=c("Manager", "Season")) %>% 
  left_join(f_prf_l, by=c("Manager", "Season")) %>% 
  left_join(o_rnk_l, by=c("Manager", "Season")) %>% 
  left_join(c_pts_l, by=c("Manager", "Season")) %>% 
  filter(Manager %notin% c("Median", "Average") &
           !is.na(Position) & !is.na(Profits))

## In progress - wide df for machine learning
df_w <- finishing_positions_wide %>% 
  left_join(finishing_points_wide, by=c("Manager")) %>% 
  left_join(finishing_profit_wide, by=c("Manager")) %>% 
  left_join(overall_rank_wide, by=c("Manager")) %>% 
  left_join(champions_points_wide, by=c("Manager"))


## 0. Linear model: (How) Does league position impact points?
dfy <- dfl$Points
dfx <- dfl$Position
# 0.1 Build linear model 
model <- lm(dfy ~ dfx, data = dfl)
summary(model)
# 0.2. Add predictions 
pred.int <- predict(model, interval = "prediction")
mydata <- cbind(dfl, pred.int)
# 0.3. Regression line + confidence intervals
library("ggplot2")
p <- ggplot(mydata, aes(dfx, dfy)) +
  geom_point() +
  stat_smooth(method = lm)
# 0.4. Add prediction intervals
p + geom_line(aes(x = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(x = upr), color = "red", linetype = "dashed") +
  scale_x_continuous(trans = "reverse", breaks = unique(dfx), limits=c(15,1))+
  ylim(1500,2500)








