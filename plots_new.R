total_distance$datetime <- anytime(total_distance$time.x)
total_distance$datetime <- as.POSIXct(total_distance$datetime ,format="%Y/%m/%d %H:%M:%S",tz=Sys.timezone())


distance_df  <-  total_distance %>%  
  #filter(day == 1) %>%
  group_by(cowid.x,month=floor_date(datetime, "2minutes")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x))
## feeding = sum(feeding), milking = sum(milking)  )

distance_df

distance_df$feeding <- ifelse(
  ( 
    (distance_df$new_x > 0.25) &
      (distance_df$new_x < 58.65) &
      (distance_df$new_y > 10.5) &
      (distance_df$new_y < 17.2)
  ),
  1,  # if condition is met, put 1
  0   # else put 0
)

distance_df$milking <- ifelse(
  ( 
    (distance_df$new_x > 29.4) &
      (distance_df$new_x < 42.25) &
      (distance_df$new_y > -8.42) &
      (distance_df$new_y < -1.62)
  ),
  1,  # if condition is met, put 1
  0   # else put 0
)


distance_df$lame <- ifelse(
  ( 
    (distance_df$cowid == '1078' ) |
      (distance_df$cowid == '1184' ) |
      (distance_df$cowid == '1340' ) |
      (distance_df$cowid == '1891' ) |
      (distance_df$cowid == '2003' ) |
      (distance_df$cowid == '2010' ) |
      (distance_df$cowid == '2060' ) |
      (distance_df$cowid == '2302' ) |
      (distance_df$cowid == '2344' ) |
      (distance_df$cowid == '2616' ) 
  ),
  1,  # if condition is met, put 1
  0   # else put 0
)


unique(distance_df$cowid)

#### Populate  heatmpa for Lame and nonlame cows


df_2003   = df[df['cowid'] == '2003',]
df_1891   = df[df['cowid'] == '1891',]
df_1892   = df[df['cowid'] == '1892',]
df_2153   = df[df['cowid'] == '2153',]
df_2959   = df[df['cowid'] == '2959',]


ggplot(df_2003, aes(x=x, y=y) ) +
  xlim(-5, 65) +
  ylim(-10, 30) +
  # geom_rect(xmin = 0.25, xmax = 58.65,   ymin =10.5, ymax = 17.2,   fill = "grey", alpha= 0.1)  +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette = "Set1", direction=1) + # + bwmap()
  
  annotate("rect", xmin = 0.25, xmax = 58.65,   ymin =10.5, ymax = 17.2,col = 'black', fill = NA)  +
  annotate("rect", xmin = 29.4, xmax = 42.5,   ymin =-8.42, ymax = -1.62,col = 'black', fill = NA)  +
  # annotate("rect", xmin = 0.25, xmax = 58.65,   ymin =10.5, ymax = 17.2, fill = "gray", alpha = 0.5)# + 
  # annotate("rect", xmin = 0, xmax = 60,   ymin =-10, ymax = 30,col = 'black', fill = NA) #+ 
  #annotate("rect", xmin = 29.4, xmax = 42.5,   ymin =-8.42, ymax = -1.62,fill = 'gray',  alpha = 0.5) #+
  annotate("segment", x = 0, xend = 60,y = 10, yend = 10,col = 'black', fill = NA) + ggtitle("Lame cow 2003") +
  xlab("X-coordinatae in meters") + ylab("Y-coordinatae in meters") + 
  theme(plot.title = element_text(size = 20, face = "bold"))



########################
#distance_df$feeding <- distance_df$feeding  * 2
#distance_df$milking <- distance_df$milking  * 2

#distance_df

distance_df_per_day  <-  distance_df %>%  
  #filter(day == 1) %>%
  group_by(cowid.x,day) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(new_x),new_y=mean(new_y),  distance = sum(distance),  actm = mean(actm), milking = sum(milking),  feeding = sum(feeding))
## feeding = sum(feeding), milking = sum(milking)  )

distance_df_per_cow  <-  distance_df %>%  
  #filter(day == 1) %>%
  group_by(cowid.x) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(new_x),new_y=mean(new_y), day = mean(day) , distance = mean(distance),  actm = mean(actm), milking = sum(milking),  feeding = sum(feeding))
## feeding = sum(feeding), milking = sum(milking)  )


distance_df_per_day$lame <- ifelse(
  ( 
    (distance_df_per_day$cowid == '1078' ) |
      (distance_df_per_day$cowid == '1184' ) |
      (distance_df_per_day$cowid == '1340' ) |
      (distance_df_per_day$cowid == '1891' ) |
      (distance_df_per_day$cowid == '2003' ) |
      (distance_df_per_day$cowid == '2010' ) |
      (distance_df_per_day$cowid == '2060' ) |
      (distance_df_per_day$cowid == '2302' ) |
      (distance_df_per_day$cowid == '2344' ) |
      (distance_df_per_day$cowid == '2616' ) 
  ),
  'lame',  # if condition is met, put 1
  'nonlame'    # else put 0
)

distance_df_per_cow2$lame <- ifelse(
  ( 
    (distance_df_per_cow2$cowid == '1078' ) |
      (distance_df_per_cow2$cowid == '1184' ) |
      (distance_df_per_cow2$cowid == '1340' ) |
      (distance_df_per_cow2$cowid == '1891' ) |
      (distance_df_per_cow2$cowid == '2003' ) |
      (distance_df_per_cow2$cowid == '2010' ) |
      (distance_df_per_cow2$cowid == '2060' ) |
      (distance_df_per_cow2$cowid == '2302' ) |
      (distance_df_per_cow2$cowid == '2344' ) |
      (distance_df_per_cow2$cowid == '2616' ) 
  ),
  'lame',  # if condition is met, put 1
  'nonlame'   # else put 0
)



distance_df_per_cow2  <-  distance_df_per_day %>%  
  #filter(day == 1) %>%
  group_by(cowid.x) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(new_x),new_y=mean(new_y), day = mean(day) , distance = mean(distance),  actm = mean(actm), milking = mean(milking),  feeding = mean(feeding))
## feeding = sum(feeding), milking = sum(milking)  )

##################

###Feeding

distance_df_per_cow2$cowid <- as.factor(distance_df_per_cow2$cowid)
distance_df_per_day$cowid <- as.factor(distance_df_per_day$cowid)
distance_df_per_day$day <- as.factor(distance_df_per_day$day)

ggplot(distance_df_per_day, aes(x = day, y = feeding,fill =lame)) +
  geom_boxplot(size = 1) + 
  geom_jitter(aes(colour = cowid),size =1)+
  scale_fill_brewer(palette="Set3") + 
  ggtitle("Feeding duration between days") +
  xlab("Days") + ylab("Feeding duration in minutes") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()

ggplot(distance_df_per_cow2, aes(x = lame, y = feeding,fill =lame)) +
  geom_boxplot(size = 1) + 
  geom_jitter(aes(colour = cowid),size =4)+
  scale_fill_brewer(palette="Set3") + 
  ggtitle("Feeding duration in Minutes") +
  xlab("Lameness Flag") + ylab("Feeding duration in minutes") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()

###Milking

ggplot(distance_df_per_day, aes(x = day, y = milking,fill =lame)) +
  geom_boxplot(size = 1) + 
  geom_jitter(aes(colour = cowid),size =1)+
  scale_fill_brewer(palette="Pastel1") + 
  ggtitle("Milking duration between days") +
  xlab("Days") + ylab("Milking duration over 5 days") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()

ggplot(distance_df_per_cow2, aes(x = lame, y = milking,fill =lame)) +
  geom_boxplot(size = 1) + 
  geom_jitter(aes(colour = cowid),size =4)+
  scale_fill_brewer(palette="Pastel1") + 
  ggtitle("Milking duration in Minutes") +
  xlab("Lameness Flag") + ylab("Milking duration in minutes") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()


###Distance 
ggplot(distance_df_per_day, aes(x = day, y = distance,fill =lame)) +
  geom_boxplot(size = 1) + 
  geom_jitter(aes(colour = cowid),size =1)+
  scale_fill_brewer(palette="Pastel1") + 
  ggtitle("Milking duration between days") +
  xlab("Days") + ylab("Milking duration over 5 days") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()


ggplot(distance_df_per_cow2, aes(x = lame, y = distance,fill =lame)) +
  geom_boxplot(size = 1) + 
  geom_jitter(aes(colour = cowid),size =4)+
  scale_fill_brewer(palette="Paired") + 
  ggtitle("Milking duration in Minutes") +
  xlab("Lameness Flag") + ylab("Milking duration in minutes") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()

###acitivity

ggplot(distance_df_per_day, aes(x = day, y = actm,fill =lame)) +
  geom_boxplot(size = 1) + 
  geom_jitter(aes(colour = cowid),size =1)+
  scale_fill_brewer(palette="Accent") + 
  ggtitle("Mean accelerometer activity") +
  xlab("Days") + ylab("Mean accelerometer acitvity tracking over 5 days") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()

ggplot(distance_df_per_cow2, aes(x = lame, y = actm,fill =lame)) +
  geom_boxplot(size = 1) + 
  geom_jitter(aes(colour = cowid),size =2)+
  scale_fill_brewer(palette="Accent") + 
  ggtitle("Mean accelerometer activity") +
  xlab("Lameness Flag") + ylab("Mean accelerometer acitvity tracking ") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()

### Coordinate analysis
#library(gridExtra)
p1<- ggplot(distance_df_per_day, aes(x = day, y = new_x,fill =lame)) +
  geom_boxplot(size = 1) + 
  geom_jitter(aes(colour = cowid),size =1)+
  scale_fill_brewer(palette="Spectral") + 
  ggtitle("Mean X-coordinate") +
  xlab("Days") + ylab("Mean X-coordinate values") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()

p2 <- ggplot(distance_df_per_cow2, aes(x = lame, y = new_x,fill =lame)) +
  geom_boxplot(size = 1) + 
 # geom_jitter(aes(colour = cowid),size =2)+
  scale_fill_brewer(palette="Spectral") + 
  ggtitle("Mean X-coordinate values") +
  xlab("Lameness Flag") + ylab("Mean X-coordinate values ") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()

grid.arrange(p1, p2, nrow = 2)


q1<- ggplot(distance_df_per_day, aes(x = day, y = new_y,fill =lame)) +
  geom_boxplot(size = 1) + 
  geom_jitter(aes(colour = cowid),size =1)+
  scale_fill_brewer(palette="BrBG") + 
  ggtitle("Mean Y-coordinate") +
  xlab("Days") + ylab("Mean Y-coordinate values") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()

q2 <- ggplot(distance_df_per_cow2, aes(x = lame, y = new_y,fill =lame)) +
  geom_boxplot(size = 1) + 
  # geom_jitter(aes(colour = cowid),size =2)+
  scale_fill_brewer(palette="BrBG") + 
  ggtitle("Mean Y-coordinate values") +
  xlab("Lameness Flag") + ylab("Mean Y-coordinate values ") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()

grid.arrange(q1, q2, nrow = 2)

############# Actm and feeding#

feeding_data$cowid <- as.factor(feeding_data$cowid)
feeding_data$day <- as.factor(feeding_data$day)
feeding_data$feeding <- as.factor(feeding_data$feeding)

distance_df %>% filter(feeding == 2 ) ->  feeding_data

#feeding_data
ggplot(feeding_data, aes(x = day, y = actm,fill =day)) +
  geom_boxplot(size = 1)+
  scale_fill_brewer(palette="Accent")+ 
  ggtitle("Mean accelerometer activity over days") +
  xlab("Days") + ylab("Mean accelerometer acitvity tracking ") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme_bw()



############## Logistic Regression
distance_df_per_day
distance_df_per_day$lame <- ifelse(
  ( 
    (distance_df_per_day$cowid == '1078' ) |
      (distance_df_per_day$cowid == '1184' ) |
      (distance_df_per_day$cowid == '1340' ) |
      (distance_df_per_day$cowid == '1891' ) |
      (distance_df_per_day$cowid == '2003' ) |
      (distance_df_per_day$cowid == '2010' ) |
      (distance_df_per_day$cowid == '2060' ) |
      (distance_df_per_day$cowid == '2302' ) |
      (distance_df_per_day$cowid == '2344' ) |
      (distance_df_per_day$cowid == '2616' ) 
  ),
  1,  # if condition is met, put 1
  0    # else put 0
)

library(MASS)
#install.packages("olsrr")
library(olsrr)
fit <- glm(lame ~ new_x + new_y +  distance + actm + feeding +  milking,data=distance_df_per_day)

step <- stepAIC(fit, direction="both")
step$anova # display results
ols_step_all_possible(fit)


newmodel <- glm(lame ~ new_x + new_y +  distance  ,data=distance_df_per_day)

selectedMod <- step(newmodel)
summary(selectedMod)

all_vifs <- (selectedMod)
print(all_vifs)

new <- glm(lame ~ new_x + new_y +  distance + actm + feeding +  milking, data=distance_df_per_day)

step <- stepAIC(new, direction="both")
step$anova # display results
summary(new)

new2 <- glm(lame ~ new_x + new_y +  distance + actm + feeding , data=distance_df_per_day)

summary(new2)





new_test <- within(distance_df_per_day, rm(lame))
probabilities <- new2 %>% predict(new_test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

table(predicted.classes , distance_df_per_day$lame)





new2 <- glm(lame ~ new_x + new_y +  distance + actm + feeding , data=distance_df_per_cow2)

summary(new2)





new_test <- within(distance_df_per_cow2, rm(lame))
probabilities <- new2 %>% predict(new_test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

table(predicted.classes , distance_df_per_cow2$lame)



