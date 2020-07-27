install.packages('sp')
install.packages('raster')
install.packages('parallel')
install.packages('ggplot2')
install.packages('lubridate')
install.packages('ggpubr')
install.packages('anytime')
install.packages("reshape")
install.packages("gridExtra")
install.packages("caret")

library(sp)
library(raster)
library(parallel)
library(reshape)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(anytime)
library(dplyr)
library(gridExtra)

### Load files

df <- read.csv('M:\\Research\\File_cow_location_activity_data.csv')


### Convert to datetime format

df$datetime <- anytime(df$time)
df$datetime <- as.POSIXct(df$datetime ,format="%Y/%m/%d %H:%M:%S",tz=Sys.timezone())

### Remove NA values 
df[complete.cases(df), ] -> df

unique(df$cowid)


#####################################################

df$ID <- seq.int(nrow(df))

df2 <- df %>% mutate_all(.funs = funs(lag))
df2$ID <- seq.int(nrow(df2))

total <- merge(df,df2,by=c('ID'))

data <- total %>%
  select(ID, cowid.x, day.x, time.x ,x.x, y.x, actm.x,actp.x, day.y, x.y, y.y, actm.y, actp.y)
df_new <- data  %>% slice(-1)
df_new$ID <- seq.int(nrow(df_new))

distance <- rep(0, nrow(df_new))
for(i in 1:nrow(df_new))
  distance[i] <- sqrt((df_new[,10][i] - df_new[,5][i])^2 + (df_new[,11][i] - df_new[,6][i])^2)

distance2 <- do.call(rbind, lapply(distance, as.data.frame))
distance2$ID <- seq.int(nrow(distance2))
colnames(distance2) <- c('distance', 'ID')

total_distance <- merge(df_new,distance2,by=c('ID'))

#########################################
### Get Feeding area:

total_distance$feeding <- ifelse(
  ( 
    (total_distance$x.x > 0.25) &
      (total_distance$x.x < 58.65) &
      (total_distance$y.x > 10.5) &
      (total_distance$y.x < 17.2)
  ),
  1,  # if condition is met, put 1
  0   # else put 0
)

total_distance$milking <- ifelse(
  ( 
    (total_distance$x.x > 29.4) &
      (total_distance$x.x < 42.25) &
      (total_distance$y.x > -8.42) &
      (total_distance$y.x < -1.62)
  ),
  1,  # if condition is met, put 1
  0   # else put 0
)

###Set target

head(total_distance)
df$lame <- ifelse(
  ( 
    (df$cowid == '1078' ) |
      (df$cowid == '1184' ) |
      (df$cowid == '1340' ) |
      (df$cowid == '1891' ) |
      (df$cowid == '2003' ) |
      (df$cowid == '2010' ) |
      (df$cowid == '2060' ) |
      (df$cowid == '2302' ) |
      (df$cowid == '2344' ) |
      (df$cowid == '2616' ) 
  ),
  1,  # if condition is met, put 1
  0   # else put 0
)


### Seperate by cows
df_1340   = df[df['cowid'] == '1340',]
df_1491   = df[df['cowid'] == '1491',]
df_1892   = df[df['cowid'] == '1892',]
df_1078   = df[df['cowid'] == '1078',]
df_1891   = df[df['cowid'] == '1891',]
df_1184   = df[df['cowid'] == '1184', ]
df_2003   = df[df['cowid'] == '2003', ]
df_2010   = df[df['cowid'] == '2010', ]
df_2060   = df[df['cowid'] == '2060', ]
df_2153   = df[df['cowid'] == '2153', ]
df_2172   = df[df['cowid'] == '2172', ]
df_2179   = df[df['cowid'] == '2179', ]
df_2302   = df[df['cowid'] == '2302', ]
df_2344   = df[df['cowid'] == '2344', ]
df_2472   = df[df['cowid'] == '2472', ]
df_2512   = df[df['cowid'] == '2512', ]
df_2596   = df[df['cowid'] == '2596', ]
df_2616   = df[df['cowid'] == '2616', ]
df_2954   = df[df['cowid'] == '2954', ]
df_2959   = df[df['cowid'] == '2959', ]



### Cow id 1340

df_1340_10mins  <-  df_1340 %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "10minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  )

df_1340_30mins  <-  df_1340 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "30minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y),day=mean(day)  ) 

df_1340_1hr  <-  df_1340 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "1hour")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 

df_1340_1hr[df_1340_1hr$day == 1,]$new_x

#par(mfrow=c(2,2))
par(mfrow=c(1,1))
##Ploting 
bwmap() + lines(df_1340_1hr[df_1340_1hr$day == 1,]$new_x, df_1340_1hr[df_1340_1hr$day == 1,]$new_y,lwd = 2, col="#4da6ff") + 
  points(df_1340_1hr[df_1340_1hr$day == 1,]$new_x, df_1340_1hr[df_1340_1hr$day == 1,]$new_y, pch = 13, bg = "grey")

bwmap() + lines(df_1340_30mins[df_1340_30mins$day == 1,]$new_x, df_1340_30mins[df_1340_30mins$day == 1,]$new_y,lwd = 2, col="#4da6ff") + 
  points(df_1340_30mins[df_1340_30mins$day == 1,]$new_x, df_1340_30mins[df_1340_30mins$day == 1,]$new_y, pch = 13, bg = "red")


bwmap() + lines(df_1340_10mins[df_1340_10mins$day == 1,]$new_x, df_1340_10mins[df_1340_10mins$day == 1,]$new_y,lwd = 2, col="#4da6ff") + 
  points(df_1340_10mins[df_1340_10mins$day == 1,]$new_x, df_1340_10mins[df_1340_10mins$day == 1,]$new_y, pch = 13, bg = "red")


### Cow id 1491

df_1491_10mins  <-  df_1491 %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "10minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  )

df_1491_30mins  <-  df_1491 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "30minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y),day=mean(day)  ) 

df_1491_1hr  <-  df_1491 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "1hour")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 


bwmap() + lines(df_1491_10mins[df_1491_10mins$day == 1,]$new_x, df_1491_10mins[df_1491_10mins$day == 1,]$new_y,lwd = 2, col="red") + 
  points(df_1491_10mins[df_1491_10mins$day == 1,]$new_x, df_1491_10mins[df_1491_10mins$day == 1,]$new_y, pch = 13, bg = "red")

bwmap() + lines(df_1491[df_1491$day == 1,]$x, df_1491[df_1491$day == 1,]$y,lwd = 2, col="red") + 
  points(df_1491[df_1491$day == 1,]$x, df_1491[df_1491$day == 1,]$y, pch = 13, bg = "red")


df_1491_1min  <-  df_1491 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "1minute")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 


bwmap() + lines(df_1491_1min[df_1491_1min$day == 1,]$new_x, df_1491_1min[df_1491_1min$day == 1,]$new_y,lwd = 2, col="red") + 
  points(df_1491_1min[df_1491_1min$day == 1,]$new_x, df_1491_1min[df_1491_1min$day == 1,]$new_y)+ title(main="Day 1")

bwmap() + lines(df_1491_1min[df_1491_1min$day == 2,]$new_x, df_1491_1min[df_1491_1min$day == 2,]$new_y,lwd = 2, col="red") + 
  points(df_1491_1min[df_1491_1min$day == 2,]$new_x, df_1491_1min[df_1491_1min$day == 2,]$new_y)+ title(main="Day 2")

bwmap() + lines(df_1491_1min[df_1491_1min$day == 3,]$new_x, df_1491_1min[df_1491_1min$day == 3,]$new_y,lwd = 2, col="red") + 
  points(df_1491_1min[df_1491_1min$day == 3,]$new_x, df_1491_1min[df_1491_1min$day == 3,]$new_y) + title(main="Day 3")

bwmap() + lines(df_1491_1min[df_1491_1min$day == 4,]$new_x, df_1491_1min[df_1491_1min$day == 4,]$new_y,lwd = 2, col="red") + 
  points(df_1491_1min[df_1491_1min$day == 4,]$new_x, df_1491_1min[df_1491_1min$day == 4,]$new_y)+ title(main="Day 4")


### Cow id 1078


df_1078_10mins  <-  df_1078 %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "10minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  )

df_1078_30mins  <-  df_1078 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "30minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y),day=mean(day)  ) 

df_1078_1hr  <-  df_1078 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "1hour")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 

df_1078_1min  <-  df_1078 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "1minute")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 


bwmap() + lines(df_1078_1min[df_1078_1min$day == 1,]$new_x, df_1078_1min[df_1078_1min$day == 1,]$new_y,lwd = 2, col='orange') + 
  points(df_1078_1min[df_1078_1min$day == 1,]$new_x, df_1078_1min[df_1078_1min$day == 1,]$new_y)+ title(main="Day 1")

bwmap() + lines(df_1078_1min[df_1078_1min$day == 2,]$new_x, df_1078_1min[df_1078_1min$day == 2,]$new_y,lwd = 2, col='orange') + 
  points(df_1078_1min[df_1078_1min$day == 2,]$new_x, df_1078_1min[df_1078_1min$day == 2,]$new_y)+ title(main="Day 2")

bwmap() + lines(df_1078_1min[df_1078_1min$day == 3,]$new_x, df_1078_1min[df_1078_1min$day == 3,]$new_y,lwd = 2, col='orange') + 
  points(df_1078_1min[df_1078_1min$day == 3,]$new_x, df_1078_1min[df_1078_1min$day == 3,]$new_y) + title(main="Day 3")

bwmap() + lines(df_1078_1min[df_1078_1min$day == 4,]$new_x, df_1078_1min[df_1078_1min$day == 4,]$new_y,lwd = 2, col='orange') + 
  points(df_1078_1min[df_1078_1min$day == 4,]$new_x, df_1078_1min[df_1078_1min$day == 4,]$new_y)+ title(main="Day 4")




### Cow id 1891

df_1891_10mins  <-  df_1891 %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "10minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  )

df_1891_30mins  <-  df_1891 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "30minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y),day=mean(day)  ) 

df_1891_1hr  <-  df_1891 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "1hour")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 

df_1891_1min  <-  df_1891 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "1minute")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 


bwmap() + lines(df_1891_1min[df_1891_1min$day == 1,]$new_x, df_1891_1min[df_1891_1min$day == 1,]$new_y,lwd = 2, col='pink') + 
  points(df_1891_1min[df_1891_1min$day == 1,]$new_x, df_1891_1min[df_1891_1min$day == 1,]$new_y)+ title(main="Day 1")

bwmap() + lines(df_1891_1min[df_1891_1min$day == 2,]$new_x, df_1891_1min[df_1891_1min$day == 2,]$new_y,lwd = 2, col='pink') + 
  points(df_1891_1min[df_1891_1min$day == 2,]$new_x, df_1891_1min[df_1891_1min$day == 2,]$new_y)+ title(main="Day 2")

bwmap() + lines(df_1891_1min[df_1891_1min$day == 3,]$new_x, df_1891_1min[df_1891_1min$day == 3,]$new_y,lwd = 2, col='pink') + 
  points(df_1891_1min[df_1891_1min$day == 3,]$new_x, df_1891_1min[df_1891_1min$day == 3,]$new_y) + title(main="Day 3")

bwmap() + lines(df_1891_1min[df_1891_1min$day == 4,]$new_x, df_1891_1min[df_1891_1min$day == 4,]$new_y,lwd = 2, col='pink') + 
  points(df_1891_1min[df_1891_1min$day == 4,]$new_x, df_1891_1min[df_1891_1min$day == 4,]$new_y)+ title(main="Day 4")




### Cow id 1184

df_1184_10mins  <-  df_1184 %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "10minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  )

df_1184_30mins  <-  df_1184 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "30minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y),day=mean(day)  ) 

df_1184_1hr  <-  df_1184 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "1hour")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 

df_1184_1min  <-  df_1184 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "1minute")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 


bwmap() + lines(df_1184_1min[df_1184_1min$day == 1,]$new_x, df_1184_1min[df_1184_1min$day == 1,]$new_y,lwd = 2, col='gray') + 
  points(df_1184_1min[df_1184_1min$day == 1,]$new_x, df_1184_1min[df_1184_1min$day == 1,]$new_y)+ title(main="Day 1")

bwmap() + lines(df_1184_1min[df_1184_1min$day == 2,]$new_x, df_1184_1min[df_1184_1min$day == 2,]$new_y,lwd = 2, col='gray') + 
  points(df_1184_1min[df_1184_1min$day == 2,]$new_x, df_1184_1min[df_1184_1min$day == 2,]$new_y)+ title(main="Day 2")

bwmap() + lines(df_1184_1min[df_1184_1min$day == 3,]$new_x, df_1184_1min[df_1184_1min$day == 3,]$new_y,lwd = 2, col='gray') + 
  points(df_1184_1min[df_1184_1min$day == 3,]$new_x, df_1184_1min[df_1184_1min$day == 3,]$new_y) + title(main="Day 3")

bwmap() + lines(df_1184_1min[df_1184_1min$day == 4,]$new_x, df_1184_1min[df_1184_1min$day == 4,]$new_y,lwd = 2, col='gray') + 
  points(df_1184_1min[df_1184_1min$day == 4,]$new_x, df_1184_1min[df_1184_1min$day == 4,]$new_y)+ title(main="Day 4")


### Cow id 2003

df_2003_10mins  <-  df_2003 %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "10minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  )

df_2003_30mins  <-  df_2003 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "30minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y),day=mean(day)  ) 

df_2003_1hr  <-  df_2003 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "1hour")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 

df_2003_1min  <-  df_2003 %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "1minute")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 


bwmap() + lines(df_2003_1min[df_2003_1min$day == 1,]$new_x, df_2003_1min[df_2003_1min$day == 1,]$new_y,lwd = 1, col='red') + 
  points(df_2003_1min[df_2003_1min$day == 1,]$new_x, df_2003_1min[df_2003_1min$day == 1,]$new_y)+ title(main="Day 1")

bwmap() + lines(df_2003_1min[df_2003_1min$day == 2,]$new_x, df_2003_1min[df_2003_1min$day == 2,]$new_y,lwd = 1, col='red') + 
  points(df_2003_1min[df_2003_1min$day == 2,]$new_x, df_2003_1min[df_2003_1min$day == 2,]$new_y)+ title(main="Day 2")

bwmap() + lines(df_2003_1min[df_2003_1min$day == 3,]$new_x, df_2003_1min[df_2003_1min$day == 3,]$new_y,lwd = 1, col='red') + 
  points(df_2003_1min[df_2003_1min$day == 3,]$new_x, df_2003_1min[df_2003_1min$day == 3,]$new_y) + title(main="Day 3")

bwmap() + lines(df_2003_1min[df_2003_1min$day == 4,]$new_x, df_2003_1min[df_2003_1min$day == 4,]$new_y,lwd = 1, col='red') + 
  points(df_2003_1min[df_2003_1min$day == 4,]$new_x, df_2003_1min[df_2003_1min$day == 4,]$new_y)+ 
  title(main="Day 4")




ggplot(df_2003_1min, aes(x=new_x, y=new_y) ) +
  xlim(0, 60) +
  ylim(-10, 30) +
  # geom_rect(xmin = 0.25, xmax = 58.65,   ymin =10.5, ymax = 17.2,   fill = "grey", alpha= 0.1)  +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette = "Set1", direction=1) + # + bwmap()
  #geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black", alpha=0.5)    
  annotate("rect", xmin = 0.25, xmax = 58.65,   ymin =10.5, ymax = 17.2, fill = "gray", alpha = 0.5) + 
  annotate("rect", xmin = 0, xmax = 60,   ymin =-10, ymax = 30,col = 'black', fill = NA) + 
  annotate("rect", xmin = 29.4, xmax = 42.5,   ymin =-8.42, ymax = -1.62,fill = 'gray',  alpha = 0.5) +
  annotate("segment", x = 0, xend = 60,y = 10, yend = 10,col = 'black', fill = NA)
# geom_rect(xmin = 0.25, xmax = 58.65,   ymin =10.5, ymax = 17.2,   fill = "red", alpha= 0.1)


#######


df_1340_1min  <-  df_1340 %>%  
  # filter(day == 3) %>%
  group_by(month=floor_date(datetime, "1minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 

ggplot(df_1340_1min, aes(x=new_x, y=new_y) ) +
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
  annotate("segment", x = 0, xend = 60,y = 10, yend = 10,col = 'black', fill = NA)  +
# geom_rect(xmin = 0.25, xmax = 58.65,   ymin =10.5, ymax = 17.2,   fill = "red", alpha= 0.1)
   facet_grid(rows  = vars(day))

######### non- Lame

df_2472_1min  <-  df_2472 %>%  
  # filter(day == 3) %>%
  group_by(month=floor_date(datetime, "1minutes")) %>% 
  summarize(new_x=mean(x),new_y=mean(y), day = mean(day)  ) 

ggplot(df_2472_1min, aes(x=new_x, y=new_y) ) +
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
  annotate("segment", x = 0, xend = 60,y = 10, yend = 10,col = 'black', fill = NA)  +
  # geom_rect(xmin = 0.25, xmax = 58.65,   ymin =10.5, ymax = 17.2,   fill = "red", alpha= 0.1)
  facet_grid(rows  = vars(day))


############ animation


warming_plot <- ggplot(warming, aes(x = year, y = value)) +
  geom_line(colour = "black") +
  geom_point(shape = 21, colour = "black", aes(fill = value), size = 5, stroke = 1) +
  scale_x_continuous(limits = c(1880,2017)) +
  scale_y_continuous(limits = c(-0.5,1)) +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-1,1), guide = FALSE) +
  xlab("") +
  ylab("Difference from 1900-2000 (ºC)") +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  # gganimate code
  transition_reveal(id = 1, along = year)

# save as a GIF
animate(warming_plot, fps = 10, width = 750, height = 450)
anim_save("warming.gif")






total_distance$datetime <- anytime(total_distance$time)
total_distance$datetime <- as.POSIXct(total_distance$datetime ,format="%Y/%m/%d %H:%M:%S",tz=Sys.timezone())

library(png)
library(gapminder)
#install.packages('png')
library(gganimate)
library(gifski)
total_distance$mintue <- substring(total_distance$datetime,15,16)

df_check  <-  total_distance %>%  
  # filter(day == 1) %>%
  group_by(month=floor_date(datetime, "1hour")) %>% 
  summarize(new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x),cowid = mean(cowid.x),distance = mean(distance )  ) 

df_check$ID <- seq.int(nrow(df_check))

ggplot(df_check, aes(x.x, x.y,  colour = as.factor(cowid.x)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = gapminder) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
 # facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(ID)