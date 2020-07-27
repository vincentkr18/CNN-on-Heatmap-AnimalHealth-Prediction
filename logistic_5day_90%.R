### Oneday frame
data_hour = data.frame()





dataframe_1340  <-  df_data_1340 %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_1340)
dim(data_hour)

dataframe_1491   <-  df_data_1491  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_1491)
dim(data_hour)

dataframe_1892   <-  df_data_1892  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_1892)

dataframe_1078   <-  df_data_1078  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_1078)

dataframe_1891   <-  df_data_1891  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_1891)


dataframe_1184   <-  df_data_1184  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_1184)



dataframe_2003   <-  df_data_2003  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2003)



dataframe_2010   <-  df_data_2010  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2010)


dataframe_2060   <-  df_data_2060  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2060)


dataframe_2153  <-  df_data_2153  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2153)

dataframe_2172  <-  df_data_2172  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2172)


dataframe_2179   <-  df_data_2179   %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2179 )

dataframe_2302    <-  df_data_2302   %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2302  )
dim(data_hour)

dataframe_2344    <-  df_data_2344   %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2344  )

dataframe_2472    <-  df_data_2472   %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2472  )

dataframe_2512    <-  df_data_2512    %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2512   )

dataframe_2596    <-  df_data_2596    %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2596   )

dataframe_2616    <-  df_data_2616    %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2616   )

dataframe_2954   <-  df_data_2954    %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2954   )


dataframe_2959   <-  df_data_2959    %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "5day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_hour <- rbind(data_hour,dataframe_2959  )


head(data_hour)




#### set label
########################################


data_hour$lame <- ifelse(
  ( 
    (data_hour$cowid == '1078' ) |
      (data_hour$cowid == '1184' ) |
      (data_hour$cowid == '1340' ) |
      (data_hour$cowid == '1891' ) |
      (data_hour$cowid == '2003' ) |
      (data_hour$cowid == '2010' ) |
      (data_hour$cowid == '2060' ) |
      (data_hour$cowid == '2302' ) |
      (data_hour$cowid == '2344' ) |
      (data_hour$cowid == '2616' ) 
  ),
  1,  # if condition is met, put 1
  0   # else put 0
)

######### Logistic Regression
#####################################


smp_size <- floor(0.65 * nrow(data_hour))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data_hour)), size = smp_size)

train <- data_hour[train_ind, ]
test <- data_hour[-train_ind, ]

dim(train)
dim(test)

test


library(aod)
#install.packages('aod')
mylogit <- glm( lame ~ new_x + new_y +  distance + actm + feeding +  milking , data = train, family = "binomial")

summary(mylogit)

new_test <- within(test, rm(lame))
probabilities <- mylogit %>% predict(new_test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

table(predicted.classes , test$lame)

fourfoldplot(table(predicted.classes , test$lame))



####### DECISION TREEE

library(rpart)
tree <- rpart( lame ~ new_x + new_y +  distance + actm + feeding +  milking , data = train)

tree_pred = predict(tree, newdata=new_test)

predicted_classes <- ifelse(tree_pred > 0.5, 1, 0)

fourfoldplot(table(predicted_classes, test$lame))
