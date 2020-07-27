#total_distance

total_distance$lame <- ifelse(
  ( 
    (total_distance$cowid.x == '1078' ) |
      (total_distance$cowid.x == '1184' ) |
      (total_distance$cowid.x == '1340' ) |
      (total_distance$cowid.x == '1891' ) |
      (total_distance$cowid.x == '2003' ) |
      (total_distance$cowid.x == '2010' ) |
      (total_distance$cowid.x == '2060' ) |
      (total_distance$cowid.x == '2302' ) |
      (total_distance$cowid.x == '2344' ) |
      (total_distance$cowid.x == '2616' ) 
  ),
  1,  # if condition is met, put 1
  0   # else put 0
)

head(total_distance)
total_distance$datetime <- anytime(total_distance$time.x)
total_distance$datetime <- as.POSIXct(total_distance$datetime ,format="%Y/%m/%d %H:%M:%S",tz=Sys.timezone())


total_distance %>%
  # filter(cit == "plus") %>%
  select(ID, cowid.x, day.x, x.x, y.x, actm.x, actp.x, distance,lame,datetime, feeding, milking) -> df_data


df_data


df_data$lame <- ifelse(
  ( 
    (df_data$cowid.x == '1078' ) |
      (df_data$cowid.x == '1184' ) |
      (df_data$cowid.x == '1340' ) |
      (df_data$cowid.x == '1891' ) |
      (df_data$cowid.x == '2003' ) |
      (df_data$cowid.x == '2010' ) |
      (df_data$cowid.x == '2060' ) |
      (df_data$cowid.x == '2302' ) |
      (df_data$cowid.x == '2344' ) |
      (df_data$cowid.x == '2616' ) 
  ),
  1,  # if condition is met, put 1
  0   # else put 0
)




### Seperate by cows
df_data_1340   = df_data[df_data['cowid.x'] == '1340',]
df_data_1491   = df_data[df_data['cowid.x'] == '1491',]
df_data_1892   = df_data[df_data['cowid.x'] == '1892',]
df_data_1078   = df_data[df_data['cowid.x'] == '1078',]
df_data_1891   = df_data[df_data['cowid.x'] == '1891',]
df_data_1184   = df_data[df_data['cowid.x'] == '1184', ]
df_data_2003   = df_data[df_data['cowid.x'] == '2003', ]
df_data_2010   = df_data[df_data['cowid.x'] == '2010', ]
df_data_2060   = df_data[df_data['cowid.x'] == '2060', ]
df_data_2153   = df_data[df_data['cowid.x'] == '2153', ]
df_data_2172   = df_data[df_data['cowid.x'] == '2172', ]
df_data_2179   = df_data[df_data['cowid.x'] == '2179', ]
df_data_2302   = df_data[df_data['cowid.x'] == '2302', ]
df_data_2344   = df_data[df_data['cowid.x'] == '2344', ]
df_data_2472   = df_data[df_data['cowid.x'] == '2472', ]
df_data_2512   = df_data[df_data['cowid.x'] == '2512', ]
df_data_2596   = df_data[df_data['cowid.x'] == '2596', ]
df_data_2616   = df_data[df_data['cowid.x'] == '2616', ]
df_data_2954   = df_data[df_data['cowid.x'] == '2954', ]
df_data_2959   = df_data[df_data['cowid.x'] == '2959', ]



data_byday <-  df_data %>%  
  #filter(day == 1) %>%
  group_by(cowid.x,month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = sum(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

### Oneday frame
data_day = data.frame()

dataframe_1340  <-  df_data_1340 %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_1340)
dim(data_day)

dataframe_1491   <-  df_data_1491  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_1491)
dim(data_day)

dataframe_1892   <-  df_data_1892  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_1892)

dataframe_1078   <-  df_data_1078  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_1078)

dataframe_1891   <-  df_data_1891  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_1891)


dataframe_1184   <-  df_data_1184  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_1184)



dataframe_2003   <-  df_data_2003  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2003)



dataframe_2010   <-  df_data_2010  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2010)


dataframe_2060   <-  df_data_2060  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2060)


dataframe_2153  <-  df_data_2153  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2153)

dataframe_2172  <-  df_data_2172  %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2172)


dataframe_2179   <-  df_data_2179   %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2179 )

dataframe_2302    <-  df_data_2302   %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2302  )
dim(data_day)

dataframe_2344    <-  df_data_2344   %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2344  )

dataframe_2472    <-  df_data_2472   %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2472  )

dataframe_2512    <-  df_data_2512    %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2512   )

dataframe_2596    <-  df_data_2596    %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2596   )

dataframe_2616    <-  df_data_2616    %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2616   )

dataframe_2954   <-  df_data_2954    %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2954   )


dataframe_2959   <-  df_data_2959    %>%  
  #filter(day == 1) %>%
  group_by(month=floor_date(datetime, "day")) %>% 
  summarize(cowid = mean(cowid.x), new_x=mean(x.x),new_y=mean(y.x), day = mean(day.x) , distance = mean(distance),  actm = mean(actm.x), feeding = sum(feeding), milking = sum(milking)  )

data_day <- rbind(data_day,dataframe_2959  )


dim(data_day)


data_day$lame <- ifelse(
  ( 
    (data_day$cowid == '1078' ) |
      (data_day$cowid == '1184' ) |
      (data_day$cowid == '1340' ) |
      (data_day$cowid == '1891' ) |
      (data_day$cowid == '2003' ) |
      (data_day$cowid == '2010' ) |
      (data_day$cowid == '2060' ) |
      (data_day$cowid == '2302' ) |
      (data_day$cowid == '2344' ) |
      (data_day$cowid == '2616' ) 
  ),
  1,  # if condition is met, put 1
  0   # else put 0
)

### Apply Logistic Regression

smp_size <- floor(0.75 * nrow(data_day))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data_day)), size = smp_size)

train <- data_day[train_ind, ]
test <- data_day[-train_ind, ]

dim(train)
dim(test)

library(aod)
#install.packages('aod')
mylogit <- glm( lame ~ new_x + new_y +  distance + actm + feeding +  milking , data = train, family = "binomial")

summary(mylogit)

new_test <- within(test, rm(lame))
probabilities <- mylogit %>% predict(new_test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

table(predicted.classes , test$lame)

fourfoldplot(table(predicted.classes , test$lame))

#### Cross validation Logistic regression

library(caret)

# define training control
train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
model <- train(lame ~  distance + actm + feeding +  milking,
               data = data_day,
               trControl = train_control,
               method = "glm",
               family=binomial())
PredTrain = predict(model, newdata=data_day, type="raw") 
table(data_day$lame, PredTrain > 0.5)
fourfoldplot(table(data_day$lame, PredTrain > 0.5))
summary(model)
print(model)

ggplot(data = dataframe, aes(x = cowid, y = feeding)) + geom_boxplot() + theme_minimal() + labs(x = "Title", y = "x")



####### DECISION TREEE

library(rpart)
tree <- rpart( lame ~ new_x + new_y +  distance + actm + feeding +  milking , data = train)

tree_pred = predict(tree, newdata=new_test)

predicted_classes <- ifelse(tree_pred > 0.5, 1, 0)

fourfoldplot(table(predicted_classes, test$lame))




distance_df_per_day


#####################################


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


### Apply Logistic Regression

smp_size <- floor(0.75 * nrow(distance_df_per_day))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(distance_df_per_day)), size = smp_size)

train <- distance_df_per_day[train_ind, ]
test <- distance_df_per_day[-train_ind, ]

dim(train)
dim(test)




#install.packages('caret')
library(aod)

mylogit <- glm( lame ~ new_x + new_y +  distance + actm + feeding + milking , data = train, family = "binomial")

summary(mylogit)

new_test <- within(test, rm(lame))
probabilities <- mylogit %>% predict(new_test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

table(predicted.classes , test$lame)

fourfoldplot(table(predicted.classes , test$lame))

#### Cross validation Logistic regression

library(caret)
#install.packages("caret", dependencies = TRUE)
# define training control
train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
model <- train(lame ~  new_x + new_y + distance + actm + feeding +  milking,
               data = data_day,
               trControl = train_control,
               method = "glm",
               family=binomial())
PredTrain = predict(model, newdata=data_day, type="raw") 
table(data_day$lame, PredTrain > 0.5)
fourfoldplot(table(data_day$lame, PredTrain > 0.5))
summary(model)
print(model)

ggplot(data = dataframe, aes(x = cowid, y = feeding)) + geom_boxplot() + theme_minimal() + labs(x = "Title", y = "x")







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
  1,  # if condition is met, put 1
  0    # else put 0
)

### Apply Logistic Regression

smp_size <- floor(0.75 * nrow(distance_df_per_cow2))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(distance_df_per_cow2)), size = smp_size)

train2 <- distance_df_per_cow2[train_ind, ]
test2 <- distance_df_per_cow2[-train_ind, ]

dim(train2)
dim(test2)




#install.packages('caret')
library(aod)

mylogit2 <- glm( lame ~ new_x + new_y +  distance + actm + feeding + milking , data = train2, family = "binomial")

summary(mylogit2)

new_test <- within(test2, rm(lame))
probabilities <- mylogit2 %>% predict(new_test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

table(predicted.classes , test2$lame)
