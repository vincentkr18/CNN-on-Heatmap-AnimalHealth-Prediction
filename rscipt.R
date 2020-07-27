library(ggplot2)
#ggplot(d,aes(X,Y,color=ag))+geom_point()+
#  scale_color_gradient(low="yellow",high="red")

library(png)
library(grid)
library(ggimage)
r <- readPNG('M:\\Research\\Rplot.png')
rg <- rasterGrob(r, width=unit(1,"npc"), height=unit(1,"npc"))


install.packages('ggmap')
library(ggmap)
df <- read.csv('M:\\Research\\File_cow_location_activity_data.csv')

head(df)



ggimage(r, scale_axes = TRUE) +
  geom_point(aes(x = x, y = y), data = df_1078,
             size = I(1), fill = NA)


summary(df)
df_1078[complete.cases(df_1078), ] -> df_1078

g <-  ggplot(df_1078,aes(x,y,color=actp))+geom_point() + 
      annotation_custom(r, 0,60, -10,30) +
      scale_color_gradient(low="yellow",high="red") +
      scale_x_continuous(expand=c(0,10), lim=c(0,60)) +
      scale_y_continuous(expand=c(0,0), lim=c(-10,30)) 

g 


IM = readPNG(file.choose())

rasterImage(IM,0,0,100,100)
points(df_1078$x,df_1078$y, pch=20, col="#33333322")


rm(list = ls())
