install.packages("ggplot2")
library("ggplot2")

time <- seq(1, 24)
temperature <- c(22, 21, 20, 19, 18, 18, 19, 20, 22, 24, 26, 28, 29, 28, 27, 25, 24, 23, 22, 22, 21, 21, 20, 19)
data <- data.frame(time=time, temp=temperature)
ggplot(data, aes(x=time, y=temp)) + 
  geom_line() + 
  labs(title="시간에 따른 온도 변화") +
  xlab("시간") +
  ylab("온도")

df <- data.frame(x = c(1,2,3,4,5), y = c(6,8,5,9,7))
ggplot(df, aes(x = x, y = y))+
  geom_point(color = "blue",size = 3)+
  labs(title = "Scatter Polt")+
  xlab("X")+
  ylab("Y")

ggplot(df, aes(x = x, y = y))+
  geom_point(color = "blue",size = 3)+
  geom_line(aes(color = "Connected Points"), linewidth = 0.5)
  labs(title = "Scatter Polt")+
  xlab("X")+
  ylab("Y")
  
df2 <- data.frame(x = c(5,6,7,8,9), y = c(18,12,16,77,63))

ggplot()+
  geom_point(data = df, aes(x = x, y = y), color = "blue",size = 3)+
  geom_line(data = df, aes(x = x, y = y, color = "Connected Points"), linewidth = 0.5)+
  geom_point(data = df2, aes(x = x, y = y), color = "blue",size = 3)+
  geom_line(data = df2, aes(x = x, y = y, color = "Connected Points"), linewidth = 0.5)+
  labs(title = "Scatter Polt")+
  xlab("X")+
  ylab("Y") 

df <- data.frame(values = c(5,7,10,12,14,18,20,22,25,27,30,70))

ggplot(df, aes(x = "Group 1",y = values))+
  geom_boxplot(fill = "steelblue", outlier.color = "red")+
  coord_flip()+
  labs(title = "Horizontal Boxplot of Values")+
  xlab("Group")+
  ylab("Values")

df <- data.frame(
  group = c(rep("Group 1",60),rep("Group 2",60)),
  values = c(rnorm(60,mean = 0,sd =1),rnorm(60, mean = 2, sd = 1))
)

ggplot(df, aes(x = group,y = values))+
  geom_boxplot(fill = c("lightblue","lightgreen"), outlier.color = "red")+
  coord_flip()+
  labs(title = "Boxplot Example")+
  xlab("Group")+
  ylab("Values")

data <- c(100,200,300,400,200,200,100,400,250,230,70,80,90)

mean <- mean(data)
variance <- var(data)
std_dev <- sqrt(variance)
median <- median(data)
Q1 <- quantile(data,0.25)
Q3 <- quantile(data,0.75)
fqr_value <- Q3 - Q1

mean
std_dev
median
Q1
Q3
fqr_v

install.packages("data.table")
install.packages("ggplot2")

library(dplyr)
library(data.table)
library(ggplot2)

df <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/spotify_global_trends_2026 - spotify_global_trends_final.csv")
result <- df %>% 
  filter(df$country == c("KR","US"))

ggplot(result, aes(x = streams, y = country, fill = country)) + 
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) + 
  coord_flip() + 
  labs(title = "Boxplot Example") +
  xlab("streams") +
  ylab("country")

ggplot(result, aes(x = streams, y = country)) + 
  geom_boxplot(fill = c("lightblue", "lightgreen"),outlier.color = "red") +
  labs(title = "Boxplot Example") +
  xlab("streams") +
  ylab("country")

test <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/boxplot_example_data.csv")

ggplot(test, aes(x="var1",y=var1))+
  geom_boxplot(fill = "lightblue",outlier.color = "red")+
  labs(title = "Boxplot of var1")+
  xlab("variable")+
  ylab("Value")

ggplot(test, aes(x = nrow(test),y=var1))+
  geom_boxplot(fill = "lightblue",outlier.color = "red")+
  labs(title = "Boxplot of var1")+
  xlab("variable")+
  ylab("Value")

df2 <- data.frame(
  Variable = c(rep("var1",nrow(test)),rep("var2",nrow(test))),
  Value = c(test$var1,test$var2)
)

ggplot(df2, aes(x=Variable,y=Value))+
  geom_boxplot(fill = c("lightblue","lightgreen"),outlier.color = "red")+
  labs(title = "Boxplot of Two Variable")+
  xlab("variable")+
  ylab("Value")

(157-160)/sqrt(49/10)

(20-15)/sqrt(100/25)

(15-25)/sqrt((12^2)/35)

(17-18)/sqrt(9/36)

(90-85)/sqrt(100/30)
(93-85)/sqrt(100/30)
