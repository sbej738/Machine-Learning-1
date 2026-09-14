data <- c(10,15,20,25,30,15,20,25,25,10)
mean_value <- mean(data)
median_value <- median(data)
max_value <- max(data)
min_value <- min(data)

find_mode <- function(x){
  u <- unique(x)
  tab <- tabulate(match(x,u))
  u[tab == max(tab)]
}
mode_value <- find_mode(data)

install.packages("dplyr")
library(dplyr)
a_mid <- c(75,88,91,68,82)
a_fin <- c(60,87,55,47,92)
mean(a_mid)
mean(a_fin)

t_a_mid <- a_mid - min(a_mid) - max(a_mid)
t_a_fin <- c(60,87,55)

mean(t_a_mid)
mean(t_a_fin)

((5.5/5)*(7/5.5)*(10/7))^(1/3)-1

data <- c(10, 15, 20, 25, 30, 15, 20, 25, 25, 10)
range_value <- max(data) - min(data)
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75)
iqr_value <- Q3 - Q1

range_value
Q1
Q3
fqr_value

data2 <- c(10,15,20,25,30,15,20,25,10)
mean_data2 <- mean(data2)
squared_diff <- (data2 - mean_data2)^2
variance <- sum(squared_diff) / (length(data2) - 1)
std_dev <- sqrt(variance)

sd_value <- sd(data2)
var_value <- var(data2)

mean_data2
squared_diff
variance
std_dev

sd_value
var_value

library(dplyr)
library(data.table)
library(ggplot2)

df <- data.frame( x = c(1, 2, 3, 4, 5), y = c(6, 8, 5, 9, 7)) 
ggplot(df, aes(x = x, y = y)) + 
  geom_point(color = "blue", size = 3) + 
  labs(title = "Scatter Plot") +
  xlab("X") +
  ylab("Y")

df <- data.frame( x = c(1, 2, 3, 4, 5), y = c(6, 8, 5, 9, 7)) 
ggplot(df, aes(x = x, y = y)) + 
  geom_point(color = "blue", size = 3) + 
  geom_line(aes(color = "Connected Points"), size = 0.5) +
  labs(title = "Scatter Plot") +
  xlab("X") +
  ylab("Y")

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

group_a <- c(85, 88, 90, 92, 91, 87, 89, 86, 84, 83)
group_b <- c(78, 82, 80, 85, 84, 87, 83, 81, 80, 79)

t_test_result <- t.test(group_a, group_b, alternative = "two.sided")

t_test_result <- t.test(group_a, group_b, alternative = "less")

t_test_result <- t.test(group_a, group_b, alternative = "greater")

install.packages("BSDA")
library(BSDA)

group_a <- c(85, 88, 90, 92, 91, 87, 89, 86, 84, 83, 85, 88, 90, 92, 91, 87, 89, 86, 84, 83, 
             85, 88, 90, 92, 91, 87, 89, 86, 84, 83, 85, 88, 90, 92, 91, 87, 89, 86, 84, 83)
group_b <- c(78, 82, 80, 85, 84, 87, 83, 81, 80, 79, 78, 82, 80, 85, 84, 87, 83, 81, 80, 79, 
             78, 82, 80, 85, 84, 87, 83, 81, 80, 79, 78, 82, 80, 85, 84, 87, 83, 81, 80, 79)

sd_a = sd(group_a)
sd_b = sd(group_b)

result <- z.test(x = group_a, y = group_b, sigma.x = sd_a, sigma.y = sd_b, alternative =  "two.sided")

t_test_result <- t.test(group_a, group_b, alternative = "two.sided")

install.packages("BSDA")
library(BSDA)

data <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/중간시험/t_test.csv")
group_a <- data$groupA
group_b <- data$groupB

t_test_result <- t.test(group_a, group_b, alternative = "two.sided")

t_test_result <- t.test(group_a, group_b, alternative = "less")

t_test_result <- t.test(group_a, group_b, alternative = "greater")


t_test_result

data <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/z_test.csv")
group_a <- data$groupA
group_b <- data$groupB

sd_a = sd(group_a)
sd_b = sd(group_b)
result <- z.test(x = group_a, y = group_b, sigma.x = sd_a, sigma.y = sd_b, alternative =  "two.sided")
result

a <- c(160,180,180,160)
b <- c(170,190,160,150)

(mean(a)*0.6 + mean(b)*0.4)
(mean(a)*0.6 + mean(b)*0.4) / 2
