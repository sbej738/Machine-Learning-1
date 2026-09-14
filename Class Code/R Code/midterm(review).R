
library(dplyr)
library(data.table)
library(ggplot2)

data <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/실습자료/box.csv")

df2 <- data.frame(
  variable = c(rep("var1", nrow(data)),
              rep("var2", nrow(data))),
  value = c(data$A, data$B)
  )

ggplot(df2, aes(x = value, y = variable)) + 
  geom_boxplot(fill = c("lightblue", "lightgreen"),
               outlier.color = "red") + 
  coord_flip()+
  labs(title = "Boxplot of Two Variable") +
  xlab("variable") +
  ylab("value")

library(BSDA)

data <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/실습자료/z_test.csv")

group_a <- data$A
group_b <- data$B

sd_a = sd(group_a)
sd_b = sd(group_b)
result <- z.test(x = group_a, y = group_b, sigma.x = sd_a, sigma.y = sd_b, alternative =  "two.sided")
result  

data <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/실습자료/t_test.csv")

group_a <- data$A
group_b <- data$B

t_test_result <- t.test(group_a, group_b, alternative = "two.sided")
t_test_result
t_test_result <- t.test(group_a, group_b, alternative = "less")
t_test_result
t_test_result <- t.test(group_a, group_b, alternative = "greater")
t_test_result
