
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



observed <- table(데이터$변수)
expected <- rep(sum(observed) / length(observed), length(observed))
test_result <- chisq.test(x = observed, p = expected / sum(expected))

print(test_result)


candy_data <- data.frame(
  Color = c("Red","Blue","Green"),
  Observed = c(30,50,20)
)
total_candies <- sum(candy_data$Observed)
expected <- rep(total_candies/3,3)
test_result <- chisq.test(candy_data$Observed, p = expected / sum(expected))
print(test_result)

std_residuals <- (candy_data$Observed - expected) / sqrt(expected)
chi_square_values <- std_residuals^2
p_values <- 1 - pchisq(chi_square_values,df=2)

candy_data$Reisduals <- std_residuals
candy_data$Chi_square <- chi_square_values
candy_data$p_value <- p_values

print(candy_data)



data <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/housetasks.csv", row.names = 1)
chi_square_test_result <- chisq.test(data)
print(chi_square_test_result)

results <- chisq.posthoc.test(data, method = "bonferroni")
results

install.packages("corrplot")
library(corrplot)
corrplot(chi_square_test_result$residuals,is.cor = F)


df <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/Machine_set.csv")
set_a <- subset(df,machine == "A")$value
set_b <- subset(df,machine == "B")$value

result <- var.test(set_a, set_b)
print(result)

var.test(set_a, set_b, alternative = "less")

var.test(set_a, set_b, alternative = "greater")

df <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/Life_case.csv")

library(corrplot)
library(chisq.posthoc.test)
library(dplyr)

data1 <- data.frame(row.names = c("Elderly parent care","Small online shop",
                                  "Apartment community","Pet care","Shared travel planning"),
                    Person_A = c(
                      sum(df$Person_A[1:8]), sum(df$Person_A[9:16]), sum(df$Person_A[17:24]),
                      sum(df$Person_A[25:32]),sum(df$Person_A[33:40])),
                    Person_B = c(
                      sum(df$Person_B[1:8]), sum(df$Person_B[9:16]), sum(df$Person_B[17:24]),
                      sum(df$Person_B[25:32]),sum(df$Person_B[33:40]))
)

results <- chisq.posthoc.test(data1,method = "bonferroni")
results

chi_square_test_result1 <- chisq.test(data1)
print(chi_square_test_result1)

results1 <- chisq.posthoc.test(data1, method = "bonferroni")
results1
results1$diff <- sqrt((results1$Person_A - results1$Person_B)^2)

results1 <- results1 %>% 
  filter(Value != "p values")

results1 <- arrange(results1, results1$diff)[2,0]

corrplot(chi_square_test_result1$residuals,is.cor = F)

library(corrplot)
library(chisq.posthoc.test)
library(dplyr)

data2 <- df[,-c(1:2)]
rownames(data2) <- df$Activity

chi_square_test_result2 <- chisq.test(data2)
print(chi_square_test_result2)


results2 <- chisq.posthoc.test(data2, method = "bonferroni")
results2
results2$diff <- sqrt((results2$Person_A - results2$Person_B)^2)

results2 <- results2 %>% 
  filter(Value != "p values")

results2 <- arrange(results2, desc(results2$diff))[1:5,]

corrplot(chi_square_test_result2$residuals,is.cor = F)





grow <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/growth.csv", stringsAsFactors = T)

anova_result <- aov(growth ~ fertilizer + water, data = grow)
summary(anova_result)

anova_result <- aov(growth ~ fertilizer * water, data = grow)
summary(anova_result)

install.packages("rstatix")
install.packages("multcomp")
library(rstatix)
library(multcomp)

tukey_result <- glht(anova_result, linfct = mcp(fertilizer = "Tukey"))
summary(tukey_result)

games_howell_test(grow, growth ~ fertilizer)

grow$group <- interaction(grow$fertilizer, grow$water)

grow$group <- as.factor(grow$group)

anova_group <- aov(growth ~ group, data = grow)

tukey_result <- glht(anova_group, linfct = mcp(group = "Tukey"))

summary(tukey_result)



data <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/cafe.csv", stringsAsFactors = TRUE)

anova_result <- aov(Satisfaction ~ SeatLocation + CafeMood + CoffeeType, data = data)
summary(anova_result)

leveneTest(Satisfaction ~ SeatLocation, data = data)
leveneTest(Satisfaction ~ CafeMood, data = data)
leveneTest(Satisfaction ~ CoffeeType, data = data)

tukey_result <- glht(anova_result, linfct = mcp(SeatLocation = "Tukey",
                                                CafeMood = "Tukey",
                                                CoffeeType = "Tukey"))
summary(tukey_result)


3
group_a <- c(85,78,98,67,47,89,78,88,48,89,88,79,85)
group_b <- c(86,81,97,69,57,88,79,90,62,95,94,85,90)

t_test_result <- t.test(group_a, group_b, alternative = "two.sided")
t_test_result

5
df5 <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/기말시험/earlyadopter.csv")
group_a <- df5$X30s
group_b <- df5$X40s

t_test_result <- t.test(group_a, group_b, alternative = "two.sided")
t_test_result
t_test_result <- t.test(group_a, group_b, alternative = "less")
t_test_result <- t.test(group_a, group_b, alternative = "greater")

6
group_a <- df5$X40s
group_b <- df5$X50s

sd_a = sd(group_a)
sd_b = sd(group_b)
result <- z.test(x = group_a, y = group_b, sigma.x = sd_a, sigma.y = sd_b, alternative =  "greater")
result 

7
df7 <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/기말시험/Income.csv", stringsAsFactors = T)
anova_result <- aov(work_class ~ sex, data = df7)
summary(anova_result)

data7 <- data.frame(row.names = c("Male","Female"),
                    Male = df7$work_class %>% 
                      filter(sex == "Male"),
                    Female = df7$work_class %>% 
                      filter(sex == "Female"))


results <- chisq.posthoc.test(data1,method = "bonferroni")
results

chi_square_test_result1 <- chisq.test(df7_1)
print(chi_square_test_result1)

results1 <- chisq.posthoc.test(data1, method = "bonferroni")
results1
results1$diff <- sqrt((results1$Person_A - results1$Person_B)^2)

results1 <- results1 %>% 
  filter(Value != "p values")

results1 <- arrange(results1, results1$diff)[2,0]

corrplot(chi_square_test_result1$residuals,is.cor = F)
set_a <- subset(df7,sex == "Male")$work_class
set_b <- subset(df7,sex == "Female")$work_class

result <- var.test(set_a, set_b)
print(result)

8
df8 <- data.frame(row.names = c("younger","older","column_total"),
                  Low = c(80,120,200),
                  Medium = c(95,80,175),
                  High = c(75,50,125))
df8 <- data.frame(row.names = c("low","medium","high"),
                  young = c(80,95,75),
                  old = c(120,80,50)
                  )
results <- chisq.posthoc.test(df8,method = "bonferroni")
results

chi_square_test_result1 <- chisq.test(df8)

corrplot(chi_square_test_result1$residuals,is.cor = F)

9
df9 <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/기말시험/Purchase.csv", stringsAsFactors = T)
chi_square_test_result <- chisq.test(df9)
print(chi_square_test_result)

results <- chisq.posthoc.test(data, method = "bonferroni")
results

install.packages("corrplot")
library(corrplot)
corrplot(chi_square_test_result$residuals,is.cor = F)

10
df10 <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/기말시험/Income_2.csv", stringsAsFactors = T)
anova_result <- aov(income ~ education + sex, data = df10)
summary(anova_result)

leveneTest(income ~ education, data = df10)
leveneTest(income ~ sex, data = df10)

tukey_result <- glht(anova_result, linfct = mcp(education = "Tukey",
                                                sex = "Tukey"))
summary(tukey_result)
