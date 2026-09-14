set_a <- c(10.1, 10.2, 10.3, 10.0, 10.1, 10.2, 10.3, 10.0, 10.1, 10.2) 
set_b <- c(9.8, 10.5, 10.2, 9.7, 10.4, 10.3, 9.6, 10.6, 9.9, 10.7)

result <- var.test(set_a, set_b)
print(result)

var.test(set_a, set_b, alternative = "less")

var.test(set_a, set_b, alternative = "greater")

df <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/Machine_set.csv")
set_a <- subset(df,machine == "A")$value
set_b <- subset(df,machine == "B")$value

result <- var.test(set_a, set_b)
print(result)

var.test(set_a, set_b, alternative = "less")

var.test(set_a, set_b, alternative = "greater")

install.packages("car")
library(car)
grow <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/growth.csv")

leveneTest(growth ~fertilizer, data = grow)

leveneTest(growth ~ interaction(fertilizer,water), data = grow)


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

df <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/cafe.csv",stringsAsFactors = TRUE)

#등분산 검정
library(car)
leveneTest(Satisfaction ~ CoffeeType, data = df)
leveneTest(Satisfaction ~ CafeMood, data = df)
leveneTest(Satisfaction ~ SeatLocation, data = df)
leveneTest(Satisfaction ~ interaction(CoffeeType,CafeMood,SeatLocation), data = df)

#분산분석
anova_result <- aov(Satisfaction ~ CoffeeType + CafeMood + SeatLocation, data = df)
summary(anova_result)

anova_result <- aov(Satisfaction ~ CoffeeType * CafeMood * SeatLocation, data = df)
summary(anova_result)

#사후 검증
library(multcomp)
library(rstatix)


tukey_result <- glht(anova_result, linfct = mcp(CoffeeType = "Tukey"))
summary(tukey_result)
tukey_result <- glht(anova_result, linfct = mcp(CafeMood = "Tukey"))
summary(tukey_result)
tukey_result <- glht(anova_result, linfct = mcp(SeatLocation = "Tukey"))
summary(tukey_result)


tukey_result <- glht(anova_result, linfct = mcp(CoffeeType = "Tukey",
                     CafeMood = "Tukey",
                     SeatLocation = "Tukey"))
summary(tukey_result)

#Latte Quiet window
# 밑의 코드는 의미 없음
df$group <- interaction(df$CoffeeType, df$CafeMood, df$SeatLocation)

df$group <- as.factor(df$group)

anova_group <- aov(Satisfaction ~ group, data = df)

tukey_result <- glht(anova_group, linfct = mcp(group = "Tukey"))
summary(tukey_result, test = adjusted(type = "bonferroni"))
summary(tukey_result)
suppressWarnings(summary(tukey_result))

