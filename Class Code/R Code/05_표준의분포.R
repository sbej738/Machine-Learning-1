(-6-(4-12))/sqrt((16/64)+(48/75))

#t-test
group_a <- c(85, 88, 90, 92, 91, 87, 89, 86, 84, 83)
group_b <- c(78, 82, 80, 85, 84, 87, 83, 81, 80, 79)

# a != b
t_test_result <- t.test(group_a, group_b, alternative = "two.sided")
#a < b
t_test_result <- t.test(group_a, group_b, alternative = "less")
#a > b
t_test_result <- t.test(group_a, group_b, alternative = "greater")

#z-test
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
#일반좌석 = groub A, 스터디룸 = groub B
data <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/t_test.csv")
group_a <- data$groupA
group_b <- data$groupB
#귀무가설 기각 시, a != b
t_test_result <- t.test(group_a, group_b, alternative = "two.sided")
#귀무가설 기각 시, a < b
t_test_result <- t.test(group_a, group_b, alternative = "less")
#귀무가설 기각 시, a > b
t_test_result <- t.test(group_a, group_b, alternative = "greater")


t_test_result
#1-1
#1-2 평균 성취도는 스터디 룸이 더 높다
  
#일반좌석 = groub A, 스터디룸 = groub B
data <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/z_test.csv")
group_a <- data$groupA
group_b <- data$groupB

sd_a = sd(group_a)
sd_b = sd(group_b)
result <- z.test(x = group_a, y = group_b, sigma.x = sd_a, sigma.y = sd_b, alternative =  "two.sided")
result
