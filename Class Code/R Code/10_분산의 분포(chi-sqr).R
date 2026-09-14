#적합도 검정 
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

#독립성 검정 
observed <- matrix(c(25, 15, 20,
                     30, 35, 25, 
                     15, 20, 15), 
                   nrow = 3, byrow = TRUE)
rownames(observed) <- c("Red", "Blue", "Green")
colnames(observed) <- c("Chocolate", "Vanilla", "Strawberry")
chi_squared <- chisq.test(observed)
print(chi_squared)

data <- data.frame(Gender = c("Male", "Female", "Male", "Male", "Female", 
                              "Female", "Male", "Male", "Female", "Female"), 
                   Food = c("국밥","마라탕","국밥","피자","피자",
                            "국밥","국밥","마라탕","피자","피자"))
cross_tab <- table(data$Gender, data$Food)
cross_tab

chi_square_test_result <- chisq.test(cross_tab)
print(chi_square_test_result)

install.packages("chisq.posthoc.test")
library(chisq.posthoc.test)
cross_tab <- table(data$Gender, data$Food)
cross_tab

results <- chisq.posthoc.test(cross_tab,method = "bonferroni")
results

data <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/housetasks.csv", row.names = 1)
chi_square_test_result <- chisq.test(data)
print(chi_square_test_result)

results <- chisq.posthoc.test(data, method = "bonferroni")
results

install.packages("corrplot")
library(corrplot)
corrplot(chi_square_test_result$residuals,is.cor = F)




df <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/Life_case.csv")

#1번
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

#2번
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


