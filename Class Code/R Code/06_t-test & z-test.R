t_csv <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/Gaming_Academic_Performance.csv")
t_data <- data.frame(gaming_hours = t_csv$gaming_hours, 
                     grades = t_csv$grades)

group_under <- t_data[t_data$gaming_hours <= 4, ]$grades

group_over <- t_data[t_data$gaming_hours > 4, ]$grades

t_df2 <- data.frame(
  variable = c(rep("group_a", length(group_under)), #하루 4시간 이하하
               rep("group_b", length(group_over))), #하루 4시간 초과
  value = c(group_under, group_over)
)

ggplot(t_df2, aes(x = value, y = variable)) + 
  geom_boxplot(fill = c("lightblue", "lightgreen"),
               outlier.color = "red") + 
  coord_flip()+
  labs(title = "Boxplot of Two Variable") +
  xlab("variable") +
  ylab("value")

t_test_result <- t.test(group_under, group_over, alternative = "two.sided")
t_test_result
t_test_result <- t.test(group_under, group_over, alternative = "less")
t_test_result
t_test_result <- t.test(group_under, group_over, alternative = "greater")
t_test_result

z_csv <- read.csv("C:/Users/Owner/OneDrive/바탕 화면/University/3학년 - 1/머신러닝(1)/predictive_maintenance.csv")
z_data <- data.frame(Type = z_csv$Type,
                     Torque = z_csv$Torque..Nm.)

group_under <- z_data[z_data$Torque <= 40, ]$Type

group_over <- z_data[z_data$Torque > 40, ]$Type

group_under_num <- ifelse(group_under == "L", 25, 
                          ifelse(group_under == "M", 65, 90))

group_over_num <- ifelse(group_over == "L", 25, 
                         ifelse(group_over == "M", 65, 90))

z_df2 <- data.frame(
  variable = c(rep("group_a", length(group_under_num)), #하루 4시간 이하하
               rep("group_b", length(group_over_num))), #하루 4시간 초과
  value = c(group_under_num, group_over_num)
)

ggplot(z_df2, aes(x = value, y = variable)) + 
  geom_boxplot(fill = c("lightblue", "lightgreen"),
               outlier.color = "red") + 
  coord_flip()+
  labs(title = "Boxplot of Two Variable") +
  xlab("variable") +
  ylab("value")

sd_a = sd(group_under_num)
sd_b = sd(group_over_num)
result <- z.test(x = group_under_num, y = group_over_num, sigma.x = sd_a, sigma.y = sd_b, alternative =  "two.sided")
result
