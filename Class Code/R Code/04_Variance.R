data <- c(-1234512,10, 15, 20, 25, 30, 15, 20, 25, 25, 10,10000,100000)
range_value <- max(data) - min(data)
Q1 <- quantile(data,0.25)
Q3 <- quantile(data,0.75)
fqr_value <- Q3 - Q1

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
