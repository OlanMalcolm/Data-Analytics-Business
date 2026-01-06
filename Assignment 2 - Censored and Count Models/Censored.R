install.packages('censReg')
library(censReg)

# Number 1
data <- read.csv('Mobile_data_usage.csv', header = TRUE)
plot(data$Quota, data$DataUse)
# A lot of the values are censored at 0

# Number 2
lin.reg <- lm(DataUse ~ Quota + Days, data = data)
summary(lin.reg)

# Number 3
tobit.reg <- censReg(DataUse ~ Quota + Days, left = 0, data = data)
summary(tobit.reg)

# Number 4
cbind(summary(lin.reg)$coefficients[,1],summary(tobit.reg)$estimate[1:3])

# Number 5
mean_days <- mean(data$Days, na.rm = TRUE)

x1 <- c(1, 10, mean_days)     # For Quota = 10
x2 <- c(1, 2000, mean_days)   # For Quota = 2000

margeff1 <- margEff(tobit.reg, xValues = x1)
print("Marginal Effects at Quota = 10:")
print(margeff1)

margeff2 <- margEff(tobit.reg, xValues = x2)
print("Marginal Effects at Quota = 2000:")
print(margeff2)

print("Quota, Days")
summary(lin.reg)$coefficients[2:3]
