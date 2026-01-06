install.packages("AER")
library(AER)

mydata <- read.csv("Education_data.csv", header = TRUE)
str(mydata)

hist(mydata$wage, breaks = 50)
hist(log(mydata$wage), breaks = 50)

# Number 1
ols.res <- lm(log(wage)~educ+exper+I(exper^2), data = mydata)
summary(ols.res)

# Number 3
stage1 <- lm(educ ~ nearc4+exper+I(exper^2), data = mydata)
summary(stage1)

# Number 4
stage2 <- lm(log(wage) ~ fitted(stage1)+exper+I(exper^2), data=mydata)
summary(stage2)

# Number 5
TSLS.res <- ivreg(log(wage)~educ+exper+I(exper^2)|nearc4+exper+I(exper^2), data = mydata)
summary(TSLS.res)

# Number 6
cbind(coef(ols.res), coef(TSLS.res))
