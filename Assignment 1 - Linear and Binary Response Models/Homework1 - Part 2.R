# Number 1
data <- read.csv('Loan.csv', header = TRUE)
str(data)
data$Education <- as.factor(data$Education)
str(data)

# Number 2
linreg <- lm(Loan ~., data = data)
summary(linreg)
# Holding all other variables constant, individuals with Education = 2 are 
# expected to have loan amounts higher by approximately 0.152 (on the scale of Loan) compared to 
# those with Education = 1.

# Number 3
# Represents a predicted probability of success for a particular observation
head(sort(predict(linreg)), 10) # 10 smallest values
tail(sort(predict(linreg)), 10) # 10 largest values

# Number 4
logit.res <- glm(Loan~., family = binomial(link=logit), data = data)
summary(logit.res)
yhat <- rep(1,nrow(data))
yhat[predict(logit.res, type = 'response') < mean(data$Loan)] <- 0

# Number 5
confusion <- table(yhat, data$Loan)
confusion
sum(diag(confusion)/ sum(confusion)) #overall PCP
confusion[1,1] / sum(confusion[,1]) #y=0 PCP
confusion[2,2] / sum(confusion[,2]) #y=1 PCP

# Number 6
coef(logit.res)
colMeans(data[,1:4])
exp(-13.17783285 + 0.05979075 * 73.774200 + 0.58707882 * 2.396400 +  0.16267911 * 1.937938 + 3.91060897) / (1 + exp(-13.17783285 + 0.05979075 * 73.774200 + 0.58707882 * 2.396400 +  0.16267911 * 1.937938 + 3.91060897))
xvalues <- data.frame(Loan = 0.096000, Income = 73.774200, Family = 2.396400, CCAvg = 1.937938, Education = '2')
predict(logit.res, newdata = xvalues, type = 'response')

# Number 7
coef(linreg)
coef(logit.res)
# Not comparable. On different scales/magnitudes

# Number 8
xvalues <- data.frame(Loan = 0.096000, Income = 73.774200, Family = 2.396400, CCAvg = 1.937938, Education = '2')
xb <- predict(logit.res, newdata = xvalues)
PE.logit <- dlogis(xb) * coef(logit.res)[-1]
PE.lin <- coef(linreg)[-1]
cbind(PE.lin, PE.logit)
# Are comparable. They are measuring the same effects
# These partial effects mean: that for example - if someones income increased by 1 unit, the probability of 
# them receiving a loan will increase by 0.002390594