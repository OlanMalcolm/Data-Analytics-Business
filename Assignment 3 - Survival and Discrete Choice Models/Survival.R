data <- read.csv('Bank_Attrition.csv', header = TRUE)

install.packages('survival')
library(survival)

# Number 1
surv.res <- survreg(Surv(ChurnTime, 1-Censored) ~. -CustomerID, data = data, dist = 'weibull')
summary(surv.res)

# Number 2
a <- 1/surv.res$scale
a
beta <- -coef(surv.res)*a
beta

# Number 3
xbar <- colMeans(data[,2:10])
xbeta <- crossprod(c(1,xbar), beta)
curve(exp(c(xbeta))*a*x^(a-1), xlim=c(0,20), xlab='time', ylab = 'hazard')
      
# Number 4
curve(dweibull(x, shape = a, scale = exp(-xbeta/a)), xlab = 'time', ylab = 'density', xlim = c(0,20), ylim = c(0, 0.12))
hist(data$ChurnTime[data$Censored==0], breaks = 50, freq = FALSE, add = TRUE, col = NULL)
