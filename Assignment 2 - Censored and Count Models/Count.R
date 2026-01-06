data <- read.csv("Forum_Posts.csv", header = TRUE)
str(data)
hist(data$posts)

# Number 1
poisson.reg <- glm(posts ~., family = poisson, data = data)
summary(poisson.reg)

# Number 2
y = data$posts
yhat <- predict(poisson.reg, type = 'response')
sum((y - yhat)^2/yhat)/(nrow(data) - length(coef(poisson.reg)))

# Number 3
library(MASS)
nb.reg <- glm.nb(posts ~., data = data)
summary(nb.reg)

# Number 4
# Theta is the dispersion parameter of a negative binomial model. 1/theta is the variance of the gamma distribution for the individaul random effects 
# If theta is large (>1), 1/theta is close to 0, and the negative binomal model conveges to poisson
# If theta is small (<1), it indicaties significant dispersion

# Number 5
cbind(AIC(poisson.reg), AIC(nb.reg))
cbind(BIC(poisson.reg), BIC(nb.reg))
#nb model fits better - smaller AIC and BIC values

# Number 6
xbar <- colMeans(data)[2:5]
xb.pos <- crossprod(coef(poisson.reg), c(1,xbar))
xb.nb <- crossprod(coef(nb.reg), c(1,xbar))
k <- 0:20
p1 <- dpois(k, exp(xb.pos))
p2 <- dnbinom(k, size = nb.reg$theta, mu = exp(xb.nb))
plot(k, p2, pch=16, xlab = "posts", ylab = "probs")
points(k, p1)
legend("topright", c("Poisson", "Neg Binomial"), pch = c(1,16))
