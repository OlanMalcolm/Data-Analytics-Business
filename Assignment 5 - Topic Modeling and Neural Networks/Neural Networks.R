# Number 1
install.packages('neuralnet')
library(neuralnet)

# Number 2
mydata <- read.csv('Smarket.csv')
mydata <- mydata[, -c(1,8)]

mydata[,-7] <- scale(mydata[,-7])

# Number 3
n.train <- floor(nrow(mydata) * 0.8)
set.seed(1000)
ind.train <- sample(1:nrow(mydata), n.train)
data.train <- mydata[ind.train,]
data.test <- mydata[-ind.train,]

# Number 4
nn <- neuralnet(Up ~ Lag1+Lag2, data = data.train, hidden = 2, linear.output = FALSE)
load("Smarket_nn1.Rda")

# Number 5
plot(nn, rep='best')

# Number 6
nn$weights

# Number 7
data.test[1,]

s1 <- (-1.5526810) + (-0.5516457) * (-0.4006151) +  (0.9047775) * (0.3979986)
s1 <- exp(s1)/(1+exp(s1))
s1

s2 <- (-20.092955) + (1.325688) * (-0.5516457) + (33.606746) * (0.9047775)
s2 <- exp(s2)/(1+exp(s2))
s2

p1 <- (-0.2670554) + (2.5133068) * s1 + (-0.7918837) * s2
p1 <- exp(p1)/(1+exp(p1))
p1

# Number 8
compute(nn, data.test[1,])

# Number 9
nn <- neuralnet(Up ~., data = data.train, hidden = c(4,2), linear.output = FALSE)
load("Smarket_nn2.Rda")

# Number 10
plot(nn, rep = 'best')
nn$weights

# Number 11
pred <- compute(nn, data.test)
pred$net.result

# Number 12
pred.class <- rep(FALSE, nrow(data.test))
pred.class[pred$net.result>0.5] <- TRUE

# Number 13
confusion <- table(pred.class, data.test$Up)
confusion

sum(diag(confusion)) / sum(confusion)

# Number 14
logit.res <- glm(Up~., data = data.train, family = binomial(link=logit))
summary(logit.res)

logit.pred.prob <- predict(logit.res, data.test, type = 'response')
logit.pred <- rep(FALSE, nrow(data.test))
logit.pred[logit.pred.prob>0.5] <- TRUE

confusion <- table(logit.pred, data.test$Up)
confusion

sum(diag(confusion)) / sum(confusion)
