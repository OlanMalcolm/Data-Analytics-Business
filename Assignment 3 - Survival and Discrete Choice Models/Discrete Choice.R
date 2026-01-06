# Number 1
install.packages('mlogit')
library(mlogit)
data <- read.csv('Commute_Mode.csv', header = TRUE)
logdata <- mlogit.data(data, shape = 'long', choice = 'choice', alt.var = 'mode', chid.var = 'id')
head(logdata)

# Number 2
ml.res <- mlogit(choice ~ cost + time , data = logdata)
summary(ml.res)

# Number 3
# bus' intercept is normalized to 0 - why there is 3 intercepts
# + intercept means that car generates higer choice on average given everything else equal
# - indicate that these two options generate less than baseline alternative

# Number 4
head(fitted(ml.res, outcome = FALSE))
# all are probability measures. Each row is an individual and each column is the 
# probability of being chosen

# Number 5
cost.avg <- tapply(logdata$cost, logdata$alt, mean)
cost.avg
time.avg <- tapply(logdata$time, logdata$alt, mean)
time.avg
xval <- data.frame(time = time.avg, cost = cost.avg)
xval

effects(ml.res, covariate = "time", data = xval)
# 