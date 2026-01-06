# Number 1
data <- read.csv("UsedCars.csv", header = TRUE)
attach(data)
lin.reg <- lm(Price~Age+KM+HP+Metallic+Automatic+CC+Doors+Gears+Weight, data = data)
summary(lin.reg)

# Number 2
yhat <- fitted(lin.reg)
yhat
uhat <- resid(lin.reg)
uhat
cbind(Age, KM, HP, Metallic, Automatic, CC, Doors, Gears, Weight, Price, yhat, uhat)[1:10,]

# Number 3
bhat <- summary(lin.reg)$coefficients[,1]
se <- summary(lin.reg)$coefficients[,2]
tstat <- bhat / se
cbind(tstat, summary(lin.reg)$coefficients[,3])

# Number 4
df <- lin.reg$df.residual
alpha <- 1-0.95
qt(1-alpha/2, df)

# Number 5
pval <- 2 * pt(-abs(tstat), df)
cbind(pval, summary(lin.reg)$coefficients[,4])

# Number 6
# Answer - Age, KM, HP, Automatic, Gears, Weight

# Number 7
summary(lin.reg)$r.squared
var(yhat) / var(Price)

# Number 8
install.packages("car")
library(car)
vif(lin.reg)

# Number 9
lin.reg.weight <- lm(Weight ~ Age+KM+HP+Metallic+Automatic+CC+Doors+Gears, data = data)
r2.weight <- summary(lin.reg.weight)$r.squared
1/(1-r2.weight)

# Number 10
lin.reg.final <- lm(Price ~ Age + KM + HP + Automatic + Gears + Weight, data = data)
summary(lin.reg.final)

# Number 11
summary(lin.reg)$r.squared
summary(lin.reg)$adj.r.squared

summary(lin.reg.final)$r.squared
summary(lin.reg.final)$adj.r.squared
# First model has a bigger R2 due to more indep vars, final model has a bigger Adj R2

# Number 12
summary(lin.reg.final)$coefficients[2,1] * 1
summary(lin.reg.final)$coefficients[3,1] * 10000

detach(data)