mydata <- read.csv('ShoppingVisits.csv')

plot(mydata)

# Number 1
km.res.ns <- kmeans(mydata, 3, nstart = 20)
km.res.ns
km.res.ns$cluster

# Number 2
mydata.sc <- scale(mydata)
mydata.sc
km.res.3 <- kmeans(mydata.sc, 3, nstart=20)
km.res.3

# Number 3
plot(mydata, col=km.res.ns$cluster, pch=20, main="K-Means W/o Scaling")
plot(mydata, col=km.res.3$cluster, pch=20, main="K-Means W/ Scaling")

# Number 4

# Number 5
km.res.2 <- kmeans(mydata.sc, 2, nstart=20)
km.res.4 <- kmeans(mydata.sc, 4, nstart=20)
km.res.5 <- kmeans(mydata.sc, 5, nstart=20)

# Number 6
par(mfrow=c(2,2))
plot(mydata, col=km.res.2$cluster, pch=20, main="K-Means W/ 2 Clusters")
plot(mydata, col=km.res.3$cluster, pch=20, main="K-Means W/ 3 Clusters")
plot(mydata, col=km.res.4$cluster, pch=20, main="K-Means W/ 4 Clusters")
plot(mydata, col=km.res.5$cluster, pch=20, main="K-Means W/ 5 Clusters")

# Number 7

# Number 8
ss1 <- km.res.2$totss
ss2 <- km.res.2$tot.withinss
ss3 <- km.res.3$tot.withinss
ss4 <- km.res.4$tot.withinss
ss5 <- km.res.5$tot.withinss
ss.vec <- c(ss1,ss2,ss3,ss4,ss5)
par(mfrow=c(1,1))
plot(ss.vec, type='b', xlab="Num of Clusters", ylab="Total Within-Cluster SS")

# Number 9
# 3 or 4 - likely 4
plot(mydata, col=km.res.4$cluster, pch=20, main="K-Means W/ 4 Clusters")

# Number 10
hc.res.average <- hclust(dist(mydata.sc), method = 'average')
plot(hc.res.average, cex=0.5, main = "Dendogram(Average Linkage)", xlab="", ylab="", sub="")

# Number 11
abline(h=1.8, col="red", lty=2)

# Number 12
cutree(hc.res.average, k=3)
par(mfrow=c(1,2))
plot(mydata, col=km.res.3$cluster, pch=20, main="K-Means W/ 3 Clusters")
plot(mydata, col=cutree(hc.res.average, k=3), pch=20, main="HC (Average)")

# Number 13

# Number 14
hc.res.centroid <- hclust(dist(mydata.sc), method = 'centroid')
hc.res.single <- hclust(dist(mydata.sc), method = 'single')
hc.res.complete <- hclust(dist(mydata.sc), method = 'complete')
par(mfrow=c(2,2))
plot(mydata, col=cutree(hc.res.average, k=3), pch=20, main="HC (Average)")
plot(mydata, col=cutree(hc.res.centroid, k=3), pch=20, main="HC (Centriod)")
plot(mydata, col=cutree(hc.res.single, k=3), pch=20, main="HC (Single)")
plot(mydata, col=cutree(hc.res.complete, k=3), pch=20, main="HC (Complete)")

