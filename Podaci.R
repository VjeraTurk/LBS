rm(list=ls())

require(rattle)

data = cars
dist = data$dist

speed = data$speed
data$sqrtDist = sqrt(data$dist)
data$logDist = log(data$dist)
data$sqrtSpeed = sqrt(data$speed)
data$logSpeed = log(data$speed)
data$expSpeed = exp(data$speed)
data$powSpeed = data$speed^2

write.table(file="newData.csv",x=data,sep=",", row.names=F)
rattle()

layout(matrix(1:4,2,2))

cor(dist,speed)
cor(sqrt(dist),speed)
cor(log(dist),speed)
cor(log(dist),log(speed))

scatter.smooth(x=speed, y=dist, main="Relationship")
scatter.smooth(x=speed, y=sqrt(dist), main="Relationship")
scatter.smooth(x=speed, y=log(dist), main="Relationship")
scatter.smooth(x=log(speed), y=log(dist), main="Relationship")