rm(list=ls())

require(DAAG)

data = cars
speed = data$speed
dist = data$dist

write.table(data, file= "cars.csv", sep=",", row.names = F)

head(cars)
summary(cars)
#plot(cars, col='blue', pch=20, cex=2, main="Relationship between Speed and Stopping Distance for 50 Cars",
 #    xlab="Speed in mph", ylab="Stopping Distance in feet")

# Analysis
  scatter.smooth(x=speed, y=dist, main="Relationship")
  
  # divide graph area in 2 columns
  par(mfrow=c(1, 2))  
  # box plot for 'speed'
  boxplot(speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(speed)$out)) 
  # box plot for 'distance'
  boxplot(dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(dist)$out))  
  
  library(e1071)
  par(mfrow=c(1, 2))  # divide graph area in 2 columns
  plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
  polygon(density(cars$speed), col="red")
  plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
  polygon(density(cars$dist), col="red")
  
  cor(speed, dist)
  
  
  scatter.smooth(x=speed, y=dist, main="Relationship")
 
  #y = lm(formula = dist ~ speed, data=cars)
  #y = lm(formula = dist ~ speed^2, data=cars)
  #y = lm(formula = dist ~ log(speed), data=cars)
  #y = lm(formula = dist ~ exp(speed), data=cars)
  #y = lm(formula = dist ~ speed^3, data=cars)
  #y = lm(formula = dist ~ sqrt(speed), data=cars)
  y = lm(formula = dist ~ (speed+log(speed)), data=cars)

  y = lm(formula = dist ~ (-1+speed), data=cars) 
  y = lm(formula = dist ~ (speed+speed^2), data=cars)
  
  #y = lm(formula = dist ~ (1+speed), data=cars)
 
  summary(y)
  anova(y)
  layout(matrix(1:4,2,2))
  plot(y)
  
#train
  set.seed(100)
  trainingRowIndex = sample(1:nrow(cars), 0.8*nrow(cars))
  trainingdata = cars[trainingRowIndex, ]
  testData = cars[-trainingRowIndex, ]
  
#y1 = lm(formula = dist ~ speed, data=trainingdata)
#y2 = lm(formula = dist ~ (1+speed), data=trainingdata)

#anova(y1,y2)
#jtest(y1,y2)  

require(glmulti)

res<-glmulti(dist~speed, xr=speed, level=20, data=trainingdata,crit="aicc", confsetsize=128)
plot(res)
print(res)
    
  
  #y = lm(formula = dist~speed, data=trainingdata)
  #y = lm(formula = dist ~ (speed+log(speed)), data=trainingdata)
  y = lm(formula = dist ~ (-1+speed), data=trainingdata) 
  Predictions = predict(y, testData)

  summary(y)
  anova(y)
  plot(speed, dist)
  abline(y)
  layout(matrix(1:4,2,2))
  plot(y)

#cross-validation
  #y = cv.lm(data, form.lm = dist~speed,m = 10, seed=110, dots=F)

require(lmtest)
require(car)
scatterplot(dist ~ (1-speed), data = cars)

#add regression
fit2 <- lm(Predictions ~ testData$dist)
lgd <- c(
  paste("R^2 =", round(summary(fit2)$r.squared,3)),
  paste("Offset =", round(coef(fit2)[1],3)),
  paste("Slope =", round(coef(fit2)[2],3))
)
legend("topleft", legend=lgd)
abline(fit2, lwd=2)
legend("bottomright", legend=c("predicted ~ observed", "1:1"), col=c(1,8), lty=1, lwd=2)

