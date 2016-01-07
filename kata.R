rm(list=ls()) #mazani promenych
cat("\014")  #mazani konzole
carsa <- read.csv("~/Scholla/___FEL/21rocnik/DVZ/AutaRC/cars.csv")
cars <- carsa
cars<- na.omit(cars)
cars$Origin <- as.numeric(cars$Origin)
y <- cars$Origin
cars<- cars[, c(1, 3, 7:8, 12, 18,25,24)]
#cars<- cars[, c(1, 6:7, 10:12, 18,25,26)]
dim(cars)
summary(cars)
names(cars)
#View(cars)

cars$Manufacturer <- as.numeric(cars$Manufacturer) 
cars$DriveTrain <- as.numeric(cars$DriveTrain)

cars$Type <- as.numeric(cars$Type)
#View(cars)

library(e1071)
library(rpart)
#data(Glass, package="mlbench")
## split data into a train and test set

data <- cars

dat <- data.frame(x=data,y=as.factor(y))
plot(dat$x.Manufacturer, col=y)

index <- 1:nrow(dat)
testindex <- sample(index, trunc(length(index)/2))
testset <- dat[testindex,]
trainset <- dat[-testindex,]

## svm
svm.model <- svm(y ~ ., data = trainset, cost = 10, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])

## rpart
rpart.model <- rpart(y ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type = "class")

## compute svm confusion matrix
table(pred = svm.pred, true = testset[,9])

## compute rpart confusion matrix
table(pred = rpart.pred, true = testset[,9])
plot(rpart.model)
text(rpart.model)

