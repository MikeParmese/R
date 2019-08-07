##### WEEKLY DATA SET #####
library(ISLR)
library(psych)

describe(Weekly)
plot(Weekly$Year, Weekly$Lag1)

##### LOGISTIC REGRESSION #####
logistic <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly, family = binomial)
summary(logistic)
#Lag2 looks to be statistically significant

coef(logistic)
summary(logistic)$coef

logistic.prob <- predict(logistic, type = "response")
logistic.prob[1:10]

attach(Weekly)
#shows 0 and 1 for direction
contrasts(Direction)

#creates a vector of 1089 Down elements
logistic.pred <- rep("Down", 1089)
#transforms to Up all of the elements that exceed a value of 0.5
logistic.pred[logistic.prob>.5]="Up"

#confusion matrix of prediction & direction
table(logistic.pred, Direction)


#diagonal values added, divided by total values n
(54+557)/1089
#same thing, different coding
mean(logistic.pred==Direction)


#create training dataset
train=(Year<=2008)
#create subset of dates 2009 and later
Weekly.2008<-Weekly[!train,]
#check for correct subset (104 weeks)
dim(Weekly.2008)

Direction.2008 = Direction[!train]

#logistic regression of subsetted data
logistic2<- glm(Direction ~ Lag2, data=Weekly, family = binomial)
logistic.prob2<- predict(logistic2,Weekly.2008, type="response")

#confusion matrix of regressed subsetted data
logistic.pred2<- rep("Down", 104)
logistic.pred2[logistic.prob2>0.5]="Up"

table(logistic.pred2, Direction.2008)

mean(logistic.pred2==Direction.2008)
mean(logistic.pred2!=Direction.2008)
#test error rate is 37.5



##### LDA #####
library(MASS)
fit.lda <- lda(Direction ~ Lag2, data=Weekly, subset = train)
fit.lda

pred.lda <- predict(fit.lda, Weekly.2008)
table(pred.lda$class, Direction.2008)

##### QDA #####
fit.qda <- qda(Direction ~ Lag2, data=Weekly, subset=train)
fit.qda

pred.qda <- predict(fit.qda, Weekly.2008)
table(pred.qda$class, Direction.2008)

##### KNN #####
library(class)
train.x<- as.matrix(Lag2[train])
test.x<- as.matrix(Lag2[!train])
train.direction <- Direction[train]
set.seed(1)
pred.knn <- knn(train.x, test.x, train.direction, k=1)
table(pred.knn, Direction.2008)




##### AUTO DATA SET #####
attach(Auto)
describe(Auto)
median(mpg)
mpg01<- rep(0,length(mpg))
mpg01[mpg>median(mpg)] <- 1
Auto<- data.frame(Auto, mpg01)

cor(Auto[,-9])
pairs(Auto)

boxplot(cylinders ~ mpg01, data=Auto, main="Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main="Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data=Auto, main="Weight vs mpg01")
boxplot(acceleration ~ mpg01, data=Auto, main="Acceleration vs mpg01")
boxplot(year ~ mpg01, data=Auto, main="Year vs mpg01")

train<- (year %% 2==0)
Auto.train <- Auto[train,]
Auto.test <- Auto[!train,]
mpg01.test <- mpg01[!train]


##### LDA #####
fit.lda <- lda(mpg01 ~ cylinders+weight+displacement+horsepower, data=Auto, subset=train)
fit.lda

pred.lda<- predict(fit.lda, Auto.test)
table(pred.lda$class, mpg01.test)

mean(pred.lda$class !=mpg01.test)
# test error rate of 12.63%


##### QDA #####
fit.qda <- qda(mpg01 ~ cylinders+weight+displacement+horsepower, data=Auto, subset=train)
fit.qda

pred.qda <- predict(fit.qda, Auto.test)
table(pred.qda$class, mpg01.test)

mean(pred.qda$class != mpg01.test)
# test error rate of 12.18%


##### LOGISITC #####
fit.glm <- glm(mpg01 ~ cylinders+weight+displacement+horsepower, data=Auto, family = binomial, subset=train)
summary(fit.glm)

probs<- predict(fit.glm, Auto.test, type="response")
pred.glm<- rep(0,length(probs))
pred.glm[probs>0.5]<-1
table(pred.glm, mpg01.test)

mean(pred.glm !=mpg01.test)
# test error rate of 12.08%


##### KNN #####
train.x<- cbind(cylinders, weight, displacement, horsepower)[train,]
test.x <- cbind(cylinders, weight, displacement, horsepower)[!train,]
train.mpg01<- mpg01[train]
set.seed(1)
pred.knn <- knn(train.x, test.x, train.mpg01, k=1)
table(pred.knn, mpg01.test)

mean(pred.knn !=mpg01.test)
# test error rate of 15.38% when k=1

pred.knn <- knn(train.x, test.x, train.mpg01, k=10)
table(pred.knn, mpg01.test)
mean(pred.knn !=mpg01.test)
# test error rate of 16.48% when k=10

pred.knn <- knn(train.x, test.x, train.mpg01, k=100)
table(pred.knn, mpg01.test)
mean(pred.knn !=mpg01.test)
# test error rate of 14.28% when k=100



##### CREATING FUNCTIONS #####
Power<- function(){
  2^3
}
Power()


Power2 =function (x,a){
  x^a
}

Power2(3,8)
Power2(10,3)
Power2(8,17)
Power2(131,3)


Power3 <- function(x,a){
  x^a
  result<-x^a
  return(result)
}

Power3(3,8)

x <- 1:10
plot(x,Power3(x,2),log="xy",
     xlab="Log of x",
     ylab="Log of x^2",
     main="Log of x^2 vs Log of x")

PlotPower<- function(x,a){
  plot(x, Power3(x,a))
}

PlotPower(1:10, 3)



##### BOSTON DATA SET #####
library(MASS)
attach(Boston)

crim01<-rep(0,length(crim))
crim01[crim > median(crim)]<-1
Boston <- data.frame(Boston, crim01)

train<- 1:(length(crim)/2)
test<- (length(crim)/2+1):length(crim)

Boston.train<- Boston[train,]
Boston.test<- Boston[test,]
crim01.test <- crim01[test]  


##### LOGISTIC #####
fit.glm <- glm(crim01 ~ . - crim01 - crim, data = Boston, family = binomial, subset = train)
probs<- predict(fit.glm, Boston.test, type="response")
pred.glm<- rep(0, length(probs))  
pred.glm[probs>0.5]<-1
table(pred.glm, crim01.test)
mean(pred.glm != crim01.test)
# test error rate of 18.18%

fit.glm <- glm(crim01 ~ . - crim01 - crim - chas - nox, data = Boston, family = binomial, subset = train)
probs <- predict(fit.glm, Boston.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, crim01.test)
mean(pred.glm != crim01.test)
#test error rate of 15.81%


##### LDA #####
fit.lda <- lda(crim01 ~ . - crim01 - crim, data = Boston, subset = train)
pred.lda <- predict(fit.lda, Boston.test)
table(pred.lda$class, crim01.test)
mean(pred.lda$class != crim01.test)
#test error rate of 13.43%

fit.lda <- lda(crim01 ~ . - crim01 - crim - chas - nox, data = Boston, subset = train)
pred.lda <- predict(fit.lda, Boston.test)
table(pred.lda$class, crim01.test)
mean(pred.lda$class != crim01.test)
#test error rate of 15.01%


##### KNN #####
train.X <- cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[train, ]
test.X <- cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[test, ]
train.crim01 <- crim01[train]
set.seed(1)
pred.knn <- knn(train.X, test.X, train.crim01, k = 1)
table(pred.knn, crim01.test)
mean(pred.knn != crim01.test)
# test error rate of 45.84% for k=1

pred.knn <- knn(train.X, test.X, train.crim01, k = 10)
table(pred.knn, crim01.test)
mean(pred.knn != crim01.test)
# test error rate of 11.85% for k=10

pred.knn <- knn(train.X, test.X, train.crim01, k = 100)
table(pred.knn, crim01.test)
mean(pred.knn != crim01.test)
# test error rate of 49.01% for k=100







