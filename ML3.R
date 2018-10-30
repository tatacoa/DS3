# ML3 Workshop2
# Ana Maria Sandoval Jimenez
# Boosting regression trees, introduction to support vector machines

#install.packages("gbm")
library(gbm)
require(MASS)

# Exercise 1 Investigating a boosted tree model

set.seed (1)

#b)one iteration
#i) 
mtcars.boost<-gbm(mpg ~ ., data=mtcars, n.trees=1,distribution="gaussian",shrinkage=0.1,bag.fraction=1)
#ii)
mtcars.boost
#iii)
#mse of null model
mean((mtcars$mpg-mean(mtcars$mpg))^2)
#mse of first boost
mtcars.boost$train.error
#iv)
plot(mtcars$mpg,predict(mtcars.boost,n.trees=1),ylim=c(19,22),ylab="fitted values",
     xlab="observed values")
abline(h=mean(mtcars$mpg))
abline(c(0,1))
#v)
plot(mtcars.boost)

#c)
#two iterations
mtcars.boost<-gbm(mpg ~ ., data=mtcars, n.trees=2,distribution="gaussian",shrinkage=0.1,bag.fraction=1)
mtcars.boost$train.error
plot(mtcars$mpg,predict(mtcars.boost,n.trees=1),ylim=c(19,22),ylab="fitted values",xlab="observed values")
points(mtcars$mpg,predict(mtcars.boost,n.trees=2),pch=2)

#d) three iterations
#i)
mtcars.boost<-gbm(mpg ~ ., data=mtcars, n.trees=3,distribution="gaussian",shrinkage=0.1,bag.fraction=1)
mtcars.boost$train.error
plot(mtcars$mpg,predict(mtcars.boost,n.trees=1),ylim=c(18,23),ylab="fitted values",xlab="observed values")
points(mtcars$mpg,predict(mtcars.boost,n.trees=2),pch=2)
points(mtcars$mpg,predict(mtcars.boost,n.trees=3),pch=3)
#ii)
summary(mtcars.boost)
#iii)
plot(mtcars.boost,i.var="cyl")
plot(mtcars.boost,i.var="disp")

#e) ten iterations
mtcars.boost<-gbm(mpg ~ ., data=mtcars, n.trees=10,distribution="gaussian",shrinkage=0.1,bag.fraction=1)
#i)
plot(mtcars$mpg,predict(mtcars.boost,n.trees=1),ylim=c(18,23),ylab="fitted values",xlab="observed values")
points(mtcars$mpg,predict(mtcars.boost,n.trees=4),pch=3)
summary(mtcars.boost, n.trees=4)
plot(mtcars.boost,i.var="cyl",n.trees=4)
plot(mtcars.boost,i.var="disp",n.trees=4)
plot(mtcars.boost,i.var="hp",n.trees=4)

#ii)
summary(mtcars.boost, n.trees=5)
plot(mtcars.boost,i.var="cyl",n.trees=5)
#iii)
matplot(t(predict(mtcars.boost,n.trees=1:5)),type="l",ylab="fitted values",xlab="iteration")
#iv)
summary(mtcars.boost, n.trees=10)
plot(mtcars.boost,i.var="cyl")
plot(mtcars.boost,i.var="disp")
plot(mtcars.boost,i.var="hp")
plot(mtcars.boost,i.var="wt")
matplot(t(predict(mtcars.boost,n.trees=1:10)),type="l",ylab="fitted values",xlab="iteration")

#f)
mtcars.boost<-gbm.more(mtcars.boost,n.new.trees = 90)
matplot(t(predict(mtcars.boost,n.trees=1:100)),type="l",ylab="fitted values",xlab="iteration")
summary(mtcars.boost)
plot(1:100,mtcars.boost$train.error,type="l")


#g)
mtcars.boost<-gbm.more(mtcars.boost,n.new.trees = 900)
matplot(t(predict(mtcars.boost,n.trees=1:1000)),type="l",ylab="fitted values",xlab="iteration")
summary(mtcars.boost)
plot(1:1000,mtcars.boost$train.error,type="l")
mtcars.boost$train.error[c(1,100,1000)]
plot(mtcars$mpg,predict(mtcars.boost,n.trees=1000),ylab="fitted values",xlab="observed values")
abline(h=mean(mtcars$mpg))
abline(c(0,1))


#h)
error0.1<-mtcars.boost$train.error
mtcars.boost<-gbm(mpg ~ ., data=mtcars, n.trees=1000,distribution="gaussian",shrinkage=0.02,bag.fraction=1)
plot(1:1000,error0.1,type="l")
lines(1:1000,mtcars.boost$train.error,col=3)

# Exercise 2 Boosting with the Boston data
install.packages('ISLR')
library(ISLR)
library(MASS)

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test=Boston[-train,"medv"]

boost.boston =gbm(medv~.,data=Boston[train,], distribution= 
                    "gaussian", n.trees =5000 , interaction.depth =4)
summary(boost.boston)

par(mfrow =c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")

yhat.boost=predict (boost.boston ,newdata = Boston[-train,],
                    n.trees =5000)
mean(( yhat.boost-boston.test)^2) #mse

boost.boston =gbm(medv~.,data=Boston[train,], distribution=
                    "gaussian",n.trees=5000 , interaction.depth =4, shrinkage =0.2,
                  verbose =F)
yhat.boost=predict(boost.boston ,newdata =Boston[-train,],
                      n.trees =5000)
mean((yhat.boost-boston.test)^2) #using λ = 0.2 leads to a slightly lower test MSE than λ = 0.001

# Exercise 3 Introduction to support vector machines

install.packages('e1071')

# A package to fit SVM models in R is called e1071, a helpful name which originates from a internal
# department code at the Institut für Statistik und Wahrscheinlichkeitstheorie in Vienna University!
#  Linear Support vector classifier
# Work through subsection 9.6.1 in James et al on page 330.
