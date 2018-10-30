# ML3 Workshop2
# Ana Maria Sandoval Jimenez
# Support vector machines

#install.packages('e1071')
#install.packages('ISLR')
#install.packages('rpart')
#install.packages('ROCR')
library(MASS)
library(rpart)
library(ROCR)
library(ISLR)
library(e1071)

# Exercise 1 Introduction to support vector machines

set.seed (1)
x=matrix (rnorm(20*2) , ncol =2)
y=c(rep(-1,10) , rep(1 ,10) )
x[y==1 ,]= x[y==1,] + 1

# check if the variables are lineary separable

plot(x, col =(3-y)) # they are not linearly separable

# use svm
dat=data.frame(x=x, y=as.factor (y))
svmfit =svm(y~., data=dat , kernel ="linear", cost =10,
              scale =FALSE ) # fit the svm classifier

plot(svmfit , dat)

svmfit$index # see de support vectors

summary (svmfit )


#chanhe the cost to a smaller one, you ger more support vectors
svmfit =svm(y~., data=dat , kernel ="linear", cost =0.1,
            scale =FALSE )
plot(svmfit , dat)
svmfit$index

# use a set of cost parameters
set.seed (1)
tune.out=tune(svm ,y~.,data=dat ,kernel ="linear",
                ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))

# to access the cross validation errors: look for the cost with lowest cross-validation error rate
summary (tune.out)

# to get the best model
bestmod =tune.out$best.model
summary (bestmod )

# predicting

xtest=matrix(rnorm(20*2) , ncol =2)
ytest=sample(c(-1,1) , 20, rep=TRUE)
xtest[ytest ==1 ,]= xtest[ytest ==1,] + 1
testdat =data.frame(x=xtest , y=as.factor (ytest))

# predict using best model
ypred=predict(bestmod ,testdat)
table(predict=ypred , truth= testdat$y)

# mow predict using cost = 0.01

svmfit =svm(y~., data=dat , kernel ="linear", cost =.01,
            scale =FALSE )
ypred=predict(svmfit ,testdat )
table(predict =ypred , truth= testdat$y )

#Now consider a situation in which the two classes are linearly separable
x[y==1 ,]= x[y==1 ,]+0.5
plot(x, col =(y+5) /2, pch =19)
dat=data.frame(x=x,y=as.factor(y))
svmfit =svm(y~., data=dat , kernel ="linear", cost =1e5)
summary(svmfit )

# try with a smalles cost
svmfit =svm(y~., data=dat , kernel ="linear", cost =1)
summary(svmfit)
plot(svmfit,dat)


#Exercise 2 Non-linear SVMs: using different kernels

# set data
set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x, col=y)
train=sample(200,100)

# Linear kernel

svmfit=svm(y~., data=dat[train,], kernel="linear", cost=1)
plot(svmfit, dat[train,])

# Polynomial kernel with degree 2
svmfit=svm(y~., data=dat[train,], kernel="polynomial", degree=4,
           gamma=1, cost=1) # play with the cost from 1 to 10 and also degree = 3 or 4
plot(svmfit, dat[train,])

# Support Vector Machine james 9.6.2

svmfit=svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)
plot(svmfit, dat[train,])
summary(svmfit) # increase the value of cost, we can reduce the number
# of training errors. at the price of a more irregular
# decision boundary that seems to be at risk of overfitting the data

svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])
 
# perform cross-validation using tune() 

set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newdata=dat[-train,]))
# best choice of parameters involves cost=1 and gamma=2


##### ROC Curves

library(ROCR)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}

svmfit.opt=svm(y~., data=dat[train,], kernel="radial",gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values

par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")

# by increasing gamma -> more flexible fit

#roc in traun data
svmfit.flex=svm(y~., data=dat[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")


#roc in test data
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")

# SVM with Multiple Classes
# 3 class obs

set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

#fit an SVM to the data
svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)

# Application to Gene Expression Data

library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)
dat=data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out=svm(y~., data=dat, kernel="linear",cost=10)
summary(out)


table(out$fitted, dat$y)
dat.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

# fit an rpart classification tree to the Khan data.

predict(Khan.tree,newdata=dat.te,type="class")
