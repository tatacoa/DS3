# ML2 workshop1
library(MASS)
?Boston

# Exercise 1
#a dimensions data set
dim(Boston)
colnames(Boston)
#b pairwise scatterplots

# load packages
require(lattice)
#install.packages("ggplot2")
require(ggplot2)

pairs(Boston)
splom(~Boston)
plotmatrix(Boston)

ggcorplot(
  data = Boston,
  var_text_size = 5,
  cor_text_limits = c(5,10))

#c Are any of the predictors associated with per capita crime rate?

round(cor(Boston), 2)

# install.packages("corrplot")
library(corrplot)
corrplot(cor(Boston), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

head(Boston)
?Boston

#d Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? 
# Pupilteacher ratios?

nose

#e How many of the suburbs in this data set bound the Charles river?
sum(which(Boston$chas == 1))

#f What is the median pupil-teacher ratio among the towns in this data set?
median(Boston$ptratio)

#g Which suburb of Boston has lowest median value of owner occupied homes? What are the values
#of the other predictors for that suburb, and how do those values compare to the overall ranges for
#those predictors ?

range(Boston$medv)
median(Boston$medv)
which(Boston$medv == 21.2)

#h In this data set, how many of the suburbs average more than seven rooms per dwelling? More
#than eight rooms per dwelling? Comment on the suburbs that average more than eight rooms per
#dwelling.

nose

##################################
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)


tree.boston=rpart(medv~.,Boston,subset=train)
print(tree.boston)

rpart.plot(tree.boston)
# Use the printcp() and plotcp() function to see whether pruning the tree will improve
# performance.
printcp(tree.boston)
plotcp(tree.boston)

prune.boston=prune(tree.boston,cp=0.016)
prune.boston
rpart.plot(prune.boston)

#Compare the mean square error (MSE) for the unpruned and pruned tree.

pred.train<-predict(tree.boston,newdata=Boston[train,])
mean((Boston$medv[train]-pred.train)^2)
pred.train.prune<-predict(prune.boston,newdata=Boston[train,])
mean((Boston$medv[train]-pred.train.prune)^2)

#Obtain the predictions for the full tree applied to the test data and for the the pruned tree applied to the
#test data. Calculate the MSE in both cases.

pred.test<-predict(tree.boston,newdata=Boston[-train,])
mean((Boston$medv[-train]-pred.test)^2)
pred.test<-predict(prune.boston,newdata=Boston[-train,])
mean((Boston$medv[-train]-pred.test)^2)

#Plot the observed median values medv against the pruned tree predictions (test data).

boston.test=Boston[-train,"medv"]
plot(pred.test,boston.test)
abline(c(0,1))

#########
#Exercise 2 Classification tree: prostate cancer

require(rpart)
?stagec
table(stagec$pgstat)

stagec$progstat <- factor(stagec$pgstat, levels = 0:1, labels = c("No", "Prog"))


#plot
head(stagec)
plot(age~progstat,data=stagec )
plot(g2~progstat,data=stagec )
plot(gleason~progstat,data=stagec )


barplot(table(stagec$eet,stagec$progstat),beside=TRUE,legend.text=TRUE )
barplot(table(stagec$ploidy,stagec$progstat),beside=TRUE,legend.text=TRUE )
barplot(table(stagec$progstat,stagec$progstat),beside=TRUE,legend.text=TRUE )


#Fit the full tree and output it as text and a diagram.

c.tree <- rpart(progstat ~ age + eet + g2 + grade + gleason + ploidy,
                data = stagec)
rpart.plot(c.tree)
print(c.tree)

# As with the regression tree we should look to see if pruning the tree is better.


printcp(c.tree)
plotcp(c.tree)
c.pruned<-prune(c.tree,cp=0.023)
print(c.pruned)
rpart.plot(c.pruned)

# use a classifier with alfa = 0:5, i.e. the mostlikely of the two outcomes is predicted
stagec$predict<-(predict(c.pruned)[,2]>0.5)
table(stagec$progstat,stagec$predict)
tt<-table(stagec$progstat,stagec$predict)
sens<-tt[2,2]/sum(tt[2,]);sens
spec<-tt[1,1]/sum(tt[1,]);spec

# The following code uses the ROCR package, to produce the ROC diagram and the AUC.
install.packages('ROCR')
library(ROCR)
p <- predict(c.pruned)[,2]
#rpart function to get the prediction for Yes
pr <- prediction(p, stagec$progstat) #convert the predictions into ROCR format
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
#ROCR function calculates everything for the ROC curve
plot(prf) #plot the ROC curve
abline(c(0,1))
AUC<-performance(pr, measure ="auc")@y.values[[1]];AUC
#RORC function calculates the AUC

