# ML2 Workshop2
# Ana Maria Sandoval Jimenez
# Tree Models: Ensemble Methods

install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
require(rpart)
require(rpart.plot)
require(randomForest)


# Exercise 1 Understanding bootstrap sampling

n<-100
vec<-1:n
bs.samp<-sample(vec,size=n,replace=T)
bs.samp
sort(bs.samp)
table(bs.samp)
table((table(bs.samp)))
#numer of unique values in the bootstrapped sample
length(unique(bs.samp))
#number of the out of bag values
n.OOB<-n-sum(table((table(bs.samp))))

n<-100
vec<-1:n
n.OOB <- numeric(length = 100L)
for(i in 1:100)
{
  bs.samp_100<-sample(vec,size=n,replace=T)
  sort(bs.samp_100) # 
  table(bs.samp_100) #
  t = table((table(bs.samp_100))) #1 2 
  print(t)
  length(unique(bs.samp_100)) #=8
  n.OOB[i]<-(n-sum(table((table(bs.samp_100)))))/100
}

n.OOB
plot(n.OOB, type = "l")
cumsum(n.OOB)
  



# Exercise 2 Bagging a regression tree “longhand”

library(MASS)
?Boston

library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)


for (i in 1:15){
  bag.samp<-sample(train,size=length(train),replace=T)
  oob.samp<-train[-bag.samp]
  tree.bag=rpart(medv~.,Boston,subset=bag.samp)
}

# Exercise 3 Bagging with R

bag.boston = randomForest(medv~., data= Boston , subset = train, mtry = 13, importance =TRUE)
bag.boston

# Exercise 5 Random forest using R


bag.boston = randomForest(medv~., data= Boston , subset = train, mtry = 3, importance =TRUE)
bag.boston

# Exercise 6 Variable importance

importance(bag.boston)
varImpPlot(bag.boston)
tree.boston$variable.importance.


# Exercise 7 Homework: Calculating variable importance
tree.boston=rpart(medv~.,Boston,subset=train,cp=0.03)
rpart.plot(tree.boston)
tree.boston$frame

