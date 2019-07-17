#Machine Learning Project
rm(list=ls())
options(warn=-1) # Get rid of warnings. Why? if we want to include All-Star data, it is null/0 for some players, and causes warnings to pop up. Unecessary if don't use All-Star.
library(ISLR)
library(class)
library(MASS)
library(pROC)
library(verification)

#Loading 2015 Player Data. This will be used to evaluate whether or not a player can be considered active
data_2015 <- read.csv('D:/MSBAMachineLearning/Project2015.csv')

#Loading data for top 1000 scorers in NBA history. Obviously more people have played, but including them is superfluous.
career_stats <- read.csv('D:/MSBAMachineLearning/ProjectBasketball.csv')

#Changing players to active if they are in the 2015 list
count <- 1
for (player in (career_stats$PLAYER)){
  for (name in (data_2015$Player)){
    if (player == name){career_stats[count,'ACTIVE'] <-  1}
  }
  count <- count + 1}

#Loading All-Star Data
all_star <- read.csv(('D:/MSBAMachineLearning/allstar.csv'))

#Calculating and adding number of all star appearances to player info
count <- 1
for (player in (career_stats$PLAYER)){
  for (name in all_star$Player){
    if (player == name){career_stats[count,'ALLSTARNO'] <- all_star$Tot[count]}
  }
  count <- count + 1 
}

#Loading Data With List of Hall of Famers
hof <- read.csv(('D:/MSBAMachineLearning/hof.csv'))

#Checking to see if a player is currently in the Hall of Fame
count <- 1
for (player in (career_stats$PLAYER)){
  for (name in (hof$Inductees)){
    if (player == name){career_stats[count,'HOF'] <-  1}
  }
  count <- count + 1}

#Getting stats of only retired players - not worth looking at active ones as we cannot test to see their HOF status yet.
retiredp <- career_stats[career_stats$ACTIVE==0,]

#What is statistically significant?
model <- glm(HOF ~ MIN + PTS + REB + AST + ALLSTARNO + MPG + ASPG + PPG + RPG,data = retiredp,family = binomial)
summary(model)
#Notably, and perhaps surprisingly, assists are not significant. Most significant is Minutes Per Game, Points Per Game, and Rebounds Per Game

#Getting train and test sets with standard .8 relationship. Subject to change
train <- sample(1:nrow(retiredp),.8*nrow(retiredp))
test <- setdiff(1:nrow(retiredp),train)

#First Model, Using Only whole-career stats-not per game averages. LDA.  Just exploring data
model <- lda(HOF ~ MIN + PTS + REB + AST, data=retiredp,subset=train)
model
pred <- predict(model,retiredp[test,])
confusm <- table(retiredp$HOF[test],pred$class)
confusm
predright <- (confusm[1,1] + confusm[2,2])/sum(confusm)
print(paste('The overall percentage of correct predictions is:',predright))
print(paste('The overall error rate is:',1-predright))
type1 <- confusm[1,2]/sum(confusm[1,])
print(paste('The type 1 error rate is:',type1))
type2 <- confusm[2,1]/sum(confusm[2,])
print(paste('The type 2 error rate is:',type2))
pwr <- 1-type2
print(paste('The power of the model is:',pwr))
precision <- confusm[2,2]/sum(confusm[,2])
print(paste('The precision of the model is:',precision))

#QDA Using Above methodology
model <- qda(HOF ~ MIN + PTS + REB + AST, data=retiredp,subset=train)
model
pred <- predict(model,retiredp[test,])
confusm <- table(retiredp$HOF[test],pred$class)
confusm
predright <- (confusm[1,1] + confusm[2,2])/sum(confusm)
print(paste('The overall percentage of correct predictions is:',predright))
print(paste('The overall error rate is:',1-predright))
type1 <- confusm[1,2]/sum(confusm[1,])
print(paste('The type 1 error rate is:',type1))
type2 <- confusm[2,1]/sum(confusm[2,])
print(paste('The type 2 error rate is:',type2))
pwr <- 1-type2
print(paste('The power of the model is:',pwr))
precision <- confusm[2,2]/sum(confusm[,2])
print(paste('The precision of the model is:',precision))
roc.plot(retiredp$HOF[test],pred$posterior[,2], main="ROC Curve")

#Instead, lets use per game averages-ones that GLM listed as most significant.
model <- lda(HOF ~ PPG + RPG + MPG, data=retiredp,subset=train)
model
pred <- predict(model,retiredp[test,])
confusm2 <- table(retiredp$HOF[test],pred$class)
confusm2
predright <- (confusm[1,1] + confusm[2,2])/sum(confusm)
print(paste('The overall percentage of correct predictions is:',predright))
print(paste('The overall error rate is:',1-predright))
type1 <- confusm[1,2]/sum(confusm[1,])
print(paste('The type 1 error rate is:',type1))
type2 <- confusm[2,1]/sum(confusm[2,])
print(paste('The type 2 error rate is:',type2))
pwr <- 1-type2
print(paste('The power of the model is:',pwr))
precision <- confusm[2,2]/sum(confusm[,2])
print(paste('The precision of the model is:',precision))
roc.plot(retiredp$HOF[test],pred$posterior[,2], main="ROC Curve 2")

#Every time the model runs, this explains the predictions it got wrong. Fun to see who predicted would get in, and who wouldn't
count <- 1
for (val in pred$class){
  if (is.na(val)==TRUE){next}
  else if (val ==1 & retiredp$HOF[test][count]==0){
    print(paste('The model predicted', retiredp$PLAYER[test][count],'would make the hall of fame but he did not.'))
  }
  else if (val ==0 & retiredp$HOF[test][count]==1){
    print(paste('The model predicted', retiredp$PLAYER[test][count],'would not make the hall of fame but he did.'))
  }
  count <- count+1
}

#In general, model is great at predicting people NOT in HOF but has relatively high type 2 error.  Maybe that could be helped by shifting threshold.
count <- 1
for (i in pred$posterior[,2]){
  if (pred$posterior[,2][count] >= .35){
    pred$class[count] <- 1}
  count <- count + 1}
confusm3 <- table(retiredp$HOF[test],pred$class)
print(confusm3)
#Somewhat surprisingly, this DOES NOT reduce type 2 error too much.  This indicates that there is something fundamentally different about players model missed.
#Given context of predictions and model attributes, makes sense.  Ignore assists and defense stats in order to have lower error rate overall, but miss out on those types of players.

#Same as above, but using QDA instead
model <- qda(HOF ~ PPG + RPG + MPG, data=retiredp,subset=train)
pred <- predict(model,retiredp[test,])
confusm <- table(retiredp$HOF[test],pred$class)
confusm
predright <- (confusm[1,1] + confusm[2,2])/sum(confusm)
print(paste('The overall percentage of correct predictions is:',predright))
print(paste('The overall error rate is:',1-predright))
type1 <- confusm[1,2]/sum(confusm[1,])
print(paste('The type 1 error rate is:',type1))
type2 <- confusm[2,1]/sum(confusm[2,])
print(paste('The type 2 error rate is:',type2))
pwr <- 1-type2
print(paste('The power of the model is:',pwr))
precision <- confusm[2,2]/sum(confusm[,2])
print(paste('The precision of the model is:',precision))

#Every time the model runs, this explains the predictions it got wrong. Fun to see who predicted would get in, and who wouldn't
count <- 1
for (val in pred$class){
  if (is.na(val)==TRUE){next}
  else if (val ==1 & retiredp$HOF[test][count]==0){
    print(paste('The model predicted', retiredp$PLAYER[test][count],'would make the hall of fame but he did not.'))
  }
  else if (val ==0 & retiredp$HOF[test][count]==1){
    print(paste('The model predicted', retiredp$PLAYER[test][count],'would not make the hall of fame but he did.'))
  }
  count <- count+1
}

#To predict active players, let's not use train/test data. Instead, let's use ALL the data we have.  Best model came from LDA of MPG,PPG,RPG, so let's use that.
#Update: after repeated trials as shown below, including ALLSTARNO was best model. Use this model now.
model <- qda(HOF ~ MPG + RPG + PPG+ALLSTARNO,data=retiredp)
activep <- career_stats[career_stats$ACTIVE==1,]
pred <- predict(model,activep)
count <- 1
for (val in pred$class[1:50]){
  if (val==1){
    print(paste('The model predicts',activep$PLAYER[count],'is on pace for a hall of fame career.'))}
  if (val==0){
    print(paste('The model suggests',activep$PLAYER[count],'should get good.'))
  }
  count <-  count + 1
}

#Minimizing error with KNN model. KNN is not very good in this case.
train.x <- cbind(retiredp[train,'PPG'],retiredp[train,'RPG'],retiredp[train,'MPG'])
test.x <- cbind(retiredp[test,'PPG'],retiredp[test,'RPG'],retiredp[test,'MPG'])
train.direct <- retiredp$HOF[train]
bestk <- 0
besterror <- 1
for (k in seq(1,49,by=1)){
  knn.pred <- knn(train = train.x,test = test.x,train.direct,k=k)
  confusm <- table(retiredp$HOF[test],knn.pred)
  errorrate <- 1-((confusm[1,1] + confusm[2,2])/sum(confusm))
  if (errorrate < besterror){bestk <- k;besterror <- errorrate}}
knn.pred <- knn(train = train.x,test = test.x,train.direct,k=bestk)
confusm <- table(retiredp$HOF[test],knn.pred)
confusm

#Now lets try to minimize type 2 error, so to not miss out on HOF quality players.
best_t2 <- 1
for (k in seq(1,49,by=1)){
  knn.pred <- knn(train = train.x,test = test.x,train.direct,k=k)
  confusm <- table(retiredp$HOF[test],knn.pred)
  type2 <- confusm[2,1]/sum(confusm[2,])
  if (type2 < best_t2){bestk <- k;best_t2 <- type2}}
knn.pred <- knn(train = train.x,test = test.x,train.direct,k=bestk)
confusm <- table(retiredp$HOF[test],knn.pred)
confusm

#After all this exploration, let's decide which model has best error rate on average
count <- 1
qdaerrors <- c()
qdatype2 <- c()
ldaerrors <- c()
ldatype2 <- c()
knnerrors <- c()
knntype2 <- c()
lda2type2 <- c()
lda2errors <- c()
qda2type2 <- c()
qda2errors <- c()
qdatype1 <-  c()
ldatype1 <- c()
knntype1 <- c()
lda2type1 <- c()
qda2type1 <- c()

#Let's go crazy and run models 1000 times. Takes a couple minutes.
for (i in seq(1,1000)){
  train <- sample(1:nrow(retiredp),.8*nrow(retiredp))
  test <- setdiff(1:nrow(retiredp),train)
  
  #LDA
  model <- lda(HOF ~ MPG + RPG + PPG,data=retiredp,subset = train)
  pred <- predict(model,retiredp[test,])
  confusm <- table(retiredp$HOF[test],pred$class)
  predright <- (confusm[1,1] + confusm[2,2])/sum(confusm)
  type2 <- confusm[2,1]/sum(confusm[2,])
  type1 <- confusm[1,2]/sum(confusm[1,])
  ldaerrors[count] <- 1-predright
  ldatype2[count] <- type2
  ldatype1[count] <- type1
  
  #QDA
  model <- qda(HOF ~ MPG + RPG + PPG,data=retiredp,subset = train)
  pred <- predict(model,retiredp[test,])
  confusm <- table(retiredp$HOF[test],pred$class)
  predright <- (confusm[1,1] + confusm[2,2])/sum(confusm)
  type2 <- confusm[2,1]/sum(confusm[2,])
  type1 <- confusm[1,2]/sum(confusm[1,])
  qdaerrors[count] <- 1-predright
  qdatype2[count] <- type2
  qdatype1[count] <- type1
  
  #LDA2
  model <- lda(HOF ~ MPG + RPG + PPG + ALLSTARNO,data=retiredp,subset = train)
  pred <- predict(model,retiredp[test,])
  confusm <- table(retiredp$HOF[test],pred$class)
  predright <- (confusm[1,1] + confusm[2,2])/sum(confusm)
  type2 <- confusm[2,1]/sum(confusm[2,])
  type1 <- confusm[1,2]/sum(confusm[1,])
  lda2errors[count] <- 1-predright
  lda2type2[count] <- type2
  lda2type1[count] <- type1
  
  #QDA2
  model <- qda(HOF ~ MPG + RPG + PPG + ALLSTARNO,data=retiredp,subset = train)
  pred <- predict(model,retiredp[test,])
  confusm <- table(retiredp$HOF[test],pred$class)
  predright <- (confusm[1,1] + confusm[2,2])/sum(confusm)
  type2 <- confusm[2,1]/sum(confusm[2,])
  type1 <- confusm[1,2]/sum(confusm[1,])
  qda2errors[count] <- 1-predright
  qda2type2[count] <- type2
  qda2type1[count] <- type1
  
  #KNN
  knn.pred <- knn(train = train.x,test = test.x,train.direct,k=bestk)
  confusm <- table(retiredp$HOF[test],knn.pred)
  predright <- (confusm[1,1] + confusm[2,2])/sum(confusm)
  type2 <- confusm[2,1]/sum(confusm[2,])
  type1 <- confusm[1,2]/sum(confusm[1,])
  knnerrors[count] <- 1-predright
  knntype2[count] <- type2
  knntype1[count] <- type1
  
  count <- count + 1
}
#Best Model is in fact QDA using the ALLSTAR Data.
mean(ldaerrors)
mean(qdaerrors)
mean(knnerrors)
mean(ldatype2)
mean(qdatype2)
mean(knntype2)
mean(lda2errors)
mean(lda2type2)
mean(qda2errors)
mean(qda2type2)
errors <- c(ldaerrors,qdaerrors,knnerrors,lda2errors,qda2errors)
type2s <- c(ldatype2,qda2type2,knntype2,lda2type2,qda2type2)

barplot(c(mean(ldaerrors),mean(qdaerrors),mean(knnerrors),mean(lda2errors),mean(qda2errors)),ylab = 'Error Rate',xlab = 'Model')
axis(1,at=1:5,labels = c('LDA first model','QDA first model','KNN Model','Second LDA Model','Second QDA Model'))

barplot(c(mean(ldatype2),mean(qdatype2),mean(knntype2),mean(lda2type2),mean(qda2type2)),ylab = 'Type 2 Error Rate',xlab = 'Model')
axis(1,at=1:5,labels = c('LDA first model','QDA first model','KNN Model','Second LDA Model','Second QDA Model'))


barplot(c(mean(ldatype1),mean(qdatype1),mean(knntype1),mean(lda2type1),mean(qda2type1)),ylab = 'Type 1 Error Rate',xlab = 'Model')
axis(1,at=1:5,labels = c('LDA first model','QDA first model','KNN Model','Second LDA Model','Second QDA Model'))
