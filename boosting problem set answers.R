rm(list = ls())

#TRUE FALSE QUESTIONS
# 1. TRUE
# 2. FALSE- Does not use bootstrap.
# 3. TRUE
# 4. TRUE
# 5. FALSE- It is in fact quite often that a split of 1 works well.

###########################
####QUESTION 1 PART 1####
###########################

library(MASS)
library(ada)
set.seed(5082)

###########################
####QUESTION 1 PART 2####
###########################

head(Boston)
nobs <- nrow(Boston)
train <- sample(nobs,.8*nobs)
test <- setdiff(1:nobs, train) 

###########################
####QUESTION 1 PART 3####
###########################

boston2 <- Boston

###########################
####QUESTION 1 PART 4####
###########################

med <- median(Boston$medv)

###########################
####QUESTION 1 PART 5####
###########################

count <- 1
for (i in boston2$medv){
  if (i > med ){
    boston2$medv[count] <- 'Above'}
  else {boston2$medv[count] <- 'Below'}
  count <-  count + 1
}

###########################
####QUESTION 1 PART 6####
###########################
print(boston2$medv)

###########################
####QUESTION 1 PART 7####
###########################

boston2$medv <- as.factor(boston2$medv)
boston_class <- ada(formula=medv ~.,data =boston2,subset  = train,iter=20,bag.frac=0.5,control=rpart.control(maxdepth=1))
boston_class

###########################
####QUESTION 1 PART 8####
###########################
#Train error is .131 and OOB error is .144

###########################
####QUESTION 1 PART 9####
###########################

preds <- predict(boston_class, newdata=boston2[test,])
confusm <- table(boston2[test,"medv"], preds,dnn=c("Actual", "Predicted"))
print(confusm)

###########################
####QUESTION 1 PART 10####
###########################

iters <- c()
errors <- c()

for (i in seq(10,500,10)){
  boston_class <- ada(formula=medv ~.,data =boston2,subset  = train,iter=i,bag.frac=0.5,control=rpart.control(maxdepth=1))
  preds <- predict(boston_class, newdata=boston2[test,])
  confusm <- table(boston2[test,"medv"], preds,dnn=c("Actual", "Predicted"))
  error <-  ((confusm[1,2]+confusm[2,1])/sum(confusm)) 
  errors <- c(errors, error)
  iters <- c(iters, i)
}

plot(iters,errors)
lines(iters,errors)
# The model likely is starting to overfit after somewhere beteen 220 and 320 or so iterations,
# which is why the overall trend is the way it is down then back up.

###########################
####QUESTION 1 PART 11####
###########################
# This method could be useful if you are looking for a general trend of housing value, but is somewhat useless as 'above' or 'below' is 
# not a hard number for housing value.  Additionally, it is measuring something that is arguably subjective- housing value is more determined
# by the market, and can shift due to immeasurable factors.  Regression boosting may be better if you are looking for specific values.
# (Other answers also valid, given good arguments & analysis.)
