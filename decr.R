# http://www.r-exercises.com/2017/09/24/extremely-boost-your-machine-learning-solutions-part-1/

library(readr)
decr <- read.csv("~/git_repos/sandbox/german_credit.csv",header=T, sep=',')
str(decr)
factor_columns <- c(2,4,5,7,8,9,10,11,12,13,15,16,17,18,19,20)
for(i in factor_columns) {decr[,i] <- as.factor(decr[,i])}
str(decr)
X <- model.matrix(~ . - Creditability, data=decr)
X
inTraining <- sample(1:nrow(decr),size=700)
dclass_trn <- decr[inTraining,]
dclass_tst <- decr[-inTraining,]

library(MASS)
dclass_fit <- lda(Creditability ~ .,data=dclass_trn)
dclass_prd <- predict(dclass_fit,dclass_tst)
sum(dclass_tst$Creditability == dclass_prd$class)/300

library(xgboost)
dboost_trn <- xgb.DMatrix(X[inTraining,], label=decr$Creditability[inTraining])
dboost_tst <- xgb.DMatrix(X[-inTraining,], label=decr$Creditability[-inTraining])
dboost_mod <- xgboost(data = dboost_trn,
                 max_depth = 2,
                 nrounds = 300,
                 objective = "binary:logistic")
dboost_err <- mean(round(predict(dboost_mod, dboost_tst)) != getinfo(dboost_tst, 'label'))
