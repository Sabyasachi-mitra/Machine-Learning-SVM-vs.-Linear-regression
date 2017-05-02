setwd("e:/IIT kgp/high pressure processing/data/New folder")
library(xlsx)
library(hydroGOF)
library(e1071)
library(caret)
library(ROCR)

#Total phenol_seed
seed_phenol_AOA2 <- read.xlsx("final.xlsx", 1, header = TRUE)
seed_phenol_AOA2 <- seed_phenol_AOA2[,1:5]
head(seed_phenol_AOA2)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(seed_phenol_AOA2))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(seed_phenol_AOA2)), size = smp_size)
train <- seed_phenol_AOA2[train_ind, ]
test <- seed_phenol_AOA2[-train_ind, ]
train
test

plot(seed_phenol_AOA2, pch=16)

# seed Total Phenolics linear regression model
model_1 <- lm(TP ~ Pressure+Time+Temperature, data=train)
model_1
summary(model_1)

# make a prediction for each X
predict_1 <- predict(model_1, newdata = test, type = 'response')
predict_1

# RMSE error
error1 <- test$TP - predict_1
rmse <- function(error1)
{
  sqrt(mean(error1^2))
}

lmPredictionRMSE <- rmse(error1)
lmPredictionRMSE
plot(predict_1)

# svm prediction total phenolics seed
seed_phenol_AOA2
model_2 <- svm(TP ~ Pressure  + Time + Temperature , train)
train
predict_2 <- predict(model_2, test)
predict_2
summary(predict_2)
plot(predict_2)
points(seed_phenol_AOA2$TP, predict_2, col = "red", pch=4)
summary(predict_2)
class(predict_2)

# RMSE error in svm
error2 <- test$TP - predict_2
rmse <- function(error2)
{
  sqrt(mean(error2^2))
}

svrPredictionRMSE <- rmse(error2)
svrPredictionRMSE

# perform a grid search
tuneResult <- tune(svm, TP ~ Pressure  + Time + Temperature ,  
                   data = train,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult)

plot(tuneResult)

# svm best fit model
tunemodel <- tuneResult$best.model
predict_3 <- predict(tunemodel, test)
predict_3
summary(predict_3)

# RMSE error of svm tune result
error3 <- test$TP - predict_3
rmse <- function(error3)
{
  sqrt(mean(error3^2))
}

svmtunePredictionRMSE <- rmse(error3)
svmtunePredictionRMSE
plot(predict_3)

# seed antioxidant activity linear regression model
model_4 <- lm(AOA ~ Pressure + Time + Temperature, data=train)
# make a prediction for each X
predict_4 <- predict(model_4, test)

# display the predictions
points(test$AOA, predict_4, col = "blue", pch=4)
predict_4
plot(predict_3)

# RMSE error for AOA seed lm
error4 <- test$AOA - predict_4
rmse <- function(error4)
{
  sqrt(mean(error4^2))
}
lmPredictionRMSE2 <- rmse(error4)
lmPredictionRMSE2

# seed antioxidant activity model SVM
model_5 <- svm(AOA ~ Pressure  + Time + Temperature , train)
model_5
# svm prediction of seed AOA
predict_5 <- predict(model_5, newdata = test)
predict_5
summary(predict_5)

# visualisation of prediction
plot(predict_5)
points(test$AOA, predict_5, col = "red", pch=4)
summary(predict_5)
class(predict_5)

# RMSE error of svm AOA seed
error5 <- test$AOA - predict_5
rmse <- function(error5)
{
  sqrt(mean(error5^2))
}

svrPredictionRMSE2 <- rmse(error5)
svrPredictionRMSE2

# perform a grid search
tuneResult2 <- tune(svm, AOA ~ Pressure  + Time + Temperature ,  
                   data = train,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult2)

plot(tuneResult2)

# svm best fit model
tunemodel2 <- tuneResult2$best.model
predict_6 <- predict(tunemodel2, test)
predict_6
summary(predict_6)

# RMSE error of svm tune result
error6 <- test$AOA - predict_6
rmse <- function(error6)
{
  sqrt(mean(error6^2))
}

svmtunePredictionRMSE2 <- rmse(error6)
svmtunePredictionRMSE2
plot(predict_6)

#SEED COAT
#Total phenol_seed
sc_phenol_AOA2 <- read.xlsx("final.xlsx", 2, header = TRUE)
sc_phenol_AOA2 <- sc_phenol_AOA2[,1:5]
head(sc_phenol_AOA2)

## 75% of the sample size
smp_size1 <- floor(0.75 * nrow(sc_phenol_AOA2))

## set the seed coat to make your partition reproductible
set.seed(123)
train_ind1 <- sample(seq_len(nrow(sc_phenol_AOA2)), size = smp_size1)
train1 <- sc_phenol_AOA2[train_ind1, ]
test1 <- sc_phenol_AOA2[-train_ind1, ]
train1
test1

plot(sc_phenol_AOA2, pch=16)

# seed Total Phenolics linear regression model
model_7 <- lm(TP ~ Pressure+Time+Temperature, data=train1)
model_7
summary(model_7)

# make a prediction for each X
predict_7 <- predict(model_7, newdata = test1, type = 'response')
predict_7

# RMSE error
error7 <- test1$TP - predict_7
rmse <- function(error7)
{
  sqrt(mean(error7^2))
}

lmPredictionRMSE3 <- rmse(error7)
lmPredictionRMSE3
plot(predict_7)

# svm prediction total phenolics seed coat
sc_phenol_AOA2
model_8 <- svm(TP ~ Pressure  + Time + Temperature , train1)
train1
predict_8 <- predict(model_8, test1)
predict_8
summary(predict_8)
plot(predict_8)
points(sc_phenol_AOA2$TP, predict_8, col = "red", pch=4)
summary(predict_8)
class(predict_8)

# RMSE error in svm
error8 <- test1$TP - predict_8
rmse <- function(error8)
{
  sqrt(mean(error8^2))
}

svrPredictionRMSE3 <- rmse(error8)
svrPredictionRMSE3

# perform a grid search
tuneResult3 <- tune(svm, TP ~ Pressure  + Time + Temperature ,  
                   data = train1,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult3)

plot(tuneResult3)

# svm best fit model
tunemodel3 <- tuneResult3$best.model
predict_9 <- predict(tunemodel3, test1)
predict_9
summary(predict_9)

# RMSE error of svm tune result
error9 <- test1$TP - predict_9
rmse <- function(error9)
{
  sqrt(mean(error9^2))
}

svmtunePredictionRMSE3 <- rmse(error9)
svmtunePredictionRMSE3
plot(predict_9)

# seed coat antioxidant activity linear regression model
model_10 <- lm(AOA ~ Pressure + Time + Temperature, data=train1)
# make a prediction for each X
predict_10 <- predict(model_10, test1)

# display the predictions
points(test1$AOA, predict_10, col = "blue", pch=4)
predict_10
plot(predict_10)

# RMSE error for AOA seed coat lm
error10 <- test1$AOA - predict_10
rmse <- function(error10)
{
  sqrt(mean(error10^2))
}
lmPredictionRMSE4 <- rmse(error10)
lmPredictionRMSE4

# seed coat antioxidant activity model SVM
model_11 <- svm(AOA ~ Pressure  + Time + Temperature , train1)
model_11
# svm prediction of seed coat AOA
predict_11 <- predict(model_11, newdata = test1)
predict_11
summary(predict_11)

# visualisation of prediction
plot(predict_11)
points(test1$AOA, predict_11, col = "red", pch=4)
summary(predict_11)
class(predict_11)

# RMSE error of svm AOA seed coat
error11 <- test1$AOA - predict_11
rmse <- function(error11)
{
  sqrt(mean(error11^2))
}

svrPredictionRMSE4 <- rmse(error11)
svrPredictionRMSE4

# perform a grid search
tuneResult4 <- tune(svm, AOA ~ Pressure  + Time + Temperature ,  
                    data = train1,
                    ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult4)

plot(tuneResult4)

# svm best fit model
tunemodel4 <- tuneResult4$best.model
predict_12 <- predict(tunemodel4, test1)
predict_12
summary(predict_12)

# RMSE error of svm tune result
error12 <- test1$AOA - predict_12
rmse <- function(error12)
{
  sqrt(mean(error12^2))
}

svmtunePredictionRMSE4 <- rmse(error12)
svmtunePredictionRMSE4
plot(predict_6)
## Best model can be foud out from above code

# Creating prediction values
new.df2 <- read.xlsx("obj4.xlsx", 1, header = TRUE)

# seed total phenolics prediction
TP_seed <- predict(tunemodel, new.df2)
write.xlsx(TP_seed, "tp_seed.xlsx")

# seed antioxidant activity prediction
AOA_seed <- predict(tunemodel2, new.df2)
write.xlsx(AOA_seed, "aoa_seed.xlsx")

# seed coat total phenol prediction 
TP_seedc <- predict(tunemodel3, new.df2)
write.xlsx(TP_seedc, "tp_seedc.xlsx")

# seed coat antioidant prediction 
TP_seedc <- predict(tunemodel4, new.df2)
write.xlsx(TP_seedc, "aoa_seedc.xlsx")