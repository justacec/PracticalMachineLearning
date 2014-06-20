# Setup -------------------------------------------------------------------

library(AppliedPredictiveModeling)
library(caret)
library(rattle)


# Question 1 --------------------------------------------------------------

rm(list = ls())
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y = factor(vowel.train$y)
vowel.test$y = factor(vowel.test$y)

set.seed(33833)

model_tree = train(y ~ ., data = vowel.train, method = 'rf')
model_gbm = train(y ~ ., data = vowel.train, method = 'gbm')

pred_tree = predict(model_tree, vowel.test)
pred_gbm = predict(model_gbm, vowel.test)


# Get the accuracy for the tree and the gbm
tree_accuracy = sum(pred_tree == vowel.test$y) / length(pred_tree)
gbm_accuracy = sum(pred_gbm == vowel.test$y) / length(pred_tree)

# Get the last part of the answer
agreeSub = vowel.test[pred_tree == pred_gbm,]
pred_comb = predict(model_tree, agreeSub)
comb_accuracy = sum(pred_comb == agreeSub$y) / length(pred_comb)

# The solution is the one with:
#   RF Accuracy = 0.6061
#   GBM Accuracy = 0.5325
#   Agreement Accuracy = 0.6518

# My solutions were:
#   RF Accuracy = 0.6061
#   GBM Accuracy = 0.5260
#   Agreement Accuracy = 0.6389


# Question 2 --------------------------------------------------------------

rm(list = ls())
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
model_rf = train(diagnosis ~ ., method = 'rf', data = training)
model_gbm = train(diagnosis ~ ., method = 'gbm', data = training)
model_lda = train(diagnosis ~ ., method = 'lda', data = training)

pred_rf = predict(model_rf, training)
pred_gbm = predict(model_gbm, training)
pred_lda = predict(model_lda, training)

comb_data = data.frame(rf = pred_rf, gbm = pred_gbm, lda = pred_lda, diagnosis = training$diagnosis)
model_comb = train(diagnosis ~ ., method = 'rf', data = comb_data)

pred_rf_test = predict(model_rf, testing)
pred_gbm_test = predict(model_gbm, testing)
pred_lda_test = predict(model_lda, testing)
comb_data_test = data.frame(rf = pred_rf_test, gbm = pred_gbm_test, lda = pred_lda_test, diagnosis = testing$diagnosis)
pred_comb_test = predict(model_comb, comb_data_test)

accuracy_rf = sum(pred_rf_test == testing$diagnosis) / length(pred_rf_test)
accuracy_gbm = sum(pred_gbm_test == testing$diagnosis) / length(pred_gbm_test)
accuracy_lda = sum(pred_lda_test == testing$diagnosis) / length(pred_lda_test)

accuracy_comb = sum(pred_comb_test == comb_data_test$diagnosis) / length(pred_comb_test)


pred_comb = predict(model_comb, comb_data)

# Need to make sure to generate the 
