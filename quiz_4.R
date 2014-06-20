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

