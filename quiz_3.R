# Setup -------------------------------------------------------------------

library(AppliedPredictiveModeling)
library(caret)
library(rattle)

# Question 1 --------------------------------------------------------------

rm(list = ls())
data(segmentationOriginal)
training = subset(segmentationOriginal, Case == "Train")
testing = subset(segmentationOriginal, Case == "Test")
set.seed(125)
model = train(Class ~ ., method = 'rpart', data = training)
print(model)
fancyRpartPlot(model$finalModel)

sampleData = training[1,]
sampleData[1,names(sampleData)] = rep(NA, length(names(sampleData)))
sampleData = rbind(sampleData, sampleData)
sampleData = rbind(sampleData, sampleData)
sampleData[1, c('TotalIntenCh1', 'FiberWidthCh1', 'PerimStatusCh1')] = c(23000, 10, 2)
sampleData[2, c('TotalIntenCh1', 'FiberWidthCh1', 'VarIntenCh4')] = c(50000, 10, 100)
sampleData[3, c('TotalIntenCh1', 'FiberWidthCh1', 'VarIntenCh4')] = c(57000, 8, 100)
sampleData[4, c('FiberWidthCh1', 'VarIntenCh4', 'PerimStatusCh1')] = c(8, 100, 2)
predict(model, sampleData)
# This did not work for some reason....



# Question 2 --------------------------------------------------------------

# For k-Fold validation:
#   Larger K = less bias and more variance
#   Smaller K = more bias and less variance
# Leave-One-Out cross validation is a special case of k-Fold where k = N

# Question 3 --------------------------------------------------------------

rm(list = ls())
library(pgmm)
data(olive)
olive = olive[,-1]

model = train(Area ~ ., method = 'rpart', data = olive)
fancyRpartPlot(model$finalModel)

newdata = as.data.frame(t(colMeans(olive)))
predict(model, newdata)

# The prediction is strange since the result should be a catagorical result and not a continious result


# Question 4 --------------------------------------------------------------

rm(list = ls())
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
model = train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method = 'glm', family = 'binomial', data = trainSA)

trainPred = predict(model, trainSA)
testPred = predict(model, testSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

trainMissClass = missClass(trainSA$chd, trainPred)
testMissClass = missClass(testSA$chd, testPred)

# TrainMissClass = 0.2727
# TestMissClass = 0.3117



# Question 5 --------------------------------------------------------------

rm(list = ls())
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)

set.seed(33833)
model = train(y ~ ., method = 'rf', data = vowel.train, prox = TRUE, importance = TRUE)  # This takes some time...
print(model)

vi = varImp(model$finalModel)
vi


