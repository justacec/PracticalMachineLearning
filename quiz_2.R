
# Setup -------------------------------------------------------------------

library(AppliedPredictiveModeling)
library(caret)


# Question 1 --------------------------------------------------------------

rm(list = ls())
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


# Question 2 --------------------------------------------------------------

rm(list = ls())
source('multiplot.R')
data(concrete)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

vars = names(training[,!(names(training) %in% c('CompressiveStrength'))])
plots = list()
tt = training
tt$index = 1:nrow(tt)
for(i in 1:length(vars)) {
  vname = sprintf('%s_Factor', vars[i])
  tt[,vname] = cut2(tt[,vars[i]], g = 4)
  plots[[i]] = ggplot(tt) + theme_bw() + geom_point(aes_string(x = 'index', y = 'CompressiveStrength', color = vname))
}
multiplot(plotlist = plots, cols = 3)
# There are no variables that are correlated with the compressive strength


# Question 3 --------------------------------------------------------------

rm(list = ls())
data(concrete)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
ggplot(training) + theme_bw() + geom_histogram(aes(x = Superplasticizer))
sum(training$Superplasticizer == 0) / nrow(training)
# There are zero values here and therefore the log transfor would yeild -Inf values

# Question 4 --------------------------------------------------------------

rm(list = ls())
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Extract all the IL variables
subtrain = training[,substr(names(training), 1, 3) == 'IL_']
r = preProcess(subtrain, method = "pca", thresh = 0.8)
rtrain = predict(r, subtrain)
rtrain$diagnosis = training$diagnosis
modelfit = train(rtrain$diagnosis ~ ., method = 'glm', data = rtrain)
summary(modelfit)
# The number of PCA components is 7 based on the output


# Question 5 --------------------------------------------------------------

rm(list = ls())
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Extract all the IL variables
subtrain = training[,substr(names(training), 1, 3) == 'IL_']
subtest = testing[,substr(names(testing), 1, 3) == 'IL_']


r = preProcess(subtrain, method = "pca", thresh = 0.8)
rtrain = predict(r, subtrain)
rtrain$diagnosis = training$diagnosis
modelfit_PCA = train(rtrain$diagnosis ~ ., method = 'glm', data = rtrain)
summary(modelfit_PCA)

strain = subtrain
strain$diagnosis = training$diagnosis
modelfit_noPCA = train(strain$diagnosis ~ ., method = 'glm', data = strain)
summary(modelfit_noPCA)

test_pca = predict(r, subtest)
test_nopca = subtest

test_pca$diagnosis = testing$diagnosis
test_nopca$diagnosis = testing$diagnosis

test_pca_pred = predict(modelfit_PCA, test_pca)
test_nopca_pred = predict(modelfit_noPCA, test_nopca)

confusionMatrix(test_pca_pred, testing$diagnosis)
confusionMatrix(test_nopca_pred, testing$diagnosis)

# Non-PCA Accuracy: 0.65
# PCA Accuracy: 0.72
