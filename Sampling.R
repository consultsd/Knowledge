#Links to refer to 
#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems

# Below are the methods used to treat imbalanced datasets:
# Undersampling
# Oversampling
# Synthetic Data Generation
# Cost Sensitive Learning

# ROSE (Random Over Sampling Examples) package helps us to generate artificial data based on sampling 
# methods and smoothed bootstrap approach.

#Set path
setwd ("C:/Users/Sharath P Dandamudi/Desktop")

#Install ROSE package
install.packages("ROSE")
library(ROSE)

# ROSE comes with an inbuilt imbalanced data set named as hacide

#Loading the dataset into R environment
data(hacide)

str(hacide.train)

#Check frequency of response variable
table(hacide.train$cls)

#Check classes distribution of response variable
prop.table(table(hacide.train$cls))

#Invoking the library for decision trees
library(rpart)

#Building decision tree on the train data
treeimb <- rpart(cls ~ ., data = hacide.train)

#Predicting test data using the DT model
pred.treeimb <- predict(treeimb, newdata = hacide.test)

#ROSE package has a function names accuracy.meas, it computes important metrics such as precision, recall & F measure
accuracy.meas(hacide.test$cls, pred.treeimb[,2])

#Computing Area under curve
roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)



#Over sampling
data_balanced_over <- ovun.sample(cls ~ ., data = hacide.train, method = "over",N = 1960)$data
# over instructs the algorithm to perform over sampling. N refers to number of observations 
# in the resulting balanced set.

table(data_balanced_over$cls)


#Under sampling
data_balanced_under <- ovun.sample(cls ~ ., data = hacide.train, method = "under", N = 40, seed = 1)$data
# Under instructs the algorithm to perform over sampling. N refers to number of observations 
# in the resulting balanced set.

table(data_balanced_under$cls)


#Both Over and Under sampling
data_balanced_both <- ovun.sample(cls ~ ., data = hacide.train, method = "both", p=0.5, N=1000, seed = 1)$data

table(data_balanced_both$cls)


#Synthetic data sampling using ROSE
data.rose <- ROSE(cls ~ ., data = hacide.train, seed = 1)$data

table(data.rose$cls)

#Build decision tree models
tree.rose <- rpart(cls ~ ., data = data.rose)
tree.over <- rpart(cls ~ ., data = data_balanced_over)
tree.under <- rpart(cls ~ ., data = data_balanced_under)
tree.both <- rpart(cls ~ ., data = data_balanced_both)

#Make predictions on unseen data
pred.tree.rose <- predict(tree.rose, newdata = hacide.test)
pred.tree.over <- predict(tree.over, newdata = hacide.test)
pred.tree.under <- predict(tree.under, newdata = hacide.test)
pred.tree.both <- predict(tree.both, newdata = hacide.test)

#AUC ROSE
roc.curve(hacide.test$cls, pred.tree.rose[,2])

#AUC Oversampling
roc.curve(hacide.test$cls, pred.tree.over[,2])

#AUC Undersampling
roc.curve(hacide.test$cls, pred.tree.under[,2])

#AUC Both
roc.curve(hacide.test$cls, pred.tree.both[,2])

#Cross validation
ROSE.holdout <- ROSE.eval(cls ~ ., data = hacide.train, learner = rpart, method.assess = "holdout", extr.pred = function(obj)obj[,2], seed = 1)

ROSE.holdout


# Random sampling without replacement - NUMBER
sample_wo_replace <- hacide.train[sample(1:nrow(hacide.train), 50,replace=FALSE),]

# Random sampling without replacement - PROPORTION
library(dplyr)
sample_wo_replace2 <- sample_frac(hacide.train, size = 0.3, replace = FALSE)

# Stratified sampling - split into TRAIN and TEST
library(caTools)
split = sample.split(hacide.train$cls, SplitRatio = 0.7)
training_set = subset(hacide.train, split == TRUE)
test_set = subset(hacide.train, split == FALSE)
table(training_set$cls)
table(test_set$cls)
