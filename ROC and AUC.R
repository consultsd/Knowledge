rm(list=ls())

setwd("C:/Users/Sharath P Dandamudi/Desktop/Titanic")

titanic <- read.csv("train.csv",stringsAsFactors=FALSE)

# Stratified sampling - split into TRAIN and TEST
library(caTools)
split = sample.split(titanic$Survived, SplitRatio = 0.7)
train = subset(titanic, split == TRUE)
test = subset(titanic, split == FALSE)

prop.table(table(train$Survived))
prop.table(table(test$Survived))


# Set random seed. Don't remove this line
set.seed(1)

# Build a tree on the training set: tree
tree <- rpart(Survived ~ Age + Pclass, data = titanic, method = "class")

# Predict probability values using the model: all_probs
all_probs <- predict(tree, test, type = "prob")

# Print out all_probs
all_probs

# Select second column of all_probs: probs
probs <- all_probs[,2]

# Load the ROCR library
library(ROCR)

# Make a prediction object: pred
pred <- prediction(probs, test$Survived)

# Make a performance object: perf
perf <- performance(pred, "tpr", "fpr")

# Plot this curve
plot(perf)

# Make a performance object for AUC: perf
perf_AUC <- performance(pred, "auc")

# Print out the AUC
print(perf_AUC@y.values[[1]])




