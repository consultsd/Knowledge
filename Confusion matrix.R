rm(list=ls())

setwd("C:/Users/Sharath P Dandamudi/Desktop/creditscoring")

titanic<- read.csv("train.csv",stringsAsFactors=FALSE)

# Set random seed. Don't remove this line
set.seed(1)

# Have a look at the structure of titanic
str(titanic)

library(rpart)

# A decision tree classification model is built on the data
tree <- rpart(Survived ~ Age + Pclass, data = titanic, method = "class")

# Use the predict() method to make predictions, assign to pred
pred <- predict(tree, titanic, type = "class")

# Use the table() method to make the confusion matrix

table(titanic$Survived, pred)

conf <- table(titanic$Survived, pred)

# Assign TP, FN, FP and TN using conf
TP <- conf[1, 1]
FN <- conf[1, 2]
FP <- conf[2, 1]
TN <- conf[2, 2]

# Calculate the accuracy: acc
acc <- (TP + TN) / (TP + FN + FP + TN)
acc

# Calculate and print out the precision: prec
prec <- TP / (TP + FP)
prec

# Calculate and print out the recall: rec
rec <- TP / (TP + FN)
rec