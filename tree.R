#Removing the objects saved in the console
rm(list = ls())

#Setting the work directory
setwd("C:/Users/Sharath P Dandamudi/Desktop/Titanic")

titanic <- read.csv("train.csv",stringsAsFactors=FALSE)

# Stratified sampling - split into TRAIN and TEST
library(caTools)
split = sample.split(titanic$Survived, SplitRatio = 0.7)
train = subset(titanic, split == TRUE)
test = subset(titanic, split == FALSE)

# Set random seed. Don't remove this line.
set.seed(1)

install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)

# Load the rpart, rattle, rpart.plot and RColorBrewer package
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Fill in the ___, build a tree model: tree
tree <- rpart(Survived ~ Age + Pclass + Fare + Embarked, train, method = "class")

# Draw the decision tree
fancyRpartPlot(tree)


# Specify the cp argument to be 0.01. This is a complexity parameter.
# It basically tells the algorithm to remove node splits that do not sufficiently decrease the impurity.
# Prune the tree: pruned
pruned <- prune(tree, cp = 0.01)

# Draw pruned
fancyRpartPlot(pruned)

# Predict the values of the test set: pred
pred <- predict(tree, test, type = "class")

# Construct the confusion matrix: conf
conf <- table(test$Survived, pred)

# Print out the accuracy
print(sum(diag(conf)) / sum(conf))




