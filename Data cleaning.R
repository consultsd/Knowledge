#Removing the objects saved in the console
rm(list = ls())

#Setting the work directory
setwd("C:/Users/Sharath P Dandamudi/Desktop/Titanic")

#Importing the train and test files
titanic_train <- read.csv("train.csv",stringsAsFactors=FALSE)
titanic_test <- read.csv("test.csv",stringsAsFactors=FALSE)

#Computing the response rate in the train dataset
resp_rate <- as.data.frame(prop.table(table(titanic_train$Survived)))

colnames(resp_rate) <- c("Survived","Frequency")
resp_rate$Frequency <- paste0(round(resp_rate$Frequency*100,2),"%")
resp_rate


#library(gmodels)
# Call CrossTable() on Survived
#CrossTable(titanic_train$Survived)

# Call CrossTable() on Survived
#CrossTable(titanic_train$Sex,titanic_train$Survived,prop.r=TRUE,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE)


#Dropping the dependent variable from the training dataset
dv <- names(titanic_train) %in% c("Survived") 
titanic_train_iv <- titanic_train[!dv]

#Another way of dropping the dependent variable from the training dataset
#titanic_train_iv_2 <- titanic_train[c(-2)]

#Creating a new variable to indicate if the record is from TRAIN or TEST
titanic_train_iv$sample <- "TRAIN"
titanic_test$sample <- "TEST"

titanic_wo_dv <-rbind(titanic_train_iv,titanic_test)

str(titanic_wo_dv)

# Create histogram of Age: hist_1
hist_1 <- hist(titanic_wo_dv$Age)

# Print locations of the breaks in hist_1
hist_1$breaks

# Change number of breaks and add labels: hist_2
hist_2 <- hist(titanic_wo_dv$Age, breaks = 10, xlab = "Age", 
               main = "Histogram of Age")


#hist(titanic_wo_dv$Fare)

# Plot the Fare variable
plot(titanic_wo_dv$Fare,ylab="Fare")

# Save the outlier's index to index_highfare
index_highfare <- which(titanic_wo_dv$Fare > 300)

# Create data set new_data with outlier deleted
new_titanic_wo_dv <- titanic_wo_dv[-index_highfare, ]

# Make bivariate scatterplot of age and Fare
#plot(titanic_wo_dv$Age, titanic_wo_dv$Fare, xlab = "Age", ylab = "Fare")

# Look at summary of Age variable
summary(titanic_wo_dv$Age)

# Get indices of missing Age: na_index
na_index <- which(is.na(titanic_wo_dv$Age))

# Remove observations with Age: titanic_wo_dv_delrow_na
titanic_wo_dv_delrow_na <- titanic_wo_dv[-c(na_index), ]

# Make copy of Titanic data
titanic_wo_dv_delcol_na <- titanic_wo_dv

# Delete Age column from titanic_wo_dv_delcol_na
loan_data_delcol_na$Age <- NULL

# Compute the median of Age
median_age <- median(titanic_wo_dv$Age, na.rm = TRUE)

# Make copy of loan_data
titanic_wo_dv_replace <- titanic_wo_dv

# Replace missing interest rates with median
titanic_wo_dv_replace$Age[na_index] <- median_age

# Check if the NAs are gone
summary(titanic_wo_dv_replace$Age)


#KEEPING MISSING DATA - MISSING as a separate category

# Make the necessary replacements in the coarse classification example below 
titanic_wo_dv$age_cat <- rep(NA, length(titanic_wo_dv$Age))

titanic_wo_dv$age_cat[which(titanic_wo_dv$Age <= 10)] <- "0-10"
titanic_wo_dv$age_cat[which(titanic_wo_dv$Age > 10 & titanic_wo_dv$Age <= 20)] <- "10-20"
titanic_wo_dv$age_cat[which(titanic_wo_dv$Age > 20 & titanic_wo_dv$Age <= 35)] <- "20-35"
titanic_wo_dv$age_cat[which(titanic_wo_dv$Age > 35 & titanic_wo_dv$Age <= 50)] <- "35-50"
titanic_wo_dv$age_cat[which(titanic_wo_dv$Age > 50)] <- "50+"
titanic_wo_dv$age_cat[which(is.na(titanic_wo_dv$Age))] <- "Missing"

titanic_wo_dv$age_cat <- as.factor(titanic_wo_dv$age_cat)

# Look at your new variable using plot()
plot(titanic_wo_dv$age_cat)

head(titanic_wo_dv$Name)

titanic_wo_dv$Name <- as.character(titanic_wo_dv$Name)

titanic_wo_dv$Name[1]

strsplit(titanic_wo_dv$Name[1], split='[,.]')[[1]][2]

titanic_wo_dv$Title <- sapply(titanic_wo_dv$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

titanic_wo_dv$Title <- sub(' ', '', titanic_wo_dv$Title)

table(titanic_wo_dv$Title)


titanic_wo_dv$Title[titanic_wo_dv$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

titanic_wo_dv$Title[titanic_wo_dv$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
titanic_wo_dv$Title[titanic_wo_dv$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'


titanic_wo_dv$Title <- factor(titanic_wo_dv$Title)


