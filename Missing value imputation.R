#Links to refer to 
#https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/

# List of R Packages for missing value imputation
# MICE
# Amelia
# missForest
# Hmisc
# mi

setwd("C:/Users/Sharath P Dandamudi/Desktop")

#*************************************MICE*************************************

#Load Iris data
data <- iris

#Summary of Iris dataset
summary(iris)

#Inserting missing values in Iris dataset at random using prodNA function in missForest package
library(missForest)
iris.mis <- prodNA(iris, noNA = 0.1)

#Check missing values introduced in the data
summary(iris.mis)

#Removing categorical variables
#To treat categorical variable, simply encode the levels 
iris.mis <- subset(iris.mis, select = -c(Species))
summary(iris.mis)

#md.pattern() returns a tabular form of missing value present in each variable in a data set.
library(mice)
df_pattern <- md.pattern(iris.mis)


#Creating a visual which represents missing values
library(VIM)
mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iris.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


imputed_Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

# Here is an explanation of the parameters used:
# m  - Refers to 5 imputed data sets
# maxit - Refers to no. of iterations taken to impute missing values
# method - Refers to method used in imputation. we used predictive mean matching.

#Check imputed values
imputed_Data$imp$Sepal.Width

# Since there are 5 imputed data sets, you can select any using complete() function
#Get complete data ( 2nd out of 5)
completeData <- complete(imputed_Data,2)


#Build predictive modelbuild models on all 5 datasets, you can do it in one go using with() command
fit <- with(data = imputed_Data, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width)) 

#Combine results of all 5 models
#Pool function averages the estimates of the complete data model
combine <- pool(fit)
summary(combine)

#*************************************AMELIA*************************************

#Install package and load library
library(Amelia)

#load data
data(iris)

#seed 10% missing values
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)

#specify columns and run amelia
#noms - categorical variable
amelia_fit <- amelia(iris.mis, m=5, parallel = "multicore", noms = "Species")

#access imputed outputs
amelia_fit$imputations[[1]]
amelia_fit$imputations[[2]]
amelia_fit$imputations[[3]]
amelia_fit$imputations[[4]]
amelia_fit$imputations[[5]]

#To check a particular column in a data set
amelia_fit$imputations[[5]]$Sepal.Length

#export the outputs to csv files
write.amelia(amelia_fit, file.stem = "imputed_data_set")

#*************************************MISSFOREST*************************************

library(missForest)

#load data
data(iris)

#seed missing values ( 10% )
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)

#impute missing values, using all parameters as default values
iris.imp <- missForest(iris.mis)

#check imputed values
iris.imp$ximp

#check imputation error
iris.imp$OOBerror

# NRMSE is normalized mean squared error. It is used to represent error derived from imputing 
# continuous values. PFC (proportion of falsely classified) is used to represent error derived 
# from imputing categorical values

#comparing actual data accuracy
iris.err <- mixError(iris.imp$ximp, iris.mis, iris)
iris.err

# This suggests that categorical variables are imputed with 6% error and continuous variables 
# are imputed with 15% error. This can be improved by tuning the values of mtry and ntree parameter. 
# mtry refers to the number of variables being randomly sampled at each split. ntree refers to number
# of trees to grow in the forest

#*************************************HMISC*************************************

library(Hmisc)

#load data
data(iris)

#seed missing values ( 10% )
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)

# impute with mean value
iris.mis$imputed_Length <- with(iris.mis, impute(Sepal.Length, mean))

# impute with random value
iris.mis$imputed_Length2 <- with(iris.mis, impute(Sepal.Length, 'random'))

#similarly you can use min, max, median to impute missing value

#using argImpute
impute_arg <- aregImpute(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width +
                             Species, data = iris.mis, n.impute = 5)

impute_arg
# The output shows R² values for predicted missing values. Higher the value, better are the values predicted.

#Converting the list to a dataframe
imputed_arg_df <- as.data.frame(impute.transcan(impute_arg, imputation=1, data=iris.mis, 
                                         list.out=TRUE, pr=FALSE, check=FALSE))

#*************************************MI*************************************
?mi2stata

library(mi)

#load data
data(iris)

#seed missing values ( 10% )
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)
                            
#imputing missing value with mi
mi_data <- as.data.frame(mi(iris.mis, seed = 335))
mi2stata(mi_data,m=2, file="mi_data.csv")

#m=2 1- for original datframe and 2-imputed dataset

summary(mi_data)

#Hmisc should be the first choice of missing value imputation followed by missForest and MICE.
