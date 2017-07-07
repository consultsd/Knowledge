# Vectors (one dimensional array): can hold numeric, character or logical values. 
# The elements in a vector all have the same data type.

# Matrices (two dimensional array): can hold numeric, character or logical values. 
# The elements in a matrix all have the same data type.

# Data frames (two-dimensional objects): can hold numeric, character or logical values. 
# Within a column all elements have the same data type, but different columns can be of 
# different data type

# List is some kind super data type: you can store practically any piece of information in it


#Loading the package to use in-built datasets
library(datasets)

#Help on datasets package
help("datasets")
?datasets

#List of datasets availble in R
data()

#To view dataset in R console
cars

#To view dataset in R Viewer window
View(cars)

#To view contents of dataset
str(cars)

#To view top 10 rows in dataframe
head(cars,10)

#To view last 10 rows in dataframe
tail(cars,10)

#To check data-set type
class(cars)

#To select element in a dataframe using row and column indicator
cars[1,2]
cars[2,]
cars[3:4,"speed"]

#To subset data - using SUBSET function
new_cars <- subset(cars,subset= speed > 5 | dist < 10)
View(new_cars)

new_cars2 <- subset(cars,subset= speed > 5 & dist < 10)
View(new_cars2)

#To sort  dataframe
positions <- order(cars$speed)
cars[positions,]
cars


#To add new variable to dataframe
cars$new_var <- sample(1:100, nrow(cars), replace=F)
cars$new_cont_var <- 25
cars$new_char_var <- "SD"

#To create a new dataframe 
s1=c(1,2,3,4,NA)
s2=c(6,7,8,9,10)

new_df <- data.frame(s1,s2)

join_var_df <- data.frame(new_join_var=sample(101:200, nrow(cars), replace=F))

#To remove an object/data frame
rm(join_var)

#To combine dataframes - horizontally
combine_df <- cbind(cars,join_var_df)

#To combine dataframes - vertically 
concatenate_df <- rbind(combine_df,c(1,2,3,4,"DSP",100))

#To induce missing values in dataframe
concatenate_df_2 <- rbind(concatenate_df,c(NA,7,8,9,NA,120))

#To append series of observations to exisiting dataframe - here Granny and Geraldine are existing column names 
#in the new.baskets dataframe- SAMPLE CODE
new.baskets <- data.frame(Granny = c(3, 8), Geraldine = c(9, 4))

typeof(concatenate_df$speed)

table(concatenate_df$new_char_var)

#To get column names in a dataset
colnames(concatenate_df)

#To convert character to factor variable
class(concatenate_df$new_var) #[1] "character"
concatenate_df$new_var=as.factor(concatenate_df$new_var)
class(concatenate_df$new_var) #[1] "factor"


#To convert character to factor variable
class(concatenate_df$new_var) #[1] "character"
concatenate_df$new_var=as.factor(concatenate_df$new_var)
class(concatenate_df$new_var) #[1] "factor"


prop.table(concatenate_df$new_var)

#To assign names to a vector

# -- Poker winnings from Monday to Friday
poker_vector <- c(140, -50, 20, -120, 240)

# ---The variable days_vector
days_vector <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

# Assign the names of the day to roulette_vector and poker_vector
names(poker_vector) <-  c(days_vector)

#To print a vector
print(poker_vector)

#To add elements of a vector
total_poker <- sum(poker_vector)
print(total_poker)

# To calculate the average of the elements in a vector
mean(poker_vector)
     
# To compare 2 vectors or vector and a constant 
total_poker > 150

# To access element in a vector and store it in another vector
poker_wednesday <- poker_vector[3]
print(poker_wednesday)

# To define a new variable based on a selection
poker_midweek <- poker_vector[c(2,3,4)]
print(poker_midweek)

poker_midweek <- poker_vector[2:4]
print(poker_midweek)

# To select by comparison
selection_vector <- poker_vector > 0
print(selection_vector)

# To construct a matrix with 3 rows that contain the numbers 1 up to 9
matrix(1:9, byrow=TRUE,nrow=3)


# To construct a matrix with 3 vectors - METHOD 1
# ---Box office Star Wars (in millions!)
new_hope <- c(460.998, 314.4)
empire_strikes <- c(290.475, 247.900)
return_jedi <- c(309.306, 165.8)

# ---Create box_office
box_office <- c(new_hope,empire_strikes,return_jedi)
box_office

# ---Construct star_wars_matrix
star_wars_matrix <- matrix(box_office,nrow=3,byrow=TRUE)
star_wars_matrix

# Vectors region and titles, used for naming
region <- c("US", "non-US")
titles <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi")

# Name the columns with region
colnames(star_wars_matrix) <- region

# Name the rows with titles
rownames(star_wars_matrix) <- titles

star_wars_matrix

# To construct a matrix with 3 vectors - METHOD 2 (using dimnames)

# Construct star_wars_matrix
box_office <- c(460.998, 314.4, 290.475, 247.900, 309.306, 165.8)
star_wars_matrix <- matrix(box_office, nrow = 3, byrow = TRUE,
                           dimnames = list(c("A New Hope", "The Empire Strikes Back", "Return of the Jedi"), 
                                           c("US", "non-US")))

# To calculate worldwide box office figures
worldwide_vector <- rowSums(star_wars_matrix)
worldwide_vector

# To create factors (ORDINAL and NOMINAL)

#---Animals
animals_vector <- c("Elephant", "Giraffe", "Donkey", "Horse")
factor_animals_vector <- factor(animals_vector)
factor_animals_vector

#---Temperature
temperature_vector <- c("High", "Low", "High","Low", "Medium")
factor_temperature_vector <- factor(temperature_vector, order = TRUE, levels = c("Low", "Medium", "High"))
factor_temperature_vector

levels(animals_vector)

# To chang levels of factor variable

#---Code to build factor_survey_vector
survey_vector <- c("M", "F", "F", "M", "M")
factor_survey_vector <- factor(survey_vector)

#---Specify the levels of factor_survey_vector
levels(factor_survey_vector) <- c("Male","Female","Female","Male","Male")
factor_survey_vector

# To generate summary for survey_vector - overview/contents of variable
summary(survey_vector)

#To sort a vector
a <- c(100, 10, 1000)
order(a) #Output - 2 1 3
a[order(a)] #Output - 10  100 1000


#To create a list

#---Vector with numerics from 1 up to 10
my_vector <- 1:10 

#---Matrix with numerics from 1 up to 9
my_matrix <- matrix(1:9, ncol = 3)

#---First 10 elements of the built-in data frame mtcars
my_df <- mtcars[1:10,]

#---Construct list with these different elements:
my_list <- list(my_vector,my_matrix,my_df)

my_list <- list(vec=my_vector, mat=my_matrix, df=my_df)
my_list

#TIP - for selecting elements from a list
#You can also refer to the names of the components, with [[ ]] or with the $ sign.
#To select elements from vectors, you use single square brackets: [ ]