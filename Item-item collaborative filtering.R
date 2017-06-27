rm(list=ls())

#Replace the path with the folder where the input data is stored
setwd ("C:/Users/Sharath P Dandamudi/Desktop")

#Replace the csv file name accordingly
#Note the folowing:
#The csv file should have only 2 columns
#The first column in the csv should be 'Sold_to_group' (no space in between) and the second column should be 'Material'
#Otherwise error will be thrown up while running the code

df = data.frame(read_csv("data_file.csv"))

library(reshape)

#Transforming the data frame to a frequency matrix
freq_matrix = dcast(df, Customer_Group ~ Product_Class, fun.aggregate = length)

#Dropping the Customer_Group variable from the matrix as it is not required to build item based collaborative filtering model
reco <- (freq_matrix[,!(names(freq_matrix) %in% c("Customer_Group"))])

#Transforming the frequency matrix to binary matrix
reco_bin = reco

for (i in 1:nrow(reco))
{
  for (j in 1:ncol(reco))
  {
    if(reco[i,j] > 0) reco_bin[i,j] = 1 else reco_bin[i,j] = 0
  }
}

# Creating a helper function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}


# Creating a placeholder dataframe listing item vs. item
reco_bin.similarity  <- matrix(NA, nrow=ncol(reco_bin),ncol=ncol(reco_bin),dimnames=list(colnames(reco_bin),colnames(reco_bin)))


# Filling in those empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(reco_bin)) {
  # Loop through the columns for each column
  for(j in 1:ncol(reco_bin)) {
    # Fill in placeholder with cosine similarities
    reco_bin.similarity[i,j] <- getCosine(as.matrix(reco_bin[,i]),as.matrix(reco_bin[,j]))
  }
}

# Back to dataframe
reco_bin.similarity <- as.data.frame(reco_bin.similarity)

write.csv(file="reco_bin.similarity.csv",x=reco_bin.similarity)


# Get the top 10 neighbours for each
reco_bin.neighbours <- matrix(NA, nrow=ncol(reco_bin.similarity),ncol=9,dimnames=list(colnames(reco_bin.similarity)))

for(i in 1:ncol(reco_bin)) 
{
  reco_bin.neighbours[i,] <- (t(head(n=9,rownames(reco_bin.similarity[order(reco_bin.similarity[,i],decreasing=TRUE),][i]))))
}

write.csv(file="reco_neighbours.csv",x=reco_bin.neighbours[,-1])