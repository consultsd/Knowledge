rm(list=ls())

setwd("C:/Users/Sharath P Dandamudi/Desktop/Titanic")

# ************************Importing and exporting excel files

library(readxl)

df1 <- read_excel("train.xlsx", sheet = "Sheet1")

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_131')

library(rJava)

library(xlsx)

write.xlsx(df1, file="train_test.xlsx", sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE)

# ************************Importing and exporting csv files
library(readr)

df2 <- read_csv("train.csv")

write.csv(df1, file = "DF1.csv")



