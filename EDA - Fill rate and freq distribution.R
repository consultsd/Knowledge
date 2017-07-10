
#run eda function and store in the environment.

eda<-function(X){
df<-X


factor_cols<-df[sapply(df,is.factor)]
factor_cols<-cbind(factor_cols,df[sapply(df,is.character)])
if(ncol(factor_cols>0)){
factor_cols_eda<-data.frame(Number_of_Classes =apply(factor_cols, 2, function(x) length(unique(x))))
factor_cols_eda$Missing_NA<- sapply(factor_cols, function(x) sum(is.na(x)))
factor_cols_eda$Missing_NA_Char<- sapply(factor_cols, function(x) sum(x == "NA"))
factor_cols_eda$Missing_Percentage<-paste0(as.character(round((factor_cols_eda$Missing_NA+factor_cols_eda$Missing_NA_Char)/nrow(factor_cols),2)*100),"%")
factor_cols_eda$Type<-"factor/character"
factor_cols_eda<-factor_cols_eda[,c(5,1,4)]
factor_cols_eda$Variable_Name<-names(factor_cols)
factor_cols_eda<-factor_cols_eda[,c(4,1,2,3)]  
}
else factor_cols_eda <-NULL


numeric_cols<-df[sapply(df,is.numeric)]
if(ncol(numeric_cols>0)){
numeric_cols_eda<-data.frame(Missing_NA = sapply(numeric_cols, function(x) sum(is.na(x))))
numeric_cols_eda$Missing_Percentage<-paste0(as.character(round((numeric_cols_eda$Missing_NA)/nrow(numeric_cols),4)*100),"%")
numeric_cols_eda$Number_of_Classes<- "NA"
numeric_cols_eda$Type<-"numeric"
numeric_cols_eda<-numeric_cols_eda[,c(4,3,2)]
numeric_cols_eda$Variable_Name<-names(numeric_cols)
numeric_cols_eda<-numeric_cols_eda[,c(4,1,2,3)]  
}
else numeric_cols_eda<-NULL  

df_eda<-rbind(factor_cols_eda,numeric_cols_eda)
write.csv(df_eda,"df_eda.csv",row.names = FALSE)
}



#call eda on a dataframe using eda("dataframe_name")

eda(iris)




