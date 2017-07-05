#Removing the objects saved in the console
rm(list = ls())

#Setting the work directory
setwd("C:/Users/Sharath P Dandamudi/Desktop")

library(readxl)

#Importing the data file
healthcare <- read_excel("Healthcare.xlsx",sheet="Sheet1")

str(healthcare)

healthcare$complexity <- as.factor(healthcare$complexity)

class(healthcare$complexity)

str(healthcare)

#MVP_escalations, Major_deviations, Minor_deviations have missing values. 
#Imputation with median

# Make copy of data
healthcare_replace <- healthcare

# Get indices of missing variables
na_index_MVP_escalations <- which(is.na(healthcare$MVP_escalations))
na_index_Major_deviations <- which(is.na(healthcare$Major_deviations))
na_index_Minor_deviations <- which(is.na(healthcare$Minor_deviations))

# Compute the median
median_MVP_escalations <- median(healthcare$MVP_escalations, na.rm = TRUE)
median_Major_deviations <- median(healthcare$Major_deviations, na.rm = TRUE)
median_Minor_deviations <- median(healthcare$Minor_deviations, na.rm = TRUE)

# Replace missing values with median
healthcare_replace$MVP_escalations[na_index_MVP_escalations] <- median_MVP_escalations 
healthcare_replace$Major_deviations[na_index_Major_deviations] <- median_Major_deviations
healthcare_replace$Minor_deviations[na_index_Minor_deviations] <- median_Minor_deviations 

#extract only the independent variables
ivs <- healthcare_replace[,9:14]

dat <- ivs

#compute correlation
cormat <- round(cor(dat),2)
head(cormat)


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
install.packages("ggplot2")
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Print the heatmap
print(ggheatmap)

#Add correlation coefficients on the heatmap
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


# Model for Total_amendments

amend_dat <- healthcare[c("Total_amendments",
                          "Number_of_sites","Number_of_secondary_endpoints",
                            "Number_of_exploratory_endpoints","FPI_LPI_duration_days")]

#compute correlation
cormat <- round(cor(amend_dat),2)
head(cormat)

linear_amend <- lm(Total_amendments ~ complexity + Number_of_sites + Number_of_secondary_endpoints + 
            Number_of_exploratory_endpoints + FPI_LPI_duration_days, data = healthcare)
summary(linear_amend)

#Varibales dropped -
#Number_of_patients
#Number_of_pri_endpoints

library(sjPlot)

sjt.lm(linear_amend)


# Model for MVP_escalations

MVP_dat <- healthcare[c("MVP_escalations",
                          "Number_of_sites","Number_of_secondary_endpoints",
                          "Number_of_exploratory_endpoints","FPI_LPI_duration_days")]

MVP_dat <- MVP_dat[!is.na(MVP_dat$MVP_escalations),]

#compute correlation
cormat <- round(cor(MVP_dat),2)
head(cormat)

linear_MVP <- lm(MVP_escalations ~ complexity + Number_of_sites + Number_of_secondary_endpoints + 
                     Number_of_exploratory_endpoints + FPI_LPI_duration_days, data = healthcare)
summary(linear_MVP)

sjt.lm(linear_MVP)


# Model for Major_deviations

MAJ_DEV_dat <- healthcare[c("Major_deviations",
                        "Number_of_sites","Number_of_secondary_endpoints",
                        "Number_of_exploratory_endpoints","FPI_LPI_duration_days")]

MAJ_DEV_dat <- MAJ_DEV_dat[!is.na(MAJ_DEV_dat$Major_deviations),]

#compute correlation
cormat <- round(cor(MAJ_DEV_dat),2)
head(cormat)

linear_MAJ_DEV <- lm(Major_deviations ~ complexity + Number_of_sites + Number_of_secondary_endpoints + 
                   Number_of_exploratory_endpoints + FPI_LPI_duration_days, data = healthcare)
summary(linear_MAJ_DEV)

sjt.lm(linear_MAJ_DEV)


# Model for Total Cost

TOTAL_COST_dat <- healthcare[c("Total_cost",
                            "Number_of_sites","Number_of_secondary_endpoints",
                            "Number_of_exploratory_endpoints","FPI_LPI_duration_days")]

TOTAL_COST_dat <- TOTAL_COST_dat[!is.na(TOTAL_COST_dat$Total_cost),]

#compute correlation
cormat <- round(cor(TOTAL_COST_dat),2)
head(cormat)



linear_TOTAL_COST <- lm(Total_cost ~ complexity + Number_of_sites + Number_of_secondary_endpoints + 
                       Number_of_exploratory_endpoints + FPI_LPI_duration_days, data = healthcare)
summary(linear_TOTAL_COST)

sjt.lm(linear_TOTAL_COST)


# Model for Minor deviations

MINOR_DEV_dat <- healthcare[c("Minor_deviations",
                               "Number_of_sites","Number_of_secondary_endpoints",
                               "Number_of_exploratory_endpoints","FPI_LPI_duration_days")]

MINOR_DEV_dat <- MINOR_DEV_dat[!is.na(MINOR_DEV_dat$Minor_deviations),]

#compute correlation
cormat <- round(cor(MINOR_DEV_dat),2)
head(cormat)

library(Hmisc)

cor2 <- rcorr(as.matrix(MINOR_DEV_dat), type="pearson")


linear_MINOR_DEV <- lm(Minor_deviations ~ complexity + Number_of_sites + Number_of_secondary_endpoints + 
                          Number_of_exploratory_endpoints + FPI_LPI_duration_days, data = healthcare)
summary(linear_MINOR_DEV)

sjt.lm(linear_MINOR_DEV)




#linear_MINOR_DEV$residuals
#linear_MINOR_DEV$fitted.values
#plot(linear_MINOR_DEV$residuals,linear_MINOR_DEV$fitted.values)

#library(psych)
#out <- corr.test(cormat)
#print(out, short = FALSE)

# install.packages("corrr")
# library(corrr)
# MINOR_DEV_dat %>% correlate() %>% focus(Minor_deviations)



						
