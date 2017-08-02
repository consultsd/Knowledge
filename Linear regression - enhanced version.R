rm(list=ls())

# Links refered to
# http://r-statistics.co/Variable-Selection-and-Importance-With-R.html
# http://www.statmethods.net/stats/regression.html

setwd("C:/Users/Sharath P Dandamudi/Desktop")

# Importing and exporting excel files

library(readxl)

df1 <- read_excel("....xlsx", sheet = "Sheet1")

str(df1)

# Making a copy
df <- df1

# library(dplyr)

var_names <- names(df)
var_names

pattern <- "*_PTCONT|*_PTPERM"
perm_cont_var <- grep(pattern,var_names)

var_names[perm_cont_var]

length(perm_cont_var)

all_role_var <- var_names[7:136]

all_role_var

length(all_role_var)

# Deriving variable - var1
df$var1 <- rowSums(df[,perm_cont_var],na.rm=T)/
                                              rowSums(df[,all_role_var],na.rm=T)

class(df$var1)
View(df[280])


# Deriving variable - var2

male_pattern <- "*_MAL_PTCONT|*_MAL_PTPERM"
male_perm_cont_var <- grep(male_pattern,all_role_var)
length(male_perm_cont_var)
male_pt <- all_role_var[male_perm_cont_var]
male_pt

male_pattern_all <- "*_MAL_*"
male_all_role_var <- grep(male_pattern_all,all_role_var)
length(male_all_role_var)
male_all <- all_role_var[male_all_role_var]
male_all

female_pattern <- "*_FEM_PTCONT|*_FEM_PTPERM"
female_perm_cont_var <- grep(female_pattern,all_role_var)
length(female_perm_cont_var)
female_pt <- all_role_var[female_perm_cont_var]
female_pt


female_pattern_all <- "*_FEM_*"
female_all_role_var <- grep(female_pattern_all,all_role_var)
length(female_all_role_var)
female_all <- all_role_var[female_all_role_var]
female_all

df$var2 <- (rowSums(df[,male_pt],na.rm=T))/
                                  (rowSums(df[,male_all],na.rm=T))/(rowSums(df[,female_pt],na.rm=T))/
  (rowSums(df[,female_all],na.rm=T))

View(df[281])

# http://stat545.com/block022_regular-expression.html

# Deriving variable -

View(df[138])
grep("Q1_1_FPS_GE_sppt_Rec_Y", colnames(df))
138

grep("Q1_10_FPS_GE_sppt_overall_NNAP", colnames(df))
237

all_pol_var <- var_names[138:237]
all_pol_var


form_pol_gender <- "*_Y$"
all_pol_Y_ind <- grep(form_pol_gender,all_pol_var)
all_pol_Y <- all_pol_var[all_pol_Y_ind]
all_pol_Y
df$num_pol_for_gender <- (rowSums(df[,all_pol_Y],na.rm=T))
View(df[282])

# Deriving variable - var3

df$var3 <- df$Q2_1_GB_1_Direct_No._Female/
  (rowSums(df[,c("Q2_1_GB_1_Direct_No._Female","Q2_1_GB_1_Direct_No._Male")],na.rm=T))
View(df[283])

# Deriving variable - var4

df$var4 <- (rowSums(df[,c("Q7_PL_PRM_Male_Manager","Q7_PL_PRM_Male_Nonmanager")]
                                           ,na.rm=T))/(rowSums(df[,male_all],na.rm=T))
View(df[284])


# Deriving variable - var5
df$var5 <- (rowSums(df[,c("Q7_PL_SEC_Male_Manager","Q7_PL_SEC_Male_Nonmanager")]
                                            ,na.rm=T))/(rowSums(df[,male_all],na.rm=T))
View(df[285])


# Deriving variable - var6

grep("OMAN_MAL_PTPERM", colnames(df))
56

manager_role_var <- var_names[7:56]
manager_role_var
male_pattern_all <- "*_MAL_*"
male_manager_role_var <- grep(male_pattern_all,manager_role_var)
length(male_manager_role_var)

male_manager_all <- manager_role_var[male_manager_role_var]
male_manager_all
df$perc_male_manag_prim_car_leave <- (df$Q7_PL_PRM_Male_Manager)/(rowSums(df[,male_manager_all],na.rm=T))
View(df[286])


df$var6 <- (df$Q7_PL_SEC_Male_Manager)/(rowSums(df[,male_manager_all],na.rm=T))
View(df[287])
grep("Q11_2_nonleave_EmpSub_Childcare", colnames(df))
250
grep("Q11_2_parenting_workshops_fathers", colnames(df))
263

# Deriving variable - var7

df$var7 <- rowSums(df[,250:263],na.rm=T)
View(df[288])

# Importing additional columns -

df_add <- read_excel(".....xlsx", sheet = "Sheet1")
df_up <- cbind(df,df_add)
var_names <- names(df_up)
all_flex_var <- var_names[289:360]
all_flex_var

# Deriving variable - var8

df_up$carer_for <- 
  rowSums(df_up[,c("Q14_CARER_FEM_MGR_FOR",
                "Q14_CARER_FEM_NON_FOR",
                "Q14_CARER_MAL_MGR_FOR",
                "Q14_CARER_MAL_NON_FOR")],na.rm=T)
View(df_up[361])

df_up$carer_for_score[which(df_up$carer_for == 4)] <- 1
df_up$carer_for_score[which(df_up$carer_for < 4)] <- 0


df_up$compww_for <- 
  rowSums(df_up[,c("Q14_COMPWW_FEM_MGR_FOR",
                   "Q14_COMPWW_FEM_NON_FOR",
                   "Q14_COMPWW_MAL_MGR_FOR",
                   "Q14_COMPWW_MAL_NON_FOR")],na.rm=T)

df_up$compww_for_score[which(df_up$compww_for == 4)] <- 1
df_up$compww_for_score[which(df_up$compww_for < 4)] <- 0


df_up$flexhrs_for <- 
  rowSums(df_up[,c("Q14_FLEXHRS_FEM_MGR_FOR",
                   "Q14_FLEXHRS_FEM_NON_FOR",
                   "Q14_FLEXHRS_MAL_MGR_FOR",
                   "Q14_FLEXHRS_MAL_NON_FOR")],na.rm=T)

df_up$flexhrs_for_score[which(df_up$flexhrs_for == 4)] <- 1
df_up$flexhrs_for_score[which(df_up$flexhrs_for < 4)] <- 0


df_up$jobsh_for <- 
  rowSums(df_up[,c("Q14_JOBSH_FEM_MGR_FOR",
                   "Q14_JOBSH_FEM_NON_FOR",
                   "Q14_JOBSH_MAL_MGR_FOR",
                   "Q14_JOBSH_MAL_NON_FOR")],na.rm=T)

df_up$jobsh_for_score[which(df_up$jobsh_for == 4)] <- 1
df_up$jobsh_for_score[which(df_up$jobsh_for < 4)] <- 0


df_up$ptwork_for <- 
  rowSums(df_up[,c("Q14_PTWORK_FEM_MGR_FOR",
                   "Q14_PTWORK_FEM_NON_FOR",
                   "Q14_PTWORK_MAL_MGR_FOR",
                   "Q14_PTWORK_MAL_NON_FOR")],na.rm=T)

df_up$ptwork_for_score[which(df_up$ptwork_for == 4)] <- 1
df_up$ptwork_for_score[which(df_up$ptwork_for < 4)] <- 0


df_up$purchl_for <- 
  rowSums(df_up[,c("Q14_PURCHL_FEM_MGR_FOR",
                   "Q14_PURCHL_FEM_NON_FOR",
                   "Q14_PURCHL_MAL_MGR_FOR",
                   "Q14_PURCHL_MAL_NON_FOR")],na.rm=T)

df_up$purchl_for_score[which(df_up$purchl_for == 4)] <- 1
df_up$purchl_for_score[which(df_up$purchl_for < 4)] <- 0


df_up$tele_for <- 
  rowSums(df_up[,c("Q14_TELE_FEM_MGR_FOR",
                   "Q14_TELE_FEM_NON_FOR",
                   "Q14_TELE_MAL_MGR_FOR",
                   "Q14_TELE_MAL_NON_FOR")],na.rm=T)

df_up$tele_for_score[which(df_up$tele_for == 4)] <- 1
df_up$tele_for_score[which(df_up$tele_for < 4)] <- 0


df_up$toil_for <- 
  rowSums(df_up[,c("Q14_TOIL_FEM_MGR_FOR",
                   "Q14_TOIL_FEM_NON_FOR",
                   "Q14_TOIL_MAL_MGR_FOR",
                   "Q14_TOIL_MAL_NON_FOR")],na.rm=T)

df_up$toil_for_score[which(df_up$toil_for == 4)] <- 1
df_up$toil_for_score[which(df_up$toil_for < 4)] <- 0


df_up$unpdlv_for <- 
  rowSums(df_up[,c("Q14_UNPDLV_FEM_MGR_FOR",
                   "Q14_UNPDLV_FEM_NON_FOR",
                   "Q14_UNPDLV_MAL_MGR_FOR",
                   "Q14_UNPDLV_MAL_NON_FOR")],na.rm=T)

df_up$unpdlv_for_score[which(df_up$unpdlv_for == 4)] <- 1
df_up$unpdlv_for_score[which(df_up$unpdlv_for < 4)] <- 0

flexi_score_var <- c("carer_for_score","compww_for_score","flexhrs_for_score",
                      "jobsh_for_score","ptwork_for_score","purchl_for_score","tele_for_score",
                      "toil_for_score","unpdlv_for_score")

flexi_score_var
str(df_up)
df_up$var9 <- rowSums(df_up[,flexi_score_var],na.rm=T)
View(df_up[360:379])


# write.csv(df_up, file = "updated_df.csv")
# prop.table(df_up$Industry)

# Deriving variable - var9 (formal)

df_up$carer_infor <- 
  rowSums(df_up[,c("Q14_CARER_FEM_MGR_INF",
                   "Q14_CARER_FEM_NON_INF",
                   "Q14_CARER_MAL_MGR_INF",
                   "Q14_CARER_MAL_NON_INF")],na.rm=T)


df_up$carer_infor_score[which(df_up$carer_infor == 4)] <- 1
df_up$carer_infor_score[which(df_up$carer_infor < 4)] <- 0


df_up$compww_infor <- 
  rowSums(df_up[,c("Q14_COMPWW_FEM_MGR_INF",
                   "Q14_COMPWW_FEM_NON_INF",
                   "Q14_COMPWW_MAL_MGR_INF",
                   "Q14_COMPWW_MAL_NON_INF")],na.rm=T)


df_up$compww_infor_score[which(df_up$compww_infor == 4)] <- 1
df_up$compww_infor_score[which(df_up$compww_infor < 4)] <- 0


df_up$flexhrs_infor <- 
  rowSums(df_up[,c("Q14_FLEXHRS_FEM_MGR_INF",
                   "Q14_FLEXHRS_FEM_NON_INF",
                   "Q14_FLEXHRS_MAL_MGR_INF",
                   "Q14_FLEXHRS_MAL_NON_INF")],na.rm=T)

df_up$flexhrs_infor_score[which(df_up$flexhrs_infor == 4)] <- 1
df_up$flexhrs_infor_score[which(df_up$flexhrs_infor < 4)] <- 0


df_up$jobsh_infor <- 
  rowSums(df_up[,c("Q14_JOBSH_FEM_MGR_INF",
                   "Q14_JOBSH_FEM_NON_INF",
                   "Q14_JOBSH_MAL_MGR_INF",
                   "Q14_JOBSH_MAL_NON_INF")],na.rm=T)

df_up$jobsh_infor_score[which(df_up$jobsh_infor == 4)] <- 1
df_up$jobsh_infor_score[which(df_up$jobsh_infor < 4)] <- 0


df_up$ptwork_infor <- 
  rowSums(df_up[,c("Q14_PTWORK_FEM_MGR_INF",
                   "Q14_PTWORK_FEM_NON_INF",
                   "Q14_PTWORK_MAL_MGR_INF",
                   "Q14_PTWORK_MAL_NON_INF")],na.rm=T)

df_up$ptwork_infor_score[which(df_up$ptwork_infor == 4)] <- 1
df_up$ptwork_infor_score[which(df_up$ptwork_infor < 4)] <- 0


df_up$purchl_infor <- 
  rowSums(df_up[,c("Q14_PURCHL_FEM_MGR_INF",
                   "Q14_PURCHL_FEM_NON_INF",
                   "Q14_PURCHL_MAL_MGR_INF",
                   "Q14_PURCHL_MAL_NON_INF")],na.rm=T)

df_up$purchl_infor_score[which(df_up$purchl_infor == 4)] <- 1
df_up$purchl_infor_score[which(df_up$purchl_infor < 4)] <- 0


df_up$tele_infor <- 
  rowSums(df_up[,c("Q14_TELE_FEM_MGR_INF",
                   "Q14_TELE_FEM_NON_INF",
                   "Q14_TELE_MAL_MGR_INF",
                   "Q14_TELE_MAL_NON_INF")],na.rm=T)

df_up$tele_infor_score[which(df_up$tele_infor == 4)] <- 1
df_up$tele_infor_score[which(df_up$tele_infor < 4)] <- 0


df_up$toil_infor <- 
  rowSums(df_up[,c("Q14_TOIL_FEM_MGR_INF",
                   "Q14_TOIL_FEM_NON_INF",
                   "Q14_TOIL_MAL_MGR_INF",
                   "Q14_TOIL_MAL_NON_INF")],na.rm=T)

df_up$toil_infor_score[which(df_up$toil_infor == 4)] <- 1
df_up$toil_infor_score[which(df_up$toil_infor < 4)] <- 0


df_up$unpdlv_infor <- 
  rowSums(df_up[,c("Q14_UNPDLV_FEM_MGR_INF",
                   "Q14_UNPDLV_FEM_NON_INF",
                   "Q14_UNPDLV_MAL_MGR_INF",
                   "Q14_UNPDLV_MAL_NON_INF")],na.rm=T)

df_up$unpdlv_infor_score[which(df_up$unpdlv_infor == 4)] <- 1
df_up$unpdlv_infor_score[which(df_up$unpdlv_infor < 4)] <- 0


flexi_score_inform_var <- c("carer_infor_score","compww_infor_score","flexhrs_infor_score",
                     "jobsh_infor_score","ptwork_infor_score","purchl_infor_score","tele_infor_score",
                     "toil_infor_score","unpdlv_infor_score")

flexi_score_inform_var

df_up$var8 <- rowSums(df_up[,flexi_score_inform_var],na.rm=T)


# grep("Q11_2_nonleave_EmpSub_Childcare", colnames(df_up))
# 
# grep("Q11_2_parenting_workshops_fathers", colnames(df_up))
# 
# var_list2 <- names(df[,250:263])
# 
# grep("Q13_supp_DV_other_notFPS_YEAP", colnames(df_up))
# 
# grep("Q13_supp_DV_other_notFPS_Y_other", colnames(df_up))
# 
# var_list3 <- names(df[,266:271])

# Recoding ANZSIC1_LABEL variable to reduce cardinality

library(car)

df_up$industry <- recode(df_up$ANZSIC1_LABEL,"c('H','E',
                         'A')='Group 1';
                         c('R','O',
                         'A','A')='Group 2';
                         c('I','',
                         'P')='Group 3';
                         c('P','R, H',
                         'W',
                         'T')='Group 4';
                         c('A')='Group 5'")

var_list <- c("var1",
              "var2",
             
              )

View(df_up[399])
df_final <- df_up[,c(var_list,var_list2,var_list3)]

# Writing manipulated dataset
write.csv(df_final, file = "final.csv")



df_final <- read_csv("final.csv")
df_final_copy <- df_final[,-1]
str(df_final_copy)
colnames(df_final_copy)

# Missing value treatment for variables with missing data

miss_var_list <- c('var1','var2')

df_final_copy[miss_var_list][is.na(df_final_copy[miss_var_list])] <- 0
summary(df_final_copy['var2'])

# Resetting infinte value to 0 for var2
df_final_copy$var2[is.infinite(df_final_copy$var2)] <- 0
summary(df_final_copy['var2'])

var_df_final <- names(df_final_copy)
var_df_final

# write.csv(var_df_final, file = "var_final.csv")


iv_var_list <- var_df_final[!var_df_final %in% c('var1','var2')]

#extract only the independent variables
ivs <- df_final_copy[,iv_var_list]
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
# install.packages("ggplot2")
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

write.csv(cormat, file = "corr_matrix.csv")

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


# ---- Factor analysis : Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(dat)) # get eigenvalues
ap <- parallel(subject=nrow(dat),var=ncol(dat),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea, model="components")
plotnScree(nS)

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 5 factors, 
# with varimax rotation 
fit <- factanal(as.data.frame(dat), 12, rotation="varimax",lower = 0.002)
fit_2 <- print(fit, digits=2, cutoff=.4, sort=TRUE)
str(fit)

write.csv(fit_2$loadings, file = "factor_ansys.csv")


iv_var_list_final <- var_df_final[!var_df_final %in% c('var1','var2')]

iv_var_list_final
df_final_copy$industry <- as.factor(df_final_copy$industry)


# - RF VAR IMP PLOT
library(party)
df_final_Top3Perc_FEM <- subset(df_final_copy,!is.na(Top3Perc_FEM))
str(df_final_copy)
require(randomForest)
fit=randomForest(factor(df_final_Top3Perc_FEM$Top3Perc_FEM)~., data=df_final_Top3Perc_FEM[iv_var_list_final],
                 importance=T)
varImpPlot(fit)

# - RF VAR IMP PLOT

df_final_Perc_FEM_MAN <- subset(df_final_copy,!is.na(Perc_FEM_MAN))
str(df_final_copy)
require(randomForest)
fit=randomForest(factor(df_final_Perc_FEM_MAN$Perc_FEM_MAN)~., data=df_final_Perc_FEM_MAN[iv_var_list_final],
                 importance=T)
varImpPlot(fit)

# - RF VAR IMP PLOT

df_final_Perc_FEM_APP <- subset(df_final_copy,!is.na(Perc_FEM_APP))
str(df_final_copy)
require(randomForest)
fit=randomForest(factor(df_final_Perc_FEM_APP$Perc_FEM_APP)~., data=df_final_Perc_FEM_APP[iv_var_list_final],
                 importance=T)
varImpPlot(fit)

#  - RF VAR IMP PLOT

df_final_Perc_FEM_PROM <- subset(df_final_copy,!is.na(Perc_FEM_PROM))
str(df_final_copy)
require(randomForest)
fit=randomForest(factor(df_final_Perc_FEM_PROM$Perc_FEM_PROM)~., data=df_final_Perc_FEM_PROM[iv_var_list_final],importance=T)
varImpPlot(fit)

#  - RF VAR IMP PLOT

df_final_Perc_FEM_RESIG <- subset(df_final_copy,!is.na(Perc_FEM_RESIG))
str(df_final_copy)
require(randomForest)
fit=randomForest(factor(df_final_Perc_FEM_RESIG$Perc_FEM_RESIG)~., data=df_final_Perc_FEM_RESIG[iv_var_list_final],importance=T)
varImpPlot(fit)

# table(df_final_copy$Q2_1_GB_1_Board_Target,exclude=NULL)

df_final_copy$Q2_1_GB_1_Board_Target[which(df_final_copy$Q2_1_GB_1_Board_Target < 1)] <- 0
df_final_copy$Q2_1_GB_1_Board_Target[which(df_final_copy$Q2_1_GB_1_Board_Target >= 1)] <- 1

# Random sampling without replacement - PROPORTION
## 75% of the sample size
# smp_size <- floor(0.7 * nrow(df_final_copy))
# 
# ## set the seed to make your partition reproductible
# set.seed(123)
# train_ind <- sample(seq_len(nrow(df_final_copy)), size = smp_size)
# 
# train <- df_final_copy[train_ind, ]
# test <- df_final_copy[-train_ind, ]


# *****Model for Top*****

#compute correlation
# library(Hmisc)
# cor2 <- rcorr(as.matrix(df_final_copy[Top]), type="pearson")
# cor2

Top3Perc_FEM_var_list <- c('var1','var2')


linear_Top<- step(lm(df_final_copy$Top ~.,data = df_final_copy[Top3]),direction="both",k=3.84)
# linear_Top <- lm(df_final_copy$Top ~.,data = df_final_copy[Top3])

require(broom)    
tidy(linear_Top)

library(relaimpo)
mm = calc.relimp(linear_Top,type=c("lmg"),rela=TRUE)
mm

str(mm)
lmg <- as.data.frame(tidy(mm$lmg))
sorted <- lmg[order(-lmg$x),]
sorted

?barplot

barplot(mm$lmg,ylab="Relative importance of variable",xlab="Variable",main="Relative importance of explanatory variables",las=2)

# library(sjPlot)
# sjt.lm(linear_Top3Perc_FEM)

library(dummies)

df_final_copy_test <- dummy(df_final_copy$industry,sep="_")

df_final_industry_bind <- cbind(df_final_copy_test,df_final_copy[Perc_FEM_RESIG_var_list],
                                df_final_copy$Perc_FEM_RESIG)


write.csv(df_final_industry_bind,'corr.csv')

