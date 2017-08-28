# Importing data :

input_data<-read.csv("C:/Users/VGX Training01/Desktop/Saranya - BI/Loans.csv",header=T,sep=",")

# Data vizualization :
hist(input_data[,3])
hist(input_data[,4])
hist(input_data[,5])
hist(input_data[,6])
hist(input_data[,7])
hist(input_data[,8])
hist(input_data[,9])
hist(input_data[,10])

# data processing 

# Converting variables into factors :
input_data$CreditPolicy<-as.factor(input_data$CreditPolicy)
input_data$NotFullyPaid<-as.factor(input_data$NotFullyPaid)
input_data$PubRec<-as.factor(input_data$PubRec)

# Binning InqLast6mths
quantile(input_data$InqLast6mths, probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
input_data$Binned_InqLast6mths<-ifelse(input_data$InqLast6mths>=4 & input_data$InqLast6mths <=7,"4-7",ifelse(input_data$InqLast6mths>=8,"8-33",input_data$InqLast6mths))
input_data$Binned_InqLast6mths<-as.factor(input_data$Binned_InqLast6mths)
xtabs(~Binned_InqLast6mths,data=input_data)

# Binning Delinq2yrs
xtabs(~Delinq2yrs,data=input_data)
input_data$Binned_Delinq2yrs<-ifelse(input_data$Delinq2yrs>=2,"2-13",input_data$Delinq2yrs)
input_data$Binned_Delinq2yrs<-as.factor(input_data$Binned_Delinq2yrs)
xtabs(~Binned_Delinq2yrs,data=input_data)

new_input_data<-data.frame(input_data[c(1,2,13,14,15,16)],scale(log10(input_data[,c(3,4,6:10)]+1)),LogAnnualInc=scale(input_data[,5]))

# Sampling
indexes<-sample(nrow(new_input_data)-1,size=.70*nrow(new_input_data))
train<-new_input_data[indexes,]
test<-new_input_data[-indexes,]

xtabs(~NotFullyPaid,data=new_input_data)
xtabs(~NotFullyPaid,data=train)
xtabs(~NotFullyPaid,data=test)

# modelling part

model_LR<-glm(NotFullyPaid~.,family=binomial(link='logit'),data=train)
summary(model_LR)

# Step wise regression :
full_model<-glm(NotFullyPaid~.,family=binomial(link='logit'),data=train)
null_model<-glm(NotFullyPaid~1,family=binomial(link='logit'),data=train)

# Forward step wise 
forward<-step(null_model, scope=list(lower=formula(null_model),upper=formula(full_model)),direction="forward")
formula(forward)

# Altered glm()
altered_model_LR<-glm(NotFullyPaid ~ Fico + Binned_InqLast6mths + Purpose + Installment + 
  LogAnnualInc + CreditPolicy + DaysWithCrLine + Binned_Delinq2yrs + 
  PubRec,family=binomial(link='logit'),data=train)
summary(altered_model_LR)

# Prediction on test data 
test$predicted_probability<-predict(altered_model_LR,test,type="response")
test$predicted_class<-ifelse(test$predicted_probability>0.40,1,0)
xtabs(~predicted_class+NotFullyPaid,test)

misclassification<-mean(test$predicted_class != test$NotFullyPaid )
misclassification
Accuracy=1-misclassification
Accuracy


# CART tree 
cart_input_data <-data.frame(input_data[c(1,2,13,14,15,16,3,4,6:10,5)])

indexes_cart<-sample(nrow(cart_input_data)-1,size=.70*nrow(cart_input_data))
train_cart<-cart_input_data[indexes_cart,]
test_cart<-cart_input_data[-indexes_cart,]

xtabs(~NotFullyPaid,data=cart_input_data)
xtabs(~NotFullyPaid,data=train_cart)
xtabs(~NotFullyPaid,data=test_cart)

ctrl<-rpart.control(minbucket=100, minsplit=200,cp=0,xval=10)
model_cart<-rpart(NotFullyPaid~.,data=train_cart,method="class",control=ctrl)
printcp(model_cart)
plot(model_cart)
text(model_cart)
