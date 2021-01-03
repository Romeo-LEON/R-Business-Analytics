###PROBLEM 1### 

#Question 1
#Load data 
auto <- read.csv('/Users/romeoleon/Desktop/ESCP/Cours/R & Business analytics/Homework/HMK3/auto.csv')
View(auto)

#Replace '?' with NA
question <- auto == "?" #Find '?' in auto dataset
is.na(auto) <- question #Replace '?' with NA

#Column with NA values
colSums(is.na(auto))

#Delete rows with missing data and check for NAs
auto_clean <- na.omit(auto)
colSums(is.na(auto_clean))

#Question 2

#Feature selection
library(Boruta)
set.seed(7)

selection_train <- Boruta(MPG~.,data = auto_clean,doTrace=2)
print(selection)
selection <- TentativeRoughFix(selection_train)
print(selection)

plot(selection)
attStats(selection)

#Feature selected (according to the Boruta values)
weight <- auto_clean$weight
year <- auto_clean$model.year

#Linear regression
linear_year <- lm(MPG~ year, data =auto_clean)
summary(linear_year)

linear_weight <- lm(MPG~weight,data=auto_clean)
summary(linear_weight)


#Plot
plot(auto_clean$MPG~auto_clean$model.year, main='MPG according to model year')
abline(linear_year,col='blue')

plot(auto_clean$MPG~auto_clean$weight, main='MPG according to weight')
abline(linear_weight,col='red')


#Local polynomial regression
library(locfit)
lp_year <- locfit(MPG~lp(model.year,deg = 4),data = auto_clean)
plot(lp_year,main='Local Polynomial Regression on MPG & Model Year')

lp_weight <- locfit(MPG~lp(weight,deg = 4),data=auto_clean)
plot(lp_weight,main='Local Polynomial Regression on MPG & Weight')

lp_both<- locfit(MPG~lp(model.year,weight,deg = 3),data=auto_clean)
plot(lp_both,main='Local Polynomial Regression on MPG with both Year & Weight')

##------------------------------------------------------------------------------#

###PROBLEM 2###

income <- read.csv('/Users/romeoleon/Desktop/ESCP/Cours/R & Business analytics/Homework/HMK3/income.csv')

#Transform income$income into boolean (to avoid later problem with logistic regression)
library(plyr)
income_bool <- mapvalues(x=income$income, from=c('<=50K','>50K'),to=c(0,1))#Map values to boolean
income$income <- income_bool

#EDA of the entire dataset with tidyverse and Hmisc
library(tidyverse)
library(Hmisc)
library(funModeling)
freq(income)
describe(income)

#Create training and test set with caTools
install.packages('caTools')
library(caTools)

set.seed(7)
sample <- sample.split(income$income,SplitRatio = 6000/7125) #Create sample of 6000 observations 
train_income = subset(income,sample==TRUE) #Select 6000 observations for training set
test_income = subset(income,sample==FALSE)

#Creating the logistic regression model
logr <- glm(as.factor(income)~.,family=binomial,data=train_income)

#Performing prediction
prediction_income <- predict(logr,test_income,type='response')

#Confusion matrix to assess the model (With 0 : for <=50K and 1 : for >50K) 
confusion_mat <- table(test_income$income,prediction_income>0.4)
plot(confusion_mat,main='Confusion Matrix of the logistic regression')

#Compute the accuracy of the model (TN + TP)/(TN + FP + FN + TP)
acc_logr <- (confusion_mat[1,1]+confusion_mat[2,2])/sum(confusion_mat)
acc_logr

#Compute the recall and precision : 

recall <- (confusion_mat[2,2])/(confusion_mat[2,2] + confusion_mat[1,2])

precision <- (confusion_mat[2,2])/(confusion_mat[2,2]+confusion_mat[2,1])

#Perform ROC analysis and AUC computation
TPR <- confusion_mat[2,2]/(confusion_mat[2,1]+confusion_mat[2,2])
TPR

FPR <- confusion_mat[1,2]/(confusion_mat[1,1]+confusion_mat[1,2])
FPR

library(ROCR) #We use the ROCR prediction & performance functions
prediction_bis <- prediction(prediction_income,test_income$income)
roc_perf <- performance(prediction_bis,'tpr','fpr')

auc_income <- performance(prediction_bis,'auc')
auc_income = unlist(slot(auc_income,"y.values"))

plot(roc_perf,main='ROC Curve with AUC',colorize=TRUE,lwd=4)
abline(0,1,lwd=2)
text(0.6,0.2,labels = paste('AUC = ',(auc_income)))

##Romeo LEON - MSc in Big Data and Business Analytics December 2020##