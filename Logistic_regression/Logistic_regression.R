loan.df=read.csv("/Users/USER/Desktop/Logistic_regression/UniversalBank.csv")
###logistic regression
logit.reg=glm(Personal.Loan~Income,data=loan.df,family="binomial")
options(scipen=999)
summary(logit.reg)
# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, loan.df[,c(4,10)], type = "response");fitted(logit.reg)
#Personal.Loan:0/1, where 0 as the baseline--> how to turn factor as 0/1-->do not execute
#loan.df$Personal.Loan<-1*(loan.df$Personal.Loan=="accepted") 

loan2.df=loan.df[,-c(1,5)] #drop ID and zi code columns for regression 
logit.reg2=glm(Personal.Loan~.,data=loan2.df,family="binomial")
summary(logit.reg2)

str(loan2.df)
#naming factor variable
loan3.df=loan2.df
loan3.df$Education <- factor(loan3.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))
logit.reg3=glm(Personal.Loan~.,data=loan3.df,family="binomial")
summary(logit.reg3)

#performance metrix
table(true=loan3.df$Personal.Loan,pred=round(fitted(logit.reg3)))
#accuracy, TPR, FPR, TNR


# Confusion matrix for threshold of 0.3
table(true=loan3.df$Personal.Loan, pred=(fitted(logit.reg3) > 0.3))
#ROC
install.packages("ROCR")
library(ROCR)
# Prediction function
ROCpred=prediction(fitted(logit.reg3), loan3.df$Personal.Loan)
# Performance function
ROCRperf = performance(ROCpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
#threshold on the right
plot(ROCRperf, colorize=TRUE)
#Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1))
# AUC
plot(ROCRperf) # Plot ROC curve
performance(ROCpred, "auc")
abline(0, 1, lty=2) #random guess

##model fit
logLik(logit.reg2) #large probability better: -2 > -3
logLik(logit.reg3)
anova(logit.reg2, logit.reg3,test= "Chisq") #chi-square = -2(LLbase-LLNew)

###prediction
#split data as training and validation
nrow(loan3.df)
set.seed(1)  # Set the seed for the random number generator for reproducing the 
# partition.
train.index=sample(1:nrow(loan3.df),
                   3000,replace=FALSE)
training=loan3.df[train.index,]
testing=loan3.df[-train.index,]
##train the data
fit.logit.1=glm(Personal.Loan~.,
                data=training,
                family=binomial(link="logit"))
fit.logit.2=glm(Personal.Loan~Age+Experience+Income+as.factor(Family)+CCAvg+Education+Mortgage+Securities.Account+CD.Account+Online+CreditCard,
                data=training,
                family=binomial(link="logit"))
summary(fit.logit.1)
summary(fit.logit.2)

##generate out-of-sample prediction
pred.out.1=predict(fit.logit.1,
                   newdata=testing,
                   type="response")
pred.out.2=predict(fit.logit.2,
                   newdata=testing,
                   type="response")


##evaluate model performance via validation data
library(ROCR)
#instead of in-sample fit, use out-of-sample prediction
ROCpred1=prediction(pred.out.1, testing$Personal.Loan)
ROCpred2=prediction(pred.out.2, testing$Personal.Loan)
ROCRperf1 = performance(ROCpred1, "tpr", "fpr")
ROCRperf2 = performance(ROCpred2, "tpr", "fpr")
#plot ROC curve
plot(ROCRperf1,col='red',lty=2,lwd=2)
plot(ROCRperf2,add=T,col='green',lty=3,lwd=2)
abline(0,1,lty=2)
#AUC
performance(ROCpred1,"auc")
performance(ROCpred2,"auc")

###Multinominal logistic regression
##three loan options
loan4.df=loan3.df
data_index=sample(1:nrow(loan4.df),
                   2000,replace=FALSE)
loan4.df[data_index,8]=2 #assum 2 indicating not decided
#try estimation without naming
loan4.df$Personal.Loan <- factor(loan4.df$Personal.Loan, levels = c(0,1,2), 
                             labels = c("No Loan", "Loan", "Considered"))


library(nnet)
mlogit1=multinom(Personal.Loan~ Income, data=loan4.df)
summary(mlogit1) #no p-value reported
library(AER)
coeftest(mlogit1) 

#default uses most common category as the base and we can change the baseline
loan4.df$Personal.Loan<-relevel(loan4.df$Personal.Loan,ref='Considered')

##choose the cateogry with the highest probabilty
head(fitted(mlogit1),10)
#predict categories instead of log of odds (logistic reg)
head(predict(mlogit1),10) 

##compare fitted and actual
table(predict(mlogit1))
table(loan4.df$Personal.Loan)

loan4.df$Personal.Loan<-relevel(loan4.df$Personal.Loan,ref='No Loan')
mlogit2=multinom(Personal.Loan~ ., data=loan4.df)
summary(mlogit2)
coeftest(mlogit2)
table(predict(mlogit2))
table(loan4.df$Personal.Loan)

#Can you try the training & testing splits?