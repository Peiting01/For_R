
maxmin=function(x){
       return ((x - min(x)) / (max(x) - min(x)))
}

UniversalBank = read.csv('/Users/USER/Desktop/NeuralNet/UniversalBank.csv')
##Classification
loan.df=UniversalBank[,-c(1,5)] #drop ID and zi code columns 
Under=ifelse(loan.df$Education==1,1,0)
Grad=ifelse(loan.df$Education== 2,1,0)
loan.df=cbind(loan.df,Under,Grad)

#Exclude the Eduction colution
loan2.df=loan.df[,-6]
loan2.df=as.data.frame(lapply(loan2.df, maxmin))

#split data as training and validation
nrow(loan2.df)
set.seed(5566)  # Set the seed for the random number generator for reproducing the 
# partition.
train.index=sample(1:nrow(loan2.df),
                   3500,replace=FALSE)
training=loan2.df[train.index,]
testing=loan2.df[-train.index,]

#Exclude the target variable for the trained neural net to predict
colnames(testing)
temp.test=testing[,-7]
colnames(temp.test)

library(neuralnet)
colnames(training)
nn=neuralnet(Personal.Loan ~ Age + Experience + Income + Family +            
             CCAvg + Mortgage + Securities.Account + CD.Account + 
             Online + CreditCard + Under + Grad, data=training, 
             stepmax = 1e+03, hidden=c(2,1), linear.output=FALSE, 
             threshold=0.01)
nn$result.matrix
plot(nn)

nn.results=compute(nn, temp.test)
nn.out=nn.results$net.result

#Fit a logistic regression model
fit.logit=glm(Personal.Loan ~ Age + Experience + Income + Family +            
                CCAvg + Mortgage + Securities.Account + CD.Account + 
                Online + CreditCard + Under + Grad, data=training,
              family=binomial(link="logit"))
logit.out=predict(fit.logit,newdata=testing,type="response")

#detach(package:neuralnet)

library(ROCR)
pred.nn=prediction(nn.out,
                   testing$Personal.Loan)
pred.logit=prediction(logit.out,
                      testing$Personal.Loan)

x11(width=12,height=5)
par(mfrow=c(1,2))
plot(performance(pred.nn,"acc"),col='red',lty=2,lwd=2,ylim=c(0,1))
plot(performance(pred.logit,"acc"),add=T,col='green',lty=3,lwd=2)
plot(performance(pred.nn,"tpr","fpr"),col='red',lty=2,lwd=2)
plot(performance(pred.logit,"tpr","fpr"),add=T,col='green',lty=3,lwd=2)
abline(0,1,lty=2)

performance(pred.nn,"auc")
performance(pred.logit,"auc")



##Regression
maxmin=function(x){
       return ((x - min(x)) / (max(x) - min(x)))
}

##Analyze the ToyotaCorolla file
##Import the ToyotaCorolla.csv first
car.data = read.csv('/Users/USER/Desktop/NeuralNet/ToyotaCorolla.csv')
# select variables for regression
selected.var = c(3, 4, 7, 9, 10, 12, 13, 14, 17, 18)
# use first 1000 rows of data
car.data = car.data[1:1000, selected.var]

car.df=as.data.frame(lapply(car.data, maxmin))

# partition data
set.seed(5566)  # set seed for reproducing the partition
train.index = sample(c(1:1000), 700, replace=FALSE)  
train.df = car.df[train.index, ]
valid.df = car.df[-train.index,]

#Exclude the target variable for the trained neural net to predict
temp.valid=valid.df[,-1]
colnames(temp.valid)

colnames(train.df)
car.nn=neuralnet(Price ~ Age_08_04+KM+HP+Met_Color+Automatic+
                  CC+Doors+Quarterly_Tax+Weight, 
                  data=train.df, stepmax = 2e+05,
                  hidden=c(2,1), linear.output=TRUE, 
                  threshold=0.01)
car.nn$result.matrix
plot(car.nn)

car.nn.results=compute(car.nn, temp.valid)
car.nn.out=car.nn.results$net.result

#Fit a linear regression model
car.lm=lm(Price ~ ., data = train.df)
summary(car.lm)
car.lm.out = predict(car.lm, valid.df)


y=car.data$Price
nn.yhat=car.nn.out*abs(diff(range(y)))+min(y)
lm.yhat=car.lm.out*abs(diff(range(y)))+min(y)

valid.Price=car.data$Price[-train.index]

mean(abs(nn.yhat-valid.Price)/valid.Price)
mean(abs(lm.yhat-valid.Price)/valid.Price)
