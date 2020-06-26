options(device='windows') 
### 2.1 Data exploration
wine.df = read.csv("/Users/USER/Desktop/Linear_regression/wine.csv")
str(wine.df)
summary(wine.df)
#relationship between variables
library(gpairs)
gpairs(wine.df[2:7])
cor(wine.df[2:7])
library(corrplot) # for correlation plot, install if needed
corrplot.mixed(corr=cor(wine.df[2:7], use="complete.obs"),
                 upper="ellipse", tl.pos="lt")

### 2.2 Linear Regression (one variable)
plot(Price~AGST, data=wine.df, xlab='AVG Growing Season Temp', ylab='Logarithm of Price')
model1 = lm(Price ~ AGST, data=wine.df)
str(model1) 
model1$coefficients; coef(model1)
summary(model1)
plot(Price~AGST, data=wine.df, xlab='AVG Growing Season Temp', ylab='Logarithm of Price')
abline(model1, col='blue')

confint(model1)

cor(wine.df$Price, wine.df$AGST)^2

##residual check
res=residuals(model1)
mean(res) 
yhat=fitted(model1)
plot(yhat, res)
#residual when non-linear
x <- rnorm(500)
y <- x^2
toy.model <- lm(y~x)
plot(toy.model$fitted.values, toy.model$residuals)
#outlier
plot(model1,which=1) #first diagnosis diagram
wine.df[c(2,8,20),]

###2.3 Linear Regerssion (multiple)
model2 = lm(Price ~ AGST + HarvestRain, data=wine.df)
summary(model2)
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine.df)
summary(model3)
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine.df)
summary(model4)
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine.df)
summary(model5)

##dummy variables
ageCat=rep(0,length(wine.df$Age))
for (i in c(1:length(wine.df$Age))){
  if (wine.df$Age[i]>20){
    ageCat[i]=3
  } else if (wine.df$Age[i]<=10)
    {
      ageCat[i]=1
  }else{
      ageCat[i]=2
  }
}
wine.new.df=cbind(wine.df,ageCat)
#attach(wine.df)
#wine.new.df=as.data.frame(cbind(AGST, HarvestRain, WinterRain, ageCat))
model7 = lm(Price ~ AGST + HarvestRain + WinterRain+ageCat, data=wine.new.df)
summary(model7)
model6 = lm(Price ~ AGST + HarvestRain + WinterRain+as.factor(ageCat), data=wine.new.df)
summary(model6)
#what as.factor does
age2=ifelse(wine.new.df$ageCat==2,1,0)
age3=ifelse(wine.new.df$ageCat==3,1,0)
model8=lm(Price ~ AGST + HarvestRain + WinterRain+age2+age3, data=wine.new.df)
summary(model8)

##additional treatment on coefficients: 
confint(model8)

library(coefplot)
coefplot(model8, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Price",
         xlab="Association with Wine Price")

##model comparison
anova(model4, model5)

###2.4 predictive modeling
##training and validation set: wine example
wineTest = read.csv("/Users/USER/Desktop/Linear_regression/wine_test.csv")
str(wineTest)
# use predict() to make predictions on a new set. 
predictTest4 = predict(model4, newdata=wineTest)
predictTest4
SSE4 = sum((wineTest$Price - predictTest4)^2) # Compute R-squared
SST = sum((wineTest$Price - mean(wine.df$Price))^2)
1 - SSE4/SST
predictTest1 = predict(model1, newdata=wineTest)
SSE1 = sum((wineTest$Price - predictTest1)^2) # Compute R-squared
1 - SSE1/SST

library(forecast)
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
accuracy(predictTest4, wineTest$Price)

##training and validation set: Toyota example
car.df = read.csv("/Users/USER/Desktop/Linear_regression/ToyotaCorolla.csv")
str(car.df)
# use first 1000 rows of data
car.df = car.df[1:1000, ]
# select variables for regression
selected.var = c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
set.seed(5566)  # set seed for reproducing the partition
train.index = sample(c(1:1000), 600, replace=FALSE)  
train.df = car.df[train.index, selected.var]
valid.df = car.df[-train.index, selected.var]
# use lm() to run a linear regression of Price on all 11 predictors in the
# training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm = lm(Price ~ ., data = train.df)
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)

#evaluate prediction accuracy
library(forecast)
# use predict() to make predictions on a new set. 
car.lm.pred = predict(car.lm, valid.df)
options(scipen=999, digits = 0)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20] #fix typo lm instead of lm1
data.frame("Predicted" = car.lm.pred[1:20], 
           "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)
options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(car.lm.pred, valid.df$Price)

##cross validation: Toyota example
car.df.new = car.df[,c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)]
names(car.df.new) #examine data
table(car.df.new$Automatic) #check variable
car.lma = lm(Price~.,data=car.df.new )
#interaction
attach(car.df.new)
agexauto=Age_08_04*Automatic
car.df.interaction=cbind(car.df.new,agexauto)
lm(Price~ Age_08_04+agexauto+KM+Fuel_Type+HP+Met_Color+Automatic+CC+Doors+Quarterly_Tax+Weight,data=car.df.interaction)
#as.factor turns numerical to categorical; no differernce here due to 0/1 value
car.lmb=lm(Price~ Age_08_04+Age_08_04:Automatic+KM+Fuel_Type+HP+Met_Color+as.factor(Automatic)+CC+Doors+Quarterly_Tax+Weight,data=car.df.new)

library(DAAG)
cv.lm(data=car.df.new, car.lma, m=2)
cv.lm(data=car.df.new, car.lmb, m=2)


###2.4 Lasso Regression
##initial trial
# we need glmnet to run lasso
library(glmnet)
Hitters=readRDS(file='/Users/USER/Desktop/Linear_regression/Hitters.rds')
# The glmnet function works with the design matrix of predictors (without
# the ones). This can be obtained easily through model.matrix()
x <- model.matrix(Salary ~  + ., data = Hitters)
head(x[1:5,])
# -1 to exclude a column of 1's for the intercept, since the intercept will be 
# added by default in glmnet::glmnet and if we do not exclude it here we will 
# end with two intercepts, one of them resulting in NA
x <- model.matrix(Salary ~  ., data = Hitters) [,-1]
y <- Hitters$Salary
# use alpha = 1 for lasso regression (the default), family = gaussian for continuous variables(default)
lassoMod <- glmnet(x = x, y = y, alpha = 1,lambda=2,family = "gaussian")
coef(lassoMod)

## find the optimal lamda using cross validation
# 10-fold cross-validation. Change the seed for a different result
set.seed(12345)
cv.Lasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = 10)
# The lambda that minimises the CV error is
minLambda=cv.Lasso$lambda.min
#no specify lambda for various lamda values
lassoMod2 <- glmnet(x = x, y = y, alpha = 1)
#visualize how coefficient and lamda change; the top shows # of non-zero variables
plot(lassoMod2, xvar='lambda', main="Lasso")
abline(v=log(minLambda), col="blue", lty=5.5 )

## find the variables with non-zero coefficients
coef(cv.Lasso, s = "lambda.min")
# when many variables to look for
select.ind = which(coef(cv.Lasso, s = "lambda.min") != 0)
select.ind = select.ind[-1]-1 # remove Intercept and shift index accordingly
select.variables = colnames(x)[select.ind]
select.variables
#fix the column name transformation error
select.variables[10]="Division"
select.variables[9]="League"
# Use those non-zero variables to run a linear regression (post-Lasso)
# Compare coefficients with those from the lasso (shrinkaged)
lm(Salary ~ ., Hitters[, c(select.variables,"Salary")])

##Ridge and Lasso comparison
ridge = glmnet(x = x, 
               y = y, 
               alpha = 0,
               family = "gaussian")

lasso = glmnet(x = x, 
               y = y, 
               alpha = 1,
               family = "gaussian")

par(mfcol = c(1, 2))
plot(lasso, xvar='lambda', main="Lasso")
plot(ridge, xvar='lambda', main="Ridge")


  

