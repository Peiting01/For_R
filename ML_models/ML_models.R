##Classification trees
##Supreme Court Cases
# Read in the data
Stevens = read.csv("/Users/USER/Desktop/ML_models/Stevens.csv")
str(Stevens)

# Split the data
library(caTools)
set.seed(9527)
spl = sample.split(Stevens$Reverse, SplitRatio = 0.7)
Train = subset(Stevens, spl==TRUE)
Test = subset(Stevens, spl==FALSE)

# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(partykit)

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
                    LowerCourt + Unconst, 
                    data = Train, method="class", minbucket=25,
                    parms = list(split="information"))

plot(StevensTree)
text(StevensTree,pretty=0)

prp(StevensTree)
prp(StevensTree, type = 1, extra = 1, split.font = 1, varlen = -10)

plot(as.party(StevensTree))

# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)

# ROC curve
library(ROCR)
PredictROC = predict(StevensTree, newdata = Test)
pred = prediction(PredictROC[,2], Test$Reverse)
x11(width=12,height=5)
par(mfrow=c(1,2))
plot(performance(pred, "acc"),col='green',lty=3,lwd=3)
plot(performance(pred, "tpr", "fpr"),col='green',lty=3,lwd=3)
abline(0,1,lty=2)


##Regression trees
##Boston Housing 
# Read in data
Boston = read.csv("/Users/USER/Desktop/ML_models/Boston.csv")
str(Boston)

# Plot observations
plot(Boston$LON, Boston$LAT)

# Tracts alongside the Charles River
points(Boston$LON[Boston$CHAS==1], Boston$LAT[Boston$CHAS==1], 
       col="blue", pch=19)

# Plot MIT
points(Boston$LON[Boston$TRACT==3531],Boston$LAT[Boston$TRACT==3531],
       col="red", pch=20)

# Plot polution
summary(Boston$NOX)
points(Boston$LON[Boston$NOX>=0.55], Boston$LAT[Boston$NOX>=0.55], 
       col="green", pch=20)

# Plot prices
plot(Boston$LON, Boston$LAT)
summary(Boston$MEDV)
points(Boston$LON[Boston$MEDV>=21.2], Boston$LAT[Boston$MEDV>=21.2], 
       col="red", pch=20)


# Load CART packages
library(rpart)
library(rpart.plot)
library(caTools)

# CART model
latlontree = rpart(MEDV ~ LAT + LON, data=Boston)
prp(latlontree)

# Visualize output
plot(Boston$LON, Boston$LAT)
points(Boston$LON[Boston$MEDV>=21.2], Boston$LAT[Boston$MEDV>=21.2], 
       col="red", pch=20)

fittedvalues = predict(latlontree)
points(Boston$LON[fittedvalues>21.2], Boston$LAT[fittedvalues>=21.2], 
       col="blue", pch="$")

# Simplify tree by increasing minbucket
latlontree = rpart(MEDV ~ LAT + LON, data=Boston, minbucket=50)
plot(latlontree)
text(latlontree)


# Let's use all the variables
# Split the data
set.seed(123)
split = sample.split(Boston$MEDV, SplitRatio = 0.7)
train = subset(Boston, split==TRUE)
test = subset(Boston, split==FALSE)

# Create linear regression
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + 
          AGE + DIS + RAD + TAX + PTRATIO, data=train)
summary(linreg)

# Make predictions
linreg.pred = predict(linreg, newdata=test)
linreg.sse = sum((linreg.pred - test$MEDV)^2)
linreg.sse


latlontree = rpart(MEDV ~ LAT + LON, data=train)
prp(latlontree)
latlon.pred = predict(latlontree, newdata=test)
latlon.sse = sum((latlon.pred - test$MEDV)^2)
latlon.sse


# Create a CART model
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM +
             AGE + DIS + RAD + TAX + PTRATIO, data=train)
prp(tree)

plot(tree)
text(tree,pretty=0)

plot(as.party(tree))


# Make predictions
tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse


cv.tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM +
                AGE + DIS + RAD + TAX + PTRATIO, data=train,
                cp=0.03)
prp(cv.tree)


printcp(cv.tree)

plot(cv.tree$cptable[,"nsplit"], 
     cv.tree$cptable[,"xerror"],type='o',pch=1)

treex=prune(cv.tree,cp=0.03)
prp(treex)
prune(cv.tree, cp = cv.tree$cptable[
  which.min(cv.tree$cptable[,"xerror"]),"CP"])


cv.tree.pred = predict(cv.tree, newdata=test)
cv.tree.sse = sum((cv.tree.pred - test$MEDV)^2)
cv.tree.sse


##Below is an alternative way to do the cp search
# Load libraries for cross-validation
library(caret)
library(e1071)

# Number of folds
tr.control = trainControl(method = "cv", number = 10)

# cp values
cp.grid = expand.grid( .cp = (0:10)*0.001)

# What did we just do?
1*0.001 
10*0.001 
0:10
0:10 * 0.001

# Cross-validation
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + 
            AGE + DIS + RAD + TAX + PTRATIO, data = train, 
           method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

# Extract tree
best.tree = tr$finalModel
prp(best.tree)

# Make predictions
best.tree.pred = predict(best.tree, newdata=test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)
best.tree.sse

##Classification trees
##Supreme Court Cases
# Read in the data
Stevens = read.csv("/Users/USER/Desktop/ML_models/Stevens.csv")
str(Stevens)

# Split the data
library(caTools)
library(rpart)
library(rpart.plot)
set.seed(9527)
spl = sample.split(Stevens$Reverse, SplitRatio = 0.7)
Train = subset(Stevens, spl==TRUE)
Test = subset(Stevens, spl==FALSE)

# Build a decision tree model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
                      LowerCourt + Unconst, 
                    data = Train, method="class", minbucket=25,
                    parms = list(split="information"))

library(randomForest)
# Build a random forest model
StevensForest = randomForest(as.factor(Reverse) ~ Circuit + Issue + 
                               Petitioner + Respondent + LowerCourt + Unconst,
                             data = Train, ntree=200, nodesize=25)

# ROC curve
library(ROCR)
PredictTree = predict(StevensTree, newdata = Test, type = "prob")
PredictForest = predict(StevensForest, newdata = Test, type = "prob")
predTree = prediction(PredictTree[,2], Test$Reverse)
predForest = prediction(PredictForest[,2], Test$Reverse)
x11(width=8,height=5)
plot(performance(predTree, "tpr", "fpr"),col='green',lty=3,lwd=3)
plot(performance(predForest, "tpr", "fpr"),col='red',add=T,lty=3,lwd=3)
abline(0,1,lty=2)

performance(predTree, "auc")
performance(predForest, "auc")


StevensForest = randomForest(as.factor(Reverse) ~ Circuit + Issue + 
                               Petitioner + Respondent + LowerCourt + Unconst,
                             data = Train, ntree=500, nodesize=25,
                             importance=TRUE)
varImpPlot(StevensForest)



##Regression trees
##Boston Housing 
# Read in data
Boston = read.csv("/Users/USER/Desktop/ML_models/Boston.csv")
str(Boston)

# Load CART packages
library(rpart)
library(rpart.plot)
library(caTools)

# Let's use all the variables
# Split the data
set.seed(123)
split = sample.split(Boston$MEDV, SplitRatio = 0.7)
train = subset(Boston, split==TRUE)
test = subset(Boston, split==FALSE)

# Create linear regression
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + 
              AGE + DIS + RAD + TAX + PTRATIO, data=train)
summary(linreg)

# Make predictions
linreg.pred = predict(linreg, newdata=test)
linreg.sse = sum((linreg.pred - test$MEDV)^2)
linreg.sse

#Train a random forest model
library(randomForest)
set.seed(5566)
rf.boston=randomForest(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS +
                         NOX + RM +  AGE + DIS + RAD + TAX + PTRATIO,
                       data=train, mtry=6, ntree=200, importance=TRUE)
rf.pred = predict(rf.boston, newdata=test)
rf.sse = sum((rf.pred - test$MEDV)^2)
rf.sse

varImpPlot(rf.boston)
importance(rf.boston)

tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM +
               AGE + DIS + RAD + TAX + PTRATIO, data=train)
prp(tree)
# Make predictions
tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse

##A bagging only forest
bag.boston=randomForest(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS +
                          NOX + RM +  AGE + DIS + RAD + TAX + PTRATIO,
                        data=train, mtry=13, ntree=200)
bag.boston
bag.pred = predict(bag.boston, newdata=test)
bag.sse = sum((bag.pred - test$MEDV)^2)
bag.sse


library(gbm)
gbm.boston=gbm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS +
                 NOX + RM +  AGE + DIS + RAD + TAX + PTRATIO,
               data=train, distribution="gaussian",
               n.trees=5000,interaction.depth=4)
gbm.pred=predict(gbm.boston,newdata=test,
                 n.trees=5000)
gbm.sse = sum((gbm.pred - test$MEDV)^2)
gbm.sse

summary(gbm.boston)

plot(gbm.boston,i="RM")
plot(gbm.boston,i="CRIM")


gbm.boston=gbm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS +
                 NOX + RM +  AGE + DIS + RAD + TAX + PTRATIO,
               data=train, distribution="gaussian",
               n.trees=5000,interaction.depth=4,
               shrinkage=0.2,cv.folds=5)
print(gbm.boston)
gbm.pred=predict(gbm.boston,newdata=test,
                 n.trees= )
gbm.sse = sum((gbm.pred - test$MEDV)^2)
gbm.sse




##Analyze Bank.csv
Bank = read.csv("/Users/USER/Desktop/ML_models/Bank.csv",header=TRUE,sep = ";")
colnames(Bank)
Bank.df = Bank[,c(1:8,17)]
colnames(Bank.df)
Bank.df[,9]=ifelse(Bank.df[,9]=="yes",1,0)

library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(partykit)

set.seed(9527)
spl = sample.split(Bank.df$y, SplitRatio = 0.7)
Train = subset(Bank.df, spl==TRUE)
Test = subset(Bank.df, spl==FALSE)


#Fit a logistic regression model
logit.Bank=glm(y~., data=Train, family=binomial(link="logit"))
summary(logit.Bank)
logit.out=predict(logit.Bank,newdata=Test, type="response")

tree.Bank=rpart(y ~ age + job + marital + education + default + balance
                + housing + loan , data = Train, method="class", 
                minbucket=25, parms = list(split="information"))
prp(tree.Bank)
plot(as.party(tree.Bank))
tree.out = predict(tree.Bank, newdata=Test, type="prob")

# Build a random forest model
rf.Bank = randomForest(as.factor(y)~ age + job + marital + education + default + balance
                       + housing + loan, data = Train, ntree=500, 
                       nodesize=25, importance=TRUE)
varImpPlot(rf.Bank)
rf.out=predict(rf.Bank, newdata = Test, type = "prob")

gbm.Bank=gbm(y ~ age + job + marital + education + default + balance
             + housing + loan, data=Train, distribution="bernoulli",
             n.trees=1500,interaction.depth=4)
print(gbm.Bank)
gbm.out=predict(gbm.Bank, newdata=Test,
                n.trees=1500, type= "response")



# ROC curve
library(ROCR)
predlogit = prediction(logit.out, Test$y)
predtree = prediction(tree.out[,2], Test$y)
predrf = prediction(rf.out[,2], Test$y)
predgbm = prediction(gbm.out, Test$y)
#
x11(width=12,height=5)
par(mfrow=c(1,2))
plot(performance(predlogit, "acc"),col='red',lty=1,lwd=3)
plot(performance(predtree, "acc"),col='blue',add=T,lty=2,lwd=3)
plot(performance(predrf, "acc"),col='green',add=T,lty=3,lwd=3)
plot(performance(predgbm, "acc"),col='yellow',add=T,lty=4,lwd=3)
#
plot(performance(predlogit, "tpr", "fpr"),col='red',lty=1,lwd=3)
plot(performance(predtree, "tpr", "fpr"),col='blue',add=T,lty=2,lwd=3)
plot(performance(predrf, "tpr", "fpr"),col='green',add=T,lty=3,lwd=3)
plot(performance(predgbm, "tpr", "fpr"),col='yellow',add=T,lty=4,lwd=3)
abline(0,1,lty=2)

performance(predlogit, "auc")
performance(predtree, "auc")
performance(predrf, "auc")
performance(predgbm, "auc")









