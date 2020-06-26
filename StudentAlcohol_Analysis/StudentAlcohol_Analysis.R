# import data
student = read.table("C:\\Users\\USER\\Desktop\\StudentAlcohol_Analysis\\student.csv", header = T, sep = ",")
summary(student)
str(student)

# plot the distribution of dependant variables
library(barcode)
barplot(table(student$Alc), main="Alcohol Distribution", xlab="Levels")

# turn into dummy
student$school = ifelse(student$school=="GP",1,0)
student$famsize = ifelse(student$famsize=="GT3",1,0)
student$sex = ifelse(student$sex=="F",1,0)
student$address = ifelse(student$address=="U",1,0)
student$Pstatus = ifelse(student$Pstatus=="A",1,0)
student$schoolsup = ifelse(student$schoolsup=="yes",1,0)
student$famsup = ifelse(student$famsup=="yes",1,0)
student$activities = ifelse(student$activities=="yes",1,0)
student$paid = ifelse(student$paid=="yes",1,0)
student$nursery = ifelse(student$nursery=="yes",1,0)
student$higher = ifelse(student$higher=="yes",1,0)
student$internet = ifelse(student$internet=="yes",1,0)
student$romantic = ifelse(student$romantic=="yes",1,0)
student$r_course = ifelse(student$reason=="course",1,0)
student$r_home = ifelse(student$reason=="home",1,0)
student$r_reputation = ifelse(student$reason=="reputation",1,0)
student$Alc = ifelse(student$Alc==1|student$Alc==2,0,1)

# plot the distribution of dependant variables
barplot(table(student$Alc), main="Alcohol Distribution", xlab="Levels")


# calculate average grade per course
student$math_avg_grade = (student$matG1+student$matG2+student$matG3)/3
student$por_avg_grade = (student$porG1+student$porG2+student$porG3)/3
# drop detail grade & no-need variable
student = student[,-c(8,(25:30))]
# turn grade from double to int
for (i in c(27:28)){
  student[,i] = as.integer(student[,i])
}


# relationship between variables
library(corrplot)
corrplot.mixed(corr = cor(student), upper="ellipse", tl.pos = "lt")
cor(student[,1:28])



#kmeams
require(factoextra)
str(student)
studentType = student[,c(2:28)]
set.seed(119)
fviz_nbclust(x = scale(studentType),FUNcluster = kmeans, method = "silhouette")
# cluster by optimal k=2
students.seg = kmeans(scale(studentType), centers=2, nstart = 100, iter.max = 200)
students.seg
table(students.seg$cluster)
students.seg$withinss
studentType$cluster = students.seg$cluster

student$cluster = students.seg$cluster
fviz_cluster(students.seg,           
             data = studentType,              
             geom = c("point","text"),
             frame.type = "norm")
alc_cluster = split.data.frame(student, student$cluster)
cluster1 = alc_cluster$`1`
cluster2 = alc_cluster$`2`

##continuous
library(ggplot2)
###
box = ggplot(student, aes(cluster, failures))
box + geom_boxplot(aes(fill = factor(cluster)))

box = ggplot(student, aes(cluster, absences))
box + geom_boxplot(aes(fill = factor(cluster)))
###
box = ggplot(student, aes(cluster, math_avg_grade))
box + geom_boxplot(aes(fill = factor(cluster)))
###
box = ggplot(student, aes(cluster, por_avg_grade))
box + geom_boxplot(aes(fill = factor(cluster)))

## noncontinous
###
bar = ggplot(student, aes(x = cluster, fill=factor(school)))
bar + geom_bar(position="fill")  
###
bar = ggplot(student, aes(x = cluster, fill=factor(studytime)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(sex)))
bar + geom_bar(position="fill") 

bar = ggplot(student, aes(x = cluster, fill=factor(age)))
bar + geom_bar(position="fill") 

bar = ggplot(student, aes(x = cluster, fill=factor(address)))
bar + geom_bar(position="fill") 

bar = ggplot(student, aes(x = cluster, fill=factor(famsize)))
bar + geom_bar(position="fill") 

bar = ggplot(student, aes(x = cluster, fill=factor(Pstatus)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(traveltime)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(schoolsup)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(famsup)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(paid)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(activities)))
bar + geom_bar(position="fill")

bar = ggplot(student, aes(x = cluster, fill=factor(nursery)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(higher)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(internet)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(romantic)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(famrel)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(freetime)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(goout)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(health)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(r_course)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(r_home)))
bar + geom_bar(position="fill")  

bar = ggplot(student, aes(x = cluster, fill=factor(r_reputation)))
bar + geom_bar(position="fill")  

#Training & Testing set
##Cluster 1
set.seed(345)
selected.var1 = c(1:28)
train.index1 = sample(c(1:nrow(cluster1)), nrow(cluster1)*0.7)
train.df1 = cluster1[train.index1, selected.var1]
valid.df1 = cluster1[-train.index1, selected.var1]

##Cluster2 
set.seed(345)
selected.var = c(1:28)
train.index = sample(c(1:nrow(cluster2)), nrow(cluster2)*0.7)
train.df2 = cluster2[train.index, selected.var]
valid.df2 = cluster2[-train.index, selected.var]


#------------------------------------------------------------------------------------------------
#Cluster 1


##Logistic
library(glmnet)
logit.reg1 <- glm(Alc~., data = train.df1, family="binomial"(link="logit"))
summary(logit.reg1)
##predict for Logistic
PredictLogit1<- predict(logit.reg1, valid.df1, type = "response")
library(ROCR)
predLogit1 = prediction(PredictLogit1,valid.df1$Alc)
PredictLogit1

#Random Forest
library(rpart)
library(rpart.plot)
library(partykit)
library(randomForest)
library(ROCR)
library(PerformanceAnalytics)

##Find best ntree
set.seed(666)
RFtest1 <- randomForest(formula = Alc ~ ., data = train.df1)
RFtest1
plot(RFtest1)
best.ntree1 = which.min(RFtest1$mse)
best.ntree1
##Build Forest
RF1 = randomForest(as.factor(Alc) ~ ., data = train.df1, ntree=best.ntree1, nodesize=25, importance=TRUE)
x11(width=8,height=5)
varImpPlot(RF1)
##Predict for RF
PredictForest1 = predict(RF1, newdata = valid.df1, type = "prob")
predForest1 = prediction(PredictForest1[,2], valid.df1$Alc)

##GBM
library(gbm)
gbm.UB1=gbm(Alc ~ ., data=train.df1, distribution="bernoulli",
            n.trees=best.ntree1, interaction.depth=4, shrinkage = 0.05)
summary(gbm.UB1)
print(gbm.UB1)

##Predict for GBM
gbm.out1=predict(gbm.UB1, newdata=valid.df1, n.trees=best.ntree1, interaction.depth=4, shrinkage = 0.05, type= "response")
predGBM1 = prediction(gbm.out1, valid.df1$Alc)

##Neuralnet
library(neuralnet)
maxmin=function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
cluster1_maxmin=as.data.frame(lapply(cluster1, maxmin))

set.seed(45)
selected.var1 = c(1:28)
train.index1 = sample(c(1:nrow(cluster1_maxmin)), nrow(cluster1_maxmin)*0.7)
training.1 = cluster1_maxmin[train.index1, selected.var1]
valid.1 = cluster1_maxmin[-train.index1, selected.var1]


nn1=neuralnet(Alc ~ +., data=training.1, 
              stepmax = 1e+03, hidden=c(3,1), linear.output=FALSE, 
              threshold=0.2)
nn1$result.matrix
plot(nn1)


##predict for nn
nn1.results=compute(nn1, valid.1[,-1])
nn1.out=nn1.results$net.result
detach(package:neuralnet)
library(ROCR)
predNN1=prediction(nn1.out[,1], valid.1$Alc)


x11(width=12,height=5)
par(mfrow=c(1,2))

##Compare Accuracy
plot(performance(predLogit1, "acc"),col='yellow', lwd=3)
plot(performance(predForest1, "acc"),col='green', add=T, lwd=3)
plot(performance(predGBM1, "acc"),col='red', add=T, lwd=3)
plot(performance(predNN1, "acc"),col='blue', add=T, lwd=3)
title('Accuracy for Cluster1')
legend("bottomright", c("Logit","RF","GBM","NN"),
       col = c('yellow','green','red','blue'), lwd = c(3,3,3,3))


##Compare ROC
plot(performance(predLogit1, "tpr", "fpr"),col='yellow', lwd=3)
plot(performance(predForest1, "tpr", "fpr"),col='green', add=T, lwd=3)
plot(performance(predGBM1, "tpr", "fpr"),col='red', add=T, lwd=3)
plot(performance(predNN1, "tpr", "fpr"),col='blue', add=T, lwd=3)
title('ROC Curve for Cluster1')
legend("bottomright", c("Logit","RF","GBM","NN"),
       col = c('yellow','green','red','blue'), lwd = c(3,3,3,3))
abline(0,1,lty=2)

##AUC
performance(predLogit1, "auc")@y.values
performance(predForest1, "auc")@y.values # highest
performance(predGBM1, "auc")@y.values
performance(predNN1, "auc")@y.values # lowest

#------------------------------------------------------------------------------------------------
##Cluster 2

##Logistic
logit.reg2 <- glm(Alc~., data = train.df2, family="binomial"(link="logit"))
summary(logit.reg2)
##Predict for Logistic
PredictLogit2 <- predict(logit.reg2, valid.df2, type = "response")
predLogit2 <- prediction(PredictLogit2, valid.df2$Alc)


#Random Forest
##Find best ntree
set.seed(666)
RFtest2 <- randomForest(formula = Alc ~ ., data = train.df2)
RFtest2
plot(RFtest2)
best.ntree2 = which.min(RFtest2$mse)
best.ntree2
##Build Forest
RF2 = randomForest(as.factor(Alc) ~ ., data = train.df2, ntree=best.ntree2, nodesize=25, importance=TRUE)
x11(width=8,height=5)
varImpPlot(RF2)
##Predict for RF
PredictForest2 = predict(RF2, newdata = valid.df2, type="prob")
PredictForest2
predForest2 = prediction(PredictForest2[,2], valid.df2$Alc)


##GBM
gbm.UB2=gbm(Alc ~ ., data=train.df2, distribution="bernoulli",
            n.trees=best.ntree2,interaction.depth=4, shrinkage = 0.05)
summary(gbm.UB2)
print(gbm.UB2)
##Predict for GBM
gbm.out2 = predict(gbm.UB2, newdata=valid.df2, n.trees=best.ntree2, interaction.depth=4, shrinkage = 0.05, type= "response")
predGBM2 = prediction(gbm.out2, valid.df2$Alc)

##Neuralnet
library(neuralnet)
maxmin=function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
cluster2_maxmin=as.data.frame(lapply(cluster2, maxmin))

set.seed(45)
selected.var2 = c(1:28)
train.index2 = sample(c(1:nrow(cluster2_maxmin)), nrow(cluster2_maxmin)*0.7)
training.2 = cluster2_maxmin[train.index2, selected.var2]
valid.2 = cluster2_maxmin[-train.index2, selected.var2]


nn2=neuralnet(Alc ~ +., data=training.2, 
              stepmax = 1e+03, hidden=c(3,1), linear.output=FALSE, 
              threshold=0.2)
nn2$result.matrix
plot(nn2)


##predict for nn
nn2.results=compute(nn2, valid.2[,-1])
nn2.out=nn2.results$net.result
detach(package:neuralnet)
library(ROCR)
predNN2=prediction(nn2.out, valid.2$Alc)


x11(width=12,height=5)
par(mfrow=c(1,2))
##Compare Accuracy
plot(performance(predLogit2, "acc"),col='yellow', lwd=3)
plot(performance(predForest2, "acc"),col='green', add=T, lwd=3)
plot(performance(predGBM2, "acc"),col='red', add=T, lwd=3)
plot(performance(predNN2, "acc"),col='blue', add=T, lwd=3)
title('Accuracy for Cluster2')
legend("bottomright", c("Logit","RF","GBM","NN"),
       col = c('yellow','green','red','blue'), lwd = c(3,3,3,3))


##Compare ROC
plot(performance(predLogit2, "tpr", "fpr"),col='yellow', lwd=3)
plot(performance(predForest2, "tpr", "fpr"),col='green', add=T, lwd=3)
plot(performance(predGBM2, "tpr", "fpr"),col='red', add=T, lwd=3)
plot(performance(predNN2, "tpr", "fpr"),col='blue', add=T, lwd=3)
title('ROC Curve for Cluster2')
legend("bottomright", c("Logit","RF","GBM","NN"),
       col = c('yellow','green','red','blue'), lwd = c(3,3,3,3,3,3))
abline(0,1,lty=2)


##AUC
performance(predLogit2, "auc")@y.values
performance(predForest2, "auc")@y.values
performance(predGBM2, "auc")@y.values # lowest
performance(predNN2, "auc")@y.values # highest

#------------------------------------------------------------------------------------------------
#Full Data

##Split Data
#Training & Testing set
set.seed(345)
selected.var = c(1:28)
train.index = sample(c(1:nrow(student)), nrow(student)*0.7)
train.df = student[train.index, selected.var]
valid.df = student[-train.index, selected.var]

##Logistic
logit.reg1 <- glm(Alc~., data = train.df, family="binomial"(link="logit"))
summary(logit.reg1)
# predict for logistic
PredictLogit<- predict(logit.reg1, valid.df, type = "response")
predLogit<- prediction(PredictLogit, valid.df$Alc)


##Random Forest
library(rpart)
library(rpart.plot)
library(partykit)
library(randomForest)

##Find best ntree
set.seed(666)
RFtest <- randomForest(formula = Alc ~ ., data = train.df)
RFtest
plot(RFtest)
best.ntree = which.min(RFtest$mse)
best.ntree
##Build Forest
RF = randomForest(as.factor(Alc) ~ ., data = train.df, ntree=best.ntree, nodesize=25)
##Predict for RF
PredictForest = predict(RF, newdata = valid.df, type = "prob")
predForest = prediction(PredictForest[,2], valid.df$Alc)
x11(width=8,height=5)
RF = randomForest(as.factor(Alc) ~ ., data = train.df, ntree=best.ntree, nodesize=25, importance=TRUE)
varImpPlot(RF)


##GBM
library(gbm)
##GBM for Weekday
gbm.UB=gbm(Alc ~ ., data=train.df, distribution="bernoulli", n.trees=best.ntree, interaction.depth=4, shrinkage = 0.05)
summary(gbm.UB)
print(gbm.UB)
##Predict for GBM
gbm.out=predict(gbm.UB, newdata=valid.df, n.trees=best.ntree, interaction.depth=4, shrinkage = 0.05, type= "response")
predGBM = prediction(gbm.out, valid.df$Alc)


##Neuralnet
library(neuralnet)
maxmin=function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
student_maxmin=as.data.frame(lapply(student, maxmin))

set.seed(45)
selected.var = c(1:28)
train.index = sample(c(1:nrow(student_maxmin)), nrow(student_maxmin)*0.7)
training.full = student_maxmin[train.index, selected.var]
valid.full = student_maxmin[-train.index, selected.var]

##Exclude the target variable for the trained neural net to predict
nn=neuralnet(Alc ~ +., data=training.full, 
             stepmax = 1e+03, hidden=c(3,1), linear.output=FALSE, 
             threshold=0.2)
nn$result.matrix
plot(nn, nid = T)

##predict for nn
nn.results=compute(nn, valid.full[,-1])
nn.out=nn.results$net.result
detach(package:neuralnet)
library(ROCR)
pred.nn=prediction(nn.out, valid.full$Alc)


x11(width=12,height=5)
par(mfrow=c(1,2))
##Compare Accuracy
plot(performance(predLogit, "acc"),col='yellow', lty=1, lwd=3)
plot(performance(predForest, "acc"),col='green', add=T, lty=2, lwd=3)
plot(performance(predGBM, "acc"),col='red', add=T, lty=3, lwd=3)
plot(performance(pred.nn, "acc"),col='blue', add=T, lty=4, lwd=3)
title('Accuracy for Student')
legend("bottomright", c("Logit","RF","GBM","NN"), col = c('yellow','green','red','blue'), lwd = c(3,3,3,3))

##Compare ROC
plot(performance(predLogit, "tpr", "fpr"),col='yellow',lty=1, lwd=3)
plot(performance(predForest, "tpr", "fpr"),col='green', add=T, lty=2, lwd=3)
plot(performance(predGBM, "tpr", "fpr"),col='red', add=T, lty=3, lwd=3)
plot(performance(pred.nn, "tpr", "fpr"),col='blue', add=T, lty=4, lwd=3)
title('ROC Curve for Student')
legend("bottomright", c("Logit","RF","GBM","NN"), col = c('yellow','green','red','blue'), lwd = c(3,3,3,3))
abline(0,1,lty=2)

##AUC
library(PerformanceAnalytics)
performance(predLogit, "auc")@y.values
performance(predForest, "auc")@y.values
performance(predGBM, "auc")@y.values
performance(pred.nn, "auc")@y.values



