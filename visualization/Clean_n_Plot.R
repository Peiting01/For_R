
store.df <- read.csv('/Users/USER/Desktop/visualization/store.csv')
str(store.df)
table(store.df$country)
table(store.df$p1price)
p1.table = table(store.df$p1price)
table(store.df$p1price, store.df$p1prom)

min(store.df$p1sales)
mean(store.df$p1prom)
mad(store.df$p1sales)
IQR(store.df$p1sales) #Interquartile range, 75th?V25th percentile


mysummary.df <- data.frame(matrix(NA, nrow=2, ncol=2))
names(mysummary.df) <- c("Median Sales", "IQR") # same as below
#colnames(mysummary.df) <- c("Median Sales", "IQR")
rownames(mysummary.df) <- c("Product 1", "Product 2")
mysummary.df["Product 1", "Median Sales"] <- median(store.df$p1sales)
mysummary.df["Product 2", "Median Sales"] <- median(store.df$p2sales)
mysummary.df["Product 1", "IQR"] <- IQR(store.df$p1sales)
mysummary.df["Product 2", "IQR"] <- IQR(store.df$p2sales)
mysummary.df

str(store.df) 
store.df$storeNum2 <- factor(store.df$storeNum)#a new variable for category
str(store.df)
dim(store.df)
head(store.df)
tail(store.df)

summary(store.df)
summary(store.df$p1sales)

p1sales.sum = aggregate(store.df$p1sales,
                        by=list(country=store.df$country), sum)

p1sales.sum


library(psych) # make it available in current window
describe(store.df)
describe(store.df[ , c(2, 4:9)]) #column 2 and 4 to 9

apply(store.df[,2:10], 2, mean) #watch for categorical variable
apply(store.df[,2:9], 2, mean)
apply(store.df[, 2:9], 2, function(x){ mean(x) - median(x) })



#Histogram
options(device='windows') 
hist(store.df$p1sales)
hist(store.df$p1sales,
     main="Product 1 Weekly Sales Frequencies, All Stores", #main title
     xlab="Product 1 Sales (Units)", #x axis label
     ylab="Count" ) # y axis label
hist(store.df$p1sales,
       main="Product 1 Weekly Sales Frequencies, All Stores", #main title
       xlab="Product 1 Sales (Units)", #x axis label
       ylab="Count" ,  # y axis label
       breaks=30, # more columns
       col="lightblue") 
hist(store.df$p1sales,
       main="Product 1 Weekly Sales Frequencies, All Stores",
       xlab="Product 1 Sales (Units)",
       ylab="Relative frequency",
       breaks=30,
       col="lightblue",
       freq=FALSE, # freq=FALSE means plot density, not counts
       xaxt="n") # xaxt="n" means "x axis tick marks == no"
axis(side=1, at=seq(60, 300, by=20))
lines(density(store.df$p1sales,bw=10), # "bw= ..." adjusts the smoothing
      type="l", col="darkred", lwd=2) # lwd = line width

boxplot(store.df$p2sales, xlab="Weekly sales", ylab="P2",
        main="Weekly sales of P2, All stores", horizontal=TRUE)



##scatter plot
cust.df <- read.csv('/Users/USER/Desktop/visualization/customers.csv')
cust.df <- read.csv('/Users/USER/Desktop/visualization/customers.csv')
str(cust.df)

plot(x=cust.df$age, y=cust.df$credit.score)
plot(cust.df$age, cust.df$credit.score,
     col="blue",
     xlim=c(15, 55), ylim=c(500, 900),
     main="Active Customers as of June 2014",
     xlab="Customer Age (years)", ylab="Customer Credit Score ")
abline(h=mean(cust.df$credit.score), col="dark blue", lty="dotted")
abline(v=mean(cust.df$age), col="dark blue", lty="dotted")

plot(cust.df$store.spend, cust.df$online.spend,
     main="Customers as of June 2014",
     xlab="Prior 12 months in-store sales ($)",
     ylab="Prior 12 months online sales ($)" )

plot(log(cust.df$store.spend+1), log(cust.df$online.spend+1),
     main="Customers as of June 2014",cex=0.7,
     xlab="Prior 12 months in-store sales ($)",
     ylab="Prior 12 months online sales ($)" )

par(mfrow=c(2, 2))
plot(cust.df$distance.to.store, cust.df$store.spend, main="store")
plot(cust.df$distance.to.store, cust.df$online.spend, main="online")
plot(cust.df$distance.to.store, cust.df$store.spend+1, log="xy",
       main="store, log")
plot(cust.df$distance.to.store, cust.df$online.spend+1, log="xy",
       main="online, log")


library(gpairs)
gpairs(cust.df[ , c(2:10)]) 

##correlation statistics
cor(cust.df$age, cust.df$credit.score)
cov(cust.df$age, cust.df$credit.score) /
  (sd(cust.df$age)*sd(cust.df$credit.score))
cor.test(cust.df$age, cust.df$credit.score)
cor(cust.df[, c(2, 3, 5:12)])
--
cor(cust.df[, c(2, 3, 5:12)],use='complete.obs')
--  
library(corrplot) # for correlation plot, install if needed
corrplot.mixed(corr=cor(cust.df[ , c(2, 3, 5:12)], use="complete.obs"),
                 upper="ellipse", tl.pos="lt")

##Data transformation
set.seed(49931)
x <- runif(1000, min=-10, max=10)
cor(x, x^2)
cor(cust.df$distance.to.store, cust.df$store.spend)
cor(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)
plot(cust.df$distance.to.store, cust.df$store.spend)
plot(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)



seg.df <- read.csv('/Users/USER/Desktop/visualization/seg.csv')
by(seg.df$income, seg.df$Segment, mean)
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)
seg.income.mean <- aggregate(seg.df$income, list(seg.df$Segment), mean)
seg.income.mean

seg.df$Segment
table(seg.df$Segment)
seg.income.mean[seg.df$Segment, ]
seg.df$segIncome = seg.income.mean[seg.df$Segment, 2]

aggregate(seg.df$income, list(seg.df$Segment,seg.df$ownHome), mean)
aggregate(income~Segment + ownHome, data=seg.df, mean)

##frequency
with(seg.df, table(Segment, ownHome)) #with to specify data

library(lattice) 
#more than two categories
#histogram is for proportion! 
histogram(~subscribe | Segment, data=seg.df)
histogram(subscribe~Segment  , data=seg.df) #try it: y-axis as subscribe & x axis as segment
histogram(~subscribe | Segment, data=seg.df, type="count",
           layout=c(4,1), col=c("burlywood", "darkolivegreen"))

#yes proportion in each segment--> only one category: segement
prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)
barchart(prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)[2, ],
         xlab="Subscriber proportion by Segment", col="darkolivegreen")
#same idea of barplot(table object for one category)
barplot(prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)[2, ])


##groups: continous data
seg.mean <- aggregate(income~Segment, data=seg.df, mean)
library(lattice)
barchart(income~Segment, data=seg.mean, col="grey")


##statistics
chisq.test(table(seg.df$Segment))
table(seg.df$subscribe,seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))

hist(seg.df$income) 
with(seg.df, hist(income[ownHome=="ownYes"])) 
with(seg.df, hist(income[ownHome=="ownNo"])) 
t.test(income ~ ownHome, data=seg.df)
t.test(income ~ ownHome, data=subset(seg.df, Segment=="Travelers"))

