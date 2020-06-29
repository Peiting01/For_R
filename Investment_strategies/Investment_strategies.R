####Mean-variance portfolios with many assets
rm(list = ls(all = T))

data_1305 <- read.table("/Users/USER/Desktop/Final_project/1305.TW.csv", sep = ",", na.strings = "null", header = T)
data_2353 <- read.table("/Users/USER/Desktop/Final_project/2353.TW.csv", sep = ",", na.strings = "null", header = T)
data_2356 <- read.table("/Users/USER/Desktop/Final_project/2356.TW.csv", sep = ",", na.strings = "null", header = T)
data_2498 <- read.table("/Users/USER/Desktop/Final_project/2498.TW.csv", sep = ",", na.strings = "null", header = T)
data_2511 <- read.table("/Users/USER/Desktop/Final_project/2511.TW.csv", sep = ",", na.strings = "null", header = T)
data_2618 <- read.table("/Users/USER/Desktop/Final_project/2618.TW.csv", sep = ",", na.strings = "null", header = T)
data_2812 <- read.table("/Users/USER/Desktop/Final_project/2812.TW.csv", sep = ",", na.strings = "null", header = T)
data_2884 <- read.table("/Users/USER/Desktop/Final_project/2884.TW.csv", sep = ",", na.strings = "null", header = T)

source("/Users/USER/Desktop/Final_project/function_FDA.R")  

#transform Date to as.Date
data_1305$Date <- as.Date(data_1305[,1])
data_2353$Date <- as.Date(data_2353[,1])
data_2356$Date <- as.Date(data_2356[,1])
data_2498$Date <- as.Date(data_2498[,1])
data_2511$Date <- as.Date(data_2511[,1])
data_2618$Date <- as.Date(data_2618[,1])
data_2812$Date <- as.Date(data_2812[,1])
data_2884$Date <- as.Date(data_2884[,1])


##clean data
##replace NA's with their previous values

data_1305 <- NA_rep(data_1305, 6)
data_2353 <- NA_rep(data_2353, 6)
data_2356 <- NA_rep(data_2356, 6)
data_2498 <- NA_rep(data_2498, 6)
data_2511 <- NA_rep(data_2511, 6)
data_2618 <- NA_rep(data_2618, 6)
data_2812 <- NA_rep(data_2812, 6)
data_2884 <- NA_rep(data_2884, 6)

##check whether there is any NA
sum(is.na(data_1305[,6]))
sum(is.na(data_2353[,6]))
sum(is.na(data_2356[,6]))
sum(is.na(data_2498[,6]))
sum(is.na(data_2511[,6]))
sum(is.na(data_2618[,6]))
sum(is.na(data_2812[,6]))
sum(is.na(data_2884[,6]))


## turn to monthly
library(xts)
xts_1305 = xts(data_1305[,2:ncol(data_1305)], order.by = data_1305$Date)
xts_2353 = xts(data_2353[,2:ncol(data_2353)], order.by = data_2353$Date)
xts_2356 = xts(data_2356[,2:ncol(data_2356)], order.by = data_2356$Date)
xts_2498 = xts(data_2498[,2:ncol(data_2498)], order.by = data_2498$Date)
xts_2511 = xts(data_2511[,2:ncol(data_2511)], order.by = data_2511$Date)
xts_2618 = xts(data_2618[,2:ncol(data_2618)], order.by = data_2618$Date)
xts_2812 = xts(data_2812[,2:ncol(data_2812)], order.by = data_2812$Date)
xts_2884 = xts(data_2884[,2:ncol(data_2884)], order.by = data_2884$Date)

xts_1305m = apply.monthly(xts_1305,mean)
xts_2353m = apply.monthly(xts_2353,mean)
xts_2356m = apply.monthly(xts_2356,mean)
xts_2498m = apply.monthly(xts_2498,mean)
xts_2511m = apply.monthly(xts_2511,mean)
xts_2618m = apply.monthly(xts_2618,mean)
xts_2812m = apply.monthly(xts_2812,mean)
xts_2884m = apply.monthly(xts_2884,mean)

library(quantmod)
##calculate daily returns
xts_1305m$ret<-monthlyReturn(xts_1305m$Adj.Close)
xts_2353m$ret<-monthlyReturn(xts_2353m$Adj.Close)
xts_2356m$ret<-monthlyReturn(xts_2356m$Adj.Close)
xts_2498m$ret<-monthlyReturn(xts_2498m$Adj.Close)
xts_2511m$ret<-monthlyReturn(xts_2511m$Adj.Close)
xts_2618m$ret<-monthlyReturn(xts_2618m$Adj.Close)
xts_2812m$ret<-monthlyReturn(xts_2812m$Adj.Close)
xts_2884m$ret<-monthlyReturn(xts_2884m$Adj.Close)

# combine return into a dataframe
data_ret_full<-data.frame(matrix(0, nrow(xts_1305m), 6))  
data_ret_full[,1] = xts_1305m$ret
data_ret_full[,2] = xts_2353m$ret
data_ret_full[,3] = xts_2356m$ret
data_ret_full[,4] = xts_2498m$ret
data_ret_full[,5] = xts_2511m$ret
data_ret_full[,6] = xts_2618m$ret
data_ret_full[,7] = xts_2812m$ret
data_ret_full[,8] = xts_2884m$ret
colnames(data_ret_full)<-c("x1305","x2353","x2356","x2498","x2511","x2618","x2812","x2884")
data_ret_full = data_ret_full[-1,]
summary(data_ret_full)

##split in to sample and out-of-sample
data_ret = data_ret_full
data_ret_OoS = data_ret_full

summary(data_ret)

# plot return
date_full <- seq.Date(from = as.Date("2010/02/28",format = "%Y/%m/%d"), by = "month", length.out = 119)
rangex = range(c(data_ret$x1305,data_ret$x2353,data_ret$x2356,data_ret$x2498,data_ret$x2511,data_ret$x2618,data_ret$x2812,data_ret$x2884), na.rm = T)
plot(x = date_full, y = data_ret$x1305,
     ylim=rangex,
     main = "Return", 
     xlab = "Date", ylab = "return", type="l",
     lwd = 3, col = 2,
     cex.main = 1.8, cex.lab = 1.5, cex.axis = 1.5)
lines(x = date_full, y = data_ret$x2353, 
      col = 3,lwd = 3)
lines(x = date_full, y = data_ret$x2356, 
      col = 4,lwd = 3)
lines(x = date_full, y = data_ret$x2498, 
      col = 5,lwd = 3)
lines(x = date_full, y = data_ret$x2511, 
      col = 6,lwd = 3)
lines(x = date_full, y = data_ret$x2618, 
      col = 7,lwd = 3)
lines(x = date_full, y = data_ret$x2812, 
      col = 8,lwd = 3)
lines(x = date_full, y = data_ret$x2884, 
      col = 9,lwd = 3)
legend(x= "bottomright", y=0.1, legend = c("1305","2353","2356","2498","2511","2618","2812","2884"),
       col = c(2,3,4,5,6,7,8,9), lwd = c(3,3,3,3,3,3,3,3), cex = 0.55)


# combine cumret into a dataframe
data_cumret<-data.frame(matrix(0, nrow(data_ret), 6))  
data_cumret[,1] = cumprod(data_ret$x1305+1)
data_cumret[,2] = cumprod(data_ret$x2353+1)
data_cumret[,3] = cumprod(data_ret$x2356+1)
data_cumret[,4] = cumprod(data_ret$x2498+1)
data_cumret[,5] = cumprod(data_ret$x2511+1)
data_cumret[,6] = cumprod(data_ret$x2618+1)
data_cumret[,7] = cumprod(data_ret$x2812+1)
data_cumret[,8] = cumprod(data_ret$x2884+1)
colnames(data_cumret)<-c("x1305","x2353","x2356","x2498","x2511","x2618","x2812","x2884")
data_cumret = data_cumret[-1,]
summary(data_cumret)

# plot Cumulative return
date_full <- date_full[-1]
rangex = range(c(data_cumret$x1305,data_cumret$x2353,data_cumret$x2356,data_cumret$x2498,data_cumret$x2511,data_cumret$x2618,data_cumret$x2812,data_cumret$x2884), na.rm = T)
plot(x = date_full, y = data_cumret$x1305,
     ylim=rangex,
     main = "Cumulative return", 
     xlab = "Date", ylab = "return", type="l",
     lwd = 3, col = 2,
     cex.main = 1.8, cex.lab = 1.5, cex.axis = 1.5)
lines(x = date_full, y = data_cumret$x2353, 
      col = 3,lwd = 3)
lines(x = date_full, y = data_cumret$x2356, 
      col = 4,lwd = 3)
lines(x = date_full, y = data_cumret$x2498, 
      col = 5,lwd = 3)
lines(x = date_full, y = data_cumret$x2511, 
      col = 6,lwd = 3)
lines(x = date_full, y = data_cumret$x2618, 
      col = 7,lwd = 3)
lines(x = date_full, y = data_cumret$x2812, 
      col = 8,lwd = 3)
lines(x = date_full, y = data_cumret$x2884, 
      col = 9,lwd = 3)
legend("topleft", legend =  c("x1305","x2353","x2356","x2498","x2511","x2618","x2812","x2884"),
       col = c(2,3,4,5,6,7,8,9), lwd = c(3,3,3,3,3,3,3,3), cex = 0.5)

##summary statistics, no annualization
summary_por<-rbind(apply(data_ret,2, summary),
                     apply(data_ret,2, var),
                     apply(data_ret,2, sd),
                     apply(data_ret,2, my_skewness),
                     apply(data_ret,2, my_kurtosis),
                     apply(data_ret,2, my_acf1)
)

rownames(summary_por)[7:nrow(summary_por)]<-c("Var","Std.","SKewness","Kurtosis","ACF1")
summary_por<-t(round(summary_por,3))
summary_por

##summary.ind49 is a matrix
##make a data.frame object
class(summary_por)
summary_por<-data.frame(summary_por)

#-------------------------------------------------------------------------------

##fix-weighted, 1/N
retFW<-apply(data_ret[, 1:ncol(data_ret)], 1, mean, na.rm = T)   ##note to set na.rm = T
retFW = retFW*100                                                  ##show the returns in percentage    

##buy and hold
##creat a new data frame "rx"
rx<-rbind(0, data_ret[-1, 1:(ncol(data_ret)-1)])                  ##note that remove the first row since it contains NA's
rx<-rx+1                                                    ##calculate gross return of each stock
bh_cumr<-apply(rx, 2, cumprod)                        ##calculate cumulative return of each stock
bh_cumr<-apply(bh_cumr, 1, mean)
##calculate average of the cumulative returns
retbh<-c(NA, bh_cumr[-1]/bh_cumr[-length(bh_cumr)]-1)    ##calculate return of the buy-and-hold portfolio
retbh<-retbh*100                              ##show the returns in percentage
retbh
##calculate mean, variance and covariance of portfolio return and turn into percentage
mean_r<-apply(data_ret[,1:8], 2, mean, na.rm = T)
var_r<-apply(data_ret[,1:8], 2, var, na.rm = T)
cov_r<-cov(data_ret[,1:8])
mean_r
var_r
cov_r
round(mean_r*100, 2)           
round(var_r*10000, 2)         
round(cov_r*10000, 2)

total_value <- sum(xts_1305m[1,5], xts_2353m[1,5], xts_2356m[1,5], xts_2498m[1,5], xts_2511m[1,5], xts_2618m[1,5], xts_2812m[1,5], xts_2884m[1,5])

share_1305 <- as.numeric(xts_1305m[1,5]/total_value)
share_2353 <- as.numeric(xts_2353m[1,5]/total_value)
share_2356 <- as.numeric(xts_2356m[1,5]/total_value)
share_2498 <- as.numeric(xts_2498m[1,5]/total_value)
share_2511 <- as.numeric(xts_2511m[1,5]/total_value)
share_2618 <- as.numeric(xts_2618m[1,5]/total_value)
share_2812 <- as.numeric(xts_2812m[1,5]/total_value)
share_2884 <- as.numeric(xts_2884m[1,5]/total_value)

value_1305 <- as.numeric(share_1305 *  xts_1305m[84,5])
value_2353 <- as.numeric(share_2353 *  xts_2353m[84,5])
value_2356 <- as.numeric(share_2356 *  xts_2356m[84,5])
value_2498 <- as.numeric(share_2498 *  xts_2498m[84,5])
value_2511 <- as.numeric(share_2511 *  xts_2511m[84,5])
value_2618 <- as.numeric(share_2618 *  xts_2618m[84,5])
value_2812 <- as.numeric(share_2812 *  xts_2812m[84,5])
value_2884 <- as.numeric(share_2884 *  xts_2884m[84,5])

total_value <- sum(value_1305, value_2353, value_2356, value_2498, value_2511, value_2618, value_2812, value_2884)

share_1305 <- as.numeric(value_1305/total_value)
share_2353 <- as.numeric(value_2353/total_value)
share_2356 <- as.numeric(value_2356/total_value)
share_2498 <- as.numeric(value_2498/total_value)
share_2511 <- as.numeric(value_2511/total_value)
share_2618 <- as.numeric(value_2618/total_value)
share_2812 <- as.numeric(value_2812/total_value)
share_2884 <- as.numeric(value_2884/total_value)

bh_weight <- c(share_1305, share_2353, share_2356, share_2498, share_2511, share_2618, share_2812, share_2884)
bh_value <- c(value_1305, value_2353, value_2356, value_2498, value_2511, value_2618, value_2812, value_2884)
bh_value

##plot weights of fw and bh
par(mfrow=c(2,1))  
rep(1/ncol(data_ret), times = ncol(data_ret), length.out = NA, each = 1)
plot(rep(1/ncol(data_ret), times = ncol(data_ret), length.out = NA, each = 1), type = "h", xaxt = "n", lwd = 12, col = 2, lend = 1,
     xlab = "Industry", ylab = "Weight",
     main = "Portfolio weights of fixed weight", 
     cex = 1.8, cex.main = 2, cex.lab = 1.8)                    ##note how to set "xaxt"
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = colnames(data_ret)[1:8], 
     las = 2)

plot(bh_weight, type = "h", xaxt = "n", lwd = 12, col = 2, lend = 1,
     xlab = "Industry", ylab = "Weight",
     main = "Portfolio weights of buy and hold", 
     cex = 1.8, cex.main = 2, cex.lab = 1.8)                    ##note how to set "xaxt"
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = colnames(data_ret)[1:8], 
     las = 2)


#-------------------------------------------------------------------------------
##using formula  
##GMVP and tangency portfolio
##This is a good way to set up the range of expected returns
  
rf<-2.5/360                                ##risk-free return in percentage

consx<-ABC_mvp(data_ret)
gmvp_mr<-consx$B/consx$C
gmvp_mr

w_gmvp<-mvp_wx(data_ret, gmvp_mr)          ##solving mvp optimization with target return = gmvp_mrx
w_gmvp1<-gmvp_wx(data_ret)                 ##solving gmvp optimization with function

##look at the first 10 elements in w_gmvp or w_gmvp1
w_gmvp[1:length(data_ret)]
w_gmvp1[1:length(data_ret)]

##tangency portfolio's weight
w_tan<-tan_wx(data_ret, rf)

##tangency portfolio's expected return and standard deviation 
result_tan<-tan_mr_sdx(data_ret, rf)
result_tan$mu_tan               
result_tan$sd_tan

#----------------------------------------------------------------------------------------------
##plot weights of gmvp and tangency portfolio
par(mfrow=c(2,1))  

plot(w_gmvp, type = "h", xaxt = "n", lwd = 12, col = 2, lend = 1,
     xlab = "Industry", ylab = "Weight",
     main = "Portfolio weights of the GMVP", 
     cex = 1.8, cex.main = 2, cex.lab = 1.8)                    ##note how to set "xaxt"
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = colnames(data_ret)[1:8], 
     las = 2)


plot(w_tan, type = "h", xaxt = "n", lwd = 12, col = 2, lend = 1,
     xlab = "Industry", ylab = "Weight",
     main = "Portfolio weights of the tangency portfolio", 
     cex = 1.8, cex.main = 2, cex.lab = 1.8)                    ##note how to set "xaxt"
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = colnames(data_ret)[1:8], 
     las = 2)

#-----------------------------------------------------------------------------------------
##plot MV frontier 
##expected target return and std of mvp
mvp_mr<-seq(-result_tan$mu_tan*1.1, result_tan$mu_tan*1.1,      ##sequence of target expected mvp's returns 
            length = 2000)                                      ##max = tangency portfolio's expected return * 1.1 
##min = minus tangency portfolio's expected return * 1.1
mvp_sd<-mvp_sdx(data_ret, mvp_mr)
mvp_mr
##different ways to calculate std of the gmvp
gmvp_sd<-sqrt(t(w_gmvp)%*%cov(data_ret)%*%w_gmvp)
gmvp_sd1<-sqrt(t(w_gmvp1)%*%cov(data_ret)%*%w_gmvp1)
gmvp_sd2<-mvp_sdx(data_ret, gmvp_mr)

c(gmvp_sd, gmvp_sd1, gmvp_sd2)

##target expected return and std of mvp with the risk-free asset
rf_mvp_mr<-seq(-result_tan$mu_tan*1.1, result_tan$mu_tan*1.1,      ##sequence of expected mvp's returns
               length = 2000)                                      ##max = tangency portfolio's expected return * 1.1
##min = minus tangency portfolio's expected return * 1.1
rf_mvp_sd<-rf_mvp_sdx(data_ret, rf_mvp_mr, rf)

#---------------------------------------------------------------------------------
##plot the efficient frontier
windows(height = 8, width = 10)
par(mfrow=c(1,1))
plot(x = mvp_sd, y = mvp_mr, 
     #xlim = c(0.0, max(rf_mvp_sd)*1.6),
     ylim = c(-result_tan$mu_tan*2, result_tan$mu_tan*1.5),
     xlab = "Portfolio standard deviation (%)", 
     ylab = "Portfolio expected return (%)", 
     main = "Mean-variance frontiers", 
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5,
     pch = 16, col = 2
)


points(x = gmvp_sd, y = gmvp_mr, pch = 17, cex = 2, col = 3)                        ##GMVP
lines(x = rf_mvp_sd, y = rf_mvp_mr, lwd = 4, lty = 2, col = 4)                      ##MV frontier with the risk-free asset
points(x = result_tan$sd_tan, y = result_tan$mu_tan, pch = 19, cex = 2, col = 4)    ##tangency portfolio 

points(x = summary_por$Std., y = summary_por$Mean, pch = 18, cex = 1.5, col = 1)##49-industry portfolios

legend("bottomright", 
       legend = c("MVF without risk-free asset", "MVF with risk-free asset","GMVP","TAN_Por","8-industry portfolios"),
       lty = c(NA,2,NA,NA,NA,NA), lwd = c(NA,3,NA,NA,NA,NA),
       pch = c(16, NA, 17, 19, 18, 18), col = c(2,4,3,4,1)
       ,cex=0.55)
#-----------------------------------------------------------------------------------------------------  
##using quadratic programming
library(quadprog)  

##a simple example
mu_targ<-0.01
rf<-2.5/360
n<-dim(data_ret)[2]                       ##number of assets

result<-mvp_wx_quad(data_ret, mu_targ)
wx<-as.matrix(result$solution)
sum(wx<0)                                  ##check whether there are nonpositive weights

sigma2x<-result$value*2
sigma2x

##verify constraints
t(wx)%*%apply(data_ret, 2, mean)
t(wx)%*%matrix(rep(1, n), n,1)

##compare portfolio standard deviation
sqrt(t(wx)%*%cov(data_ret)%*%wx)
sqrt(sigma2x)
#-------------------------------------------------------------------------------------------
##gmvp
consx<-ABC_mvp(data_ret)
gmvp_mr<-consx$B/consx$C
result3<-mvp_wx_quad(data_ret, gmvp_mr)
gmvp_sd<-sqrt(result3$value*2)
gmvp_sd

##gmvp, using gmvp_wx_quad
result31<-gmvp_wx_quad(data_ret)
gmvp_sd1<-sqrt(result3$value*2)
gmvp_sd1  

##tangency portfollio
tan_mr<-tan_mr_sdx(data_ret, rf)$mu_tan
result4<-mvp_wx_quad(data_ret, tan_mr)
tan_sd<-sqrt(result4$value*2)
tan_sd
tan_mr_sdx(data_ret, rf)$sd_tan 
#-----------------------------------------------------------------------------------------------------
##plot weights of gmvp and tangency portfolio
windows(height = 8, width = 10)
par(mfrow=c(2,1))  

plot(result3$solution, type = "h", xaxt = "n", lwd = 12, col = 2, lend = 1,
     xlab = "Industry", ylab = "Weight",
     main = "Portfolio weights of the GMVP", 
     cex = 1.8, cex.main = 2, cex.lab = 1.8)                    ##note how to set "xaxt"
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = colnames(data_ret), 
     las = 2)

plot(result4$solution, type = "h", xaxt = "n", lwd = 12, col =2, lend = 1,
     xlab = "Industry", ylab = "Weight",
     main = "Portfolio weights of the tangency portfolio", 
     cex = 1.8, cex.main = 2, cex.lab = 1.8)                    ##note how to set "xaxt"
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = colnames(data_ret), 
     las = 2)

#-------------------------------------------------------------------------------------------------------
##generate optimal portfolio weight vector with the vector "mvp_mr"
##verify whether the generated optimal portfolio weight vector the same as before
  
##Without risk-free assets
mvp_mr<-seq(-result_tan$mu_tan*1.1, result_tan$mu_tan*1.1,      ##sequence of expected mvp's returns 
              length = 2000)                                      ##max = tangency portfolio's expected return * 1.1 
##min = minus tangency portfolio's expected return * 1.1 
result1<-NULL

for(i in 1:length(mvp_mr)){
  
  mu_targ<-mvp_mr[i]
  result<-mvp_wx_quad(data_ret, mu_targ)
  result1<-rbind(result1,
                 c(mu_targ, 
                   sqrt(result$value*2)))
  
}  

##With risk-free assets
rf_mvp_mr<-seq(-result_tan$mu_tan*1.1, result_tan$mu_tan*1.1,    ##sequence of expected mvp's returns 
               length = 2000)                                    ##max = tangency portfolio's expected return * 1.1 
##min = minus tangency portfolio's expected return * 1.1 
result2<-NULL

for(i in 1:length(rf_mvp_mr)){
  
  mu_targ<-rf_mvp_mr[i]
  result<-rf_mvp_wx_quad(data_ret, mu_targ, rf)
  result2<-rbind(result2,
                 c(mu_targ, 
                   sqrt(result$value*2)))
  
}  

#---------------------------------------------------------------------------------------------------
##plot MV frontier
windows(height = 8, width = 10)
par(mfrow=c(1,1)) 
plot(x = result1[,2], y = result1[,1], 
     xlim = c(min(result2[,2]), max(result2[,2])*1.6),
     ylim = c(result_tan$mu_tan*1.1, -result_tan$mu_tan*1.1),
     xlab = "Portfolio standard deviation (%)", 
     ylab = "Portfolio expected return (%)", 
     main = "Mean-variance frontiers", 
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5,
     pch = 16, col = 2)

points(x = gmvp_sd, y = gmvp_mr, pch = 17, cex = 2, col = 3)      ##GMVP
lines(x = result2[,2], y=result2[,1], lwd = 3, lty = 2, col = 4)  ##MV frontier with the risk-free asset
points(x = tan_sd, y = tan_mr, pch = 19, cex = 2, col = 4)        ##tangency portfolio 

points(x = summary_por$Std., y = summary_por$Mean, pch = 18, cex = 1.5, col = 1)##49-industry portfolios

legend("bottomright",
       legend = c("MVP without risk-free asset","MVP with risk-free asset","GMVP","TAN_Por","8-industry portfolios"),
       lty = c(NA,2,NA,NA,NA,NA), lwd = c(NA,3,NA,NA,NA,NA),
       pch = c(16, NA, 17, 19, 18, 18), col = c(2,4,3,4,1),cex=0.55)




#--------------------------------------------------------------------------------------------------
##No-shortsale portfolio

mu_targ<-0.01                              ##note that in this case, mu_targ should be carefully set
rf<-2.5/360
nx<-dim(data_ret)[2]                       ##number of assets

result<-nsmvp_wx_quad(data_ret, mu_targ)
result
wx<-as.matrix(result$solution)
wx
wx<-round(wx,8)                            ##round 
wx
sum(wx<0)                                  ##check whether there are nonpositive weights
sum(wx==0);sum(wx>0)
#---------------------------------------------------------------------------------------------
##plot nsmvp portfolio weights
windows(height = 8, width = 10)

plot(wx, type = "h", xaxt = "n", lwd = 12, col =2, lend = 1,
     xlab = "Industry", ylab = "Weight",
     main = "Portfolio weights of the NSMVP", 
     cex = 1.8, cex.main = 2, cex.lab = 1.8)                    ##note how to set "xaxt"
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = colnames(data_ret), 
     las = 2)

#-------------------------------------------------------------------------------------
##portfolio variance
sigma2x<-result$value*2
sigma2x

##verify constraints
t(wx)%*%apply(data_ret,2,mean)
mu_targ
t(wx)%*%matrix(rep(1,nx),nx,1)

sqrt(t(wx)%*%cov(data_ret)%*%wx)
sqrt(sigma2x)
#---------------------------------------------------------------------------------------------
##mean return vector
mean_r<-apply(data_ret, 2, mean)
rf<-2.5/360

##Without risk-free assets
##sequence of expected mvp's returns
##note that here it must be between min. and max. of mean_r, 
##otherwise the constraints will be inconsistent
mvp_mr<-seq(min(mean_r)*0.99,                   
            max(mean_r)*0.99, length = 2000)      

result11<-NULL

for(i in 1:length(mvp_mr)){
  
  mu_targ<-mvp_mr[i]
  result<-nsmvp_wx_quad(data_ret, mu_targ)
  result11<-rbind(result11,
                  c(mu_targ, 
                    sqrt(result$value*2)))
  
}  

##With risk-free assets
##sequence of expected mvp's returns
##note that here there is no special restriction
rf_mvp_mr<-seq(-result_tan$mu_tan*1.1, result_tan$mu_tan*1.1, length = 2000)        

result12<-NULL

for(i in 1:length(rf_mvp_mr)){
  
  mu_targ<-rf_mvp_mr[i]
  result<-rf_nsmvp_wx_quad(data_ret, mu_targ, rf)
  result12<-rbind(result12,
                  c(mu_targ, 
                    sqrt(result$value*2)))
  
}  
#--------------------------------------------------------------------------------
##no-shortsale gmvp (nsgmvp): directly calculate its variance with its weights
nsgmvp_w<-nsgmvp_wx_quad(data_ret)$solution
nsgmvp_w<-round(nsgmvp_w, 8)

nsgmvp_mr<-t(nsgmvp_w)%*%mean_r
nsgmvp_sd<-sqrt(t(nsgmvp_w)%*%cov(data_ret)%*%nsgmvp_w)

nsgmvp_mr                               ##it is different from B/C!!
nsgmvp_sd    


##Tangency portfollio of nsmvp: find the maximum SR
nsmvp_sr<-(result11[,1]-rf)/result11[,2]
max(nsmvp_sr)
indx1<-which(nsmvp_sr==max(nsmvp_sr))
ns_tan_sd<-result11[indx1,2]
ns_tan_mr<-result11[indx1,1]

##calculate slope of the efficient frontier
##should be the same as SR
gg<-result12[nrow(result12),]-result12[nrow(result12)-1,]       
gg[1]/gg[2]                                                     

##calculate slope of the inefficient frontier (asymmetric)
gg<-result12[2,]-result12[1,]                                    
gg[1]/gg[2]                                                     

#----------------------------------------------------------------------------------------
##plot frontier of nsmvp
windows(height = 8, width = 10)
plot(x = result11[,2], y = result11[,1], 
     xlim = c(0, max(result12[,2])),
     ylim = c(-result_tan$mu_tan*1.1, result_tan$mu_tan*1.1),
     xlab = "Portfolio standard deviation (%)", 
     ylab = "Portfolio expected return (%)", 
     main = "Mean-variance frontiers", 
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 2,
     pch = 16, col = 2)

lines(x = result12[,2], y=result12[,1], lwd = 3, lty = 2, col = 4)    ##MV frontier with the risk-free asset
points(x = nsgmvp_sd, y = nsgmvp_mr, pch = 17, cex = 1.5, col = 3)      ##nsgmvp
points(x = ns_tan_sd, y = ns_tan_mr, pch = 19, cex = 1.5, col = 4)      ##nstan  
#--------------------------------------------------------------------------------------
##add frontier of mvp to the figure 
points(x = result1[,2], y = result1[,1], lwd = 0.1, col = 5, cex = 1)
lines(x = result2[,2], y = result2[,1], lwd = 3, lty = 3, col = 6)
points(x = gmvp_sd, y = gmvp_mr, pch = 17, cex = 1, col = 7)          ##add GMVP
points(x = tan_sd, y = tan_mr, pch = 19, cex = 2, col = 8)            ##tangency portfolio  

points(x = summary_por$Std., y = summary_por$Mean, pch = 18, cex = 1, col = 1)##49-industry portfolios

legend("bottomright", legend = c("MVP without risk-free asset",
                              "MVP with risk-free asset",
                              "NSMVP without risk-free asset",
                              "NSMVP with risk-free asset",
                              "NS-GMVP","GMVP","NS-TAN_Por","TAN_Por",
                              "8-industry portfolios"),
       col = c(5,6,2,4,3,7,4,8,1), 
       lty=c(1,3,1,3,NA,NA,NA,NA,NA), 
       lwd = c(2,2,1,1,NA,NA,NA,NA,NA),
       pch = c(NA,NA,NA,NA,17,17,19,19,18),cex=0.55)

#--------------------------------------------------------------------------------------------
##calculate component contribution
##wihtout the risk-free asset  
##with mu_targ = 0.08%, rf = 2.5/360


##obtain portfolio weights  
w_gmvp<-gmvp_wx(data_ret)                             ##gmvp
w_mvp<-mvp_wx(data_ret, mu_targ = 0.08)               ##mvp
w_tan1<-tan_wx(data_ret, rf = rf)                     ##tangency portfolio
w_nsmvp<-nsmvp_wx_quad(data_ret, mu_targ = 0.01)      ##no-shortsale mvp  
w_fw<-rep(1/(ncol(data_ret)), times = ncol(data_ret), length.out = NA, each = 1) ##fixed-weight

##sample covariance matrix
covx<-cov(data_ret)

##result
result_gmvp<-por_ccx(covx, w_gmvp)
result_mvp<-por_ccx(covx, w_mvp)
result_tan1<-por_ccx(covx, w_tan1)
result_nsmvp<-por_ccx(covx, w_nsmvp$solution)

##collect the results
result<-data.frame(cbind(result_gmvp$percentage_contribution,
                         result_mvp$percentage_contribution,
                         result_tan1$percentage_contribution,
                         result_nsmvp$percentage_contribution))

rownames(result)<-names(data_ret)

##plot
windows(width = 10, height = 8)
par(mfrow=c(2,3))
##gmvp
plot(result[,1], type = "h", xaxt = "n", lwd = 10, col = 4, lend = 1,
     xlab = "Industry", ylab = "Percentage Contribution (%)",
     main = "GMVP", 
     cex = 1.2, cex.main = 2, cex.lab = 1.5)                    
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = names(data_ret), 
     las = 2, cex.axis = 0.8)

##mvp
plot(result[,2], type = "h", xaxt = "n", lwd = 10, col = 4, lend = 1,
     xlab = "Industry", ylab = "Percentage Contribution (%)",
     main = "MVP", 
     cex = 1.2, cex.main = 2, cex.lab = 1.5)                    
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = names(data_ret), 
     las = 2, cex.axis = 0.8)

##tangency portfolio
plot(result[,3], type = "h", xaxt = "n", lwd = 12, col = 4, lend = 1,
     xlab = "Industry", ylab = "Percentage Contribution (%)",
     main = "Tangency Portfolio", 
     cex = 1.2, cex.main = 2, cex.lab = 1.5)                    
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = names(data_ret), 
     las = 2, cex.axis = 0.8)

##no-shortsale mvp
plot(result[,4], type = "h", xaxt = "n", lwd = 12, col = 4, lend = 1,
     xlab = "Industry", ylab = "Percentage Contribution (%)",
     main = "NSMVP", 
     cex = 1.2, cex.main = 2, cex.lab = 1.5)                    
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = names(data_ret), 
     las = 2, cex.axis = 0.8)
## fw
plot(w_fw*100,
     type = "h", xaxt = "n", lwd = 12, col = 4, lend = 1,
     xlab = "Industry", ylab = "Percentage Contribution (%)",
     main = "Fixed weight", 
     cex = 1.2, cex.main = 2, cex.lab = 1.5)                    
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = names(data_ret), 
     las = 2, cex.axis = 0.8)

## bh
plot(bh_weight*100,
     type = "h", xaxt = "n", lwd = 12, col = 4, lend = 1,
     xlab = "Industry", ylab = "Percentage Contribution (%)",
     main = "Buy and Hold", 
     cex = 1.2, cex.main = 2, cex.lab = 1.5)                    
abline(0,0,lty=2)
axis(side = 1, at = c(1:8), 
     labels = names(data_ret), 
     las = 2, cex.axis = 0.8)

result
#--------------------------------------------------------------------------------------------
#Out of Sample
library(quadprog)

data_ret_full[, ncol(data_ret_full)]<-data_ret_full[, ncol(data_ret_full)]/100 ##scale by 1/100, not in percentage

kx<- 24                                       ##window length
hx<-nrow(data_ret_full)-kx                     ##length of out-of-sample period
kx
hx
##portfolio weights, starting from period t-1
wx_mat_gmvp<-matrix(0, hx+1, ncol(data_ret_full))
wx_mat_mvp<-matrix(0, hx+1, ncol(data_ret_full))
wx_mat_tan<-matrix(0, hx+1, ncol(data_ret_full))
wx_mat_nsmvp<-matrix(0, hx+1, ncol(data_ret_full))
wx_mat_fw<-matrix(0, hx+1, ncol(data_ret_full))
wx_mat_bh<-matrix(0, hx+1, ncol(data_ret_full))

##portfolio net return
por_netrx_gmvp<-numeric(hx)
por_netrx_mvp<-numeric(hx)
por_netrx_tan<-numeric(hx)
por_netrx_nsmvp<-numeric(hx)
por_netrx_fw<-numeric(hx)
por_netrx_bh<-numeric(hx)

##turnover rate
tor_gmvp<-numeric(hx)
tor_mvp<-numeric(hx)
tor_tan<-numeric(hx)
tor_nsmvp<-numeric(hx)
tor_fw<-numeric(hx)
tor_bh<-numeric(hx)

##HHI
hhi_gmvp<-numeric(hx)
hhi_mvp<-numeric(hx)
hhi_tan<-numeric(hx)
hhi_nsmvp<-numeric(hx)
hhi_fw<-numeric(hx)
hhi_bh<-numeric(hx)

##SLR
slr_gmvp<-numeric(hx)
slr_mvp<-numeric(hx)
slr_tan<-numeric(hx)
slr_nsmvp<-numeric(hx)
slr_fw<-numeric(hx)
slr_bh<-numeric(hx)

##fix-weighted, 1/N
retFW<-apply(data_ret_full[, 1:ncol(data_ret_full)], 1, mean, na.rm = T)   ##note to set na.rm = T
retFW = retFW*100                                                  ##show the returns in percentage 
retFW_cumr = retFW
for (i in c(2:length(retFW))) {
  retFW_cumr[i] = retFW[i] + retFW[i-1]
}

##buy and hold
##creat a new data frame "rx"
rx<-rbind(0, data_ret_full[-1, 1:(ncol(data_ret_full)-1)])                  ##note that remove the first row since it contains NA's
rx<-rx+1                                                    ##calculate gross return of each stock
bh_cumr<-apply(rx, 2, cumprod)                        ##calculate cumulative return of each stock
bh_cumr<-apply(bh_cumr, 1, mean)
##calculate average of the cumulative returns
retbh<-c(NA, bh_cumr[-1]/bh_cumr[-length(bh_cumr)]-1)    ##calculate return of the buy-and-hold portfolio
retbh<-retbh*100                              ##show the returns in percentage
retbh
##calculate mean, variance and covariance of portfolio return and turn into percentage
mean_r<-apply(data_ret_full[,1:8], 2, mean, na.rm = T)
var_r<-apply(data_ret_full[,1:8], 2, var, na.rm = T)
cov_r<-cov(data_ret_full[,1:8])
mean_r
var_r
cov_r
round(mean_r*100, 2)           
round(var_r*10000, 2)         
round(cov_r*10000, 2)

total_value <- sum(xts_1305m[85,5], xts_2353m[85,5], xts_2356m[85,5], xts_2498m[85,5], xts_2511m[85,5], xts_2618m[85,5], xts_2812m[85,5], xts_2884m[85,5])

share_1305 <- as.numeric(xts_1305m[85,5]/total_value)
share_2353 <- as.numeric(xts_2353m[85,5]/total_value)
share_2356 <- as.numeric(xts_2356m[85,5]/total_value)
share_2498 <- as.numeric(xts_2498m[85,5]/total_value)
share_2511 <- as.numeric(xts_2511m[85,5]/total_value)
share_2618 <- as.numeric(xts_2618m[85,5]/total_value)
share_2812 <- as.numeric(xts_2812m[85,5]/total_value)
share_2884 <- as.numeric(xts_2884m[85,5]/total_value)

value_1305 <- as.numeric(share_1305 *  xts_1305m[120,5])
value_2353 <- as.numeric(share_2353 *  xts_2353m[120,5])
value_2356 <- as.numeric(share_2356 *  xts_2356m[120,5])
value_2498 <- as.numeric(share_2498 *  xts_2498m[120,5])
value_2511 <- as.numeric(share_2511 *  xts_2511m[120,5])
value_2618 <- as.numeric(share_2618 *  xts_2618m[120,5])
value_2812 <- as.numeric(share_2812 *  xts_2812m[120,5])
value_2884 <- as.numeric(share_2884 *  xts_2884m[120,5])

total_value <- sum(value_1305, value_2353, value_2356, value_2498, value_2511, value_2618, value_2812, value_2884)

share_1305 <- as.numeric(value_1305/total_value)
share_2353 <- as.numeric(value_2353/total_value)
share_2356 <- as.numeric(value_2356/total_value)
share_2498 <- as.numeric(value_2498/total_value)
share_2511 <- as.numeric(value_2511/total_value)
share_2618 <- as.numeric(value_2618/total_value)
share_2812 <- as.numeric(value_2812/total_value)
share_2884 <- as.numeric(value_2884/total_value)

bh_weight <- c(share_1305, share_2353, share_2356, share_2498, share_2511, share_2618, share_2812, share_2884)

##transaction cost
epx<-3.5/1000                                                  ##transaction cost

rf<-2.5/360                                                    ##risk-free return in percentage

mu_targ<- 0.01/100                                           ##daily return, 0.01%
wx_gmvp
w_fw<-rep(1/(ncol(data_ret_full)), times = ncol(data_ret_full), length.out = NA, each = 1) ##fixed-weight
for(i in 1:hx){
  
  datax<-data_ret_full[i:(i+kx-1),]             ##data in the window (rolling window)
  datax
  wx_gmvp<-as.vector(gmvp_wx(datax))                                ##gmvp
  wx_mvp<-as.vector(mvp_wx(datax, mu_targ = mu_targ))               ##mvp
  wx_tan<-as.vector(tan_wx(datax, rf = rf))                         ##tangency porfolio
  wx_nsmvp<-nsmvp_wx_quad(datax, mu_targ = mu_targ)$solution        ##nsmvp
  wx_nsmvp<-round(wx_nsmvp,8)
  wx_fw <- w_fw
  wx_bh <- bh_weight
  
  rx<-data_ret_full[i+kx,]                       ##return at period i+kx (period t+1), scaled by 1/100
  rx_lag<-datax[kx,]                                            ##return at period i+kx-1 (period t)

  ##individual assets' turnover over rate
  tor_ind_gmvp<-wx_gmvp-wx_mat_gmvp[i,]*(1+rx_lag)/(1+sum(wx_mat_gmvp[i,]*rx_lag))
  tor_ind_mvp<-wx_mvp-wx_mat_mvp[i,]*(1+rx_lag)/(1+sum(wx_mat_mvp[i,]*rx_lag))
  tor_ind_tan<-wx_mvp-wx_mat_tan[i,]*(1+rx_lag)/(1+sum(wx_mat_tan[i,]*rx_lag))
  tor_ind_nsmvp<-wx_nsmvp-wx_mat_nsmvp[i,]*(1+rx_lag)/(1+sum(wx_mat_nsmvp[i,]*rx_lag))
  tor_ind_fx<-wx_mvp-wx_mat_fw[i,]*(1+rx_lag)/(1+sum(wx_mat_fw[i,]*rx_lag))
  tor_ind_bh<-wx_nsmvp-wx_mat_bh[i,]*(1+rx_lag)/(1+sum(wx_mat_bh[i,]*rx_lag))
  
  ##portfolio turn over rate
  tor_gmvp[i]<-sum(abs(tor_ind_gmvp))
  tor_mvp[i]<-sum(abs(tor_ind_mvp))
  tor_tan[i]<-sum(abs(tor_ind_tan))
  tor_nsmvp[i]<-sum(abs(tor_ind_nsmvp))
  tor_fw[i]<-sum(abs(tor_ind_fx))
  tor_bh[i]<-sum(abs(tor_ind_bh))
  
  ##portfolio net return
  por_netrx_gmvp[i]<-(1+sum(wx_gmvp*rx))*(1-epx*tor_gmvp[i])-1
  por_netrx_mvp[i]<-(1+sum(wx_mvp*rx))*(1-epx*tor_mvp[i])-1
  por_netrx_tan[i]<-(1+sum(wx_tan*rx))*(1-epx*tor_tan[i])-1
  por_netrx_nsmvp[i]<-(1+sum(wx_nsmvp*rx))*(1-epx*tor_nsmvp[i])-1
  por_netrx_fw[i]<-(1+sum(wx_fw*rx))*(1-epx*tor_fw[i])-1
  por_netrx_bh[i]<-(1+sum(wx_bh*rx))*(1-epx*tor_bh[i])-1
  
  ##HHI
  hhi_gmvp[i]<-sum(wx_gmvp^2)/(sum(abs(wx_gmvp))^2)
  hhi_mvp[i]<-sum(wx_mvp^2)/(sum(abs(wx_mvp))^2)
  hhi_tan[i]<-sum(wx_tan^2)/(sum(abs(wx_tan))^2)
  hhi_nsmvp[i]<-sum(wx_nsmvp^2)/(sum(abs(wx_nsmvp))^2)
  hhi_fw[i]<-sum(wx_fw^2)/(sum(abs(wx_fw))^2)
  hhi_bh[i]<-sum(wx_bh^2)/(sum(abs(wx_bh))^2)
  
  ##SLR
  slr_gmvp[i]<-sum(abs(wx_gmvp[wx_gmvp<0]))/sum(abs(wx_gmvp[wx_gmvp>0]))
  slr_mvp[i]<-sum(abs(wx_mvp[wx_mvp<0]))/sum(abs(wx_mvp[wx_mvp>0]))
  slr_tan[i]<-sum(abs(wx_tan[wx_tan<0]))/sum(abs(wx_tan[wx_tan>0]))
  slr_nsmvp[i]<-sum(abs(wx_nsmvp[wx_nsmvp<0]))/sum(abs(wx_nsmvp[wx_nsmvp>0]))
  slr_fw[i]<-sum(abs(wx_fw[wx_fw<0]))/sum(abs(wx_fw[wx_fw>0]))
  slr_bh[i]<-sum(abs(wx_bh[wx_bh<0]))/sum(abs(wx_bh[wx_bh>0]))
  
  
  ##store portfolio weight vector at this period
  wx_mat_gmvp[i+1,]<-wx_gmvp
  wx_mat_mvp[i+1,]<-wx_mvp
  wx_mat_tan[i+1,]<-wx_tan
  wx_mat_nsmvp[i+1,]<-wx_nsmvp
  wx_mat_fw[i+1,]<-wx_fw
  wx_mat_bh[i+1,]<-wx_bh
  
  print(i) 
  
}  

##some summary statistics for net portfolio returns
c(summary(por_netrx_gmvp), sd(por_netrx_gmvp))
c(summary(por_netrx_mvp), sd(por_netrx_mvp))
c(summary(por_netrx_tan), sd(por_netrx_tan))
c(summary(por_netrx_nsmvp), sd(por_netrx_nsmvp))
c(summary(por_netrx_fw), sd(por_netrx_fw))
c(summary(por_netrx_bh), sd(por_netrx_bh))

##Annualized Sharpe ratio, annualized interest rate 1%
rfx<-0.01/12
(mean(por_netrx_gmvp)-rfx)/(sd(por_netrx_gmvp))*sqrt((12))
(mean(por_netrx_mvp)-rfx)/(sd(por_netrx_mvp))*sqrt((12))
(mean(por_netrx_tan)-rfx)/(sd(por_netrx_tan))*sqrt((12))
(mean(por_netrx_nsmvp)-rfx)/(sd(por_netrx_nsmvp))*sqrt((12))
(mean(por_netrx_fw)-rfx)/(sd(por_netrx_fw))*sqrt((12))
(mean(por_netrx_bh)-rfx)/(sd(por_netrx_bh))*sqrt((12))

##tunrover rate
c(summary(tor_gmvp),sd(tor_gmvp))
c(summary(tor_mvp),sd(tor_mvp))
c(summary(tor_tan),sd(tor_tan))
c(summary(tor_nsmvp),sd(tor_nsmvp))
c(summary(tor_fw),sd(tor_fw))
c(summary(tor_bh),sd(tor_bh))

##HHI index
c(summary(hhi_gmvp),sd(hhi_gmvp))
c(summary(hhi_mvp),sd(hhi_mvp))
c(summary(hhi_tan),sd(hhi_tan))
c(summary(hhi_nsmvp),sd(hhi_nsmvp))
c(summary(hhi_fw),sd(hhi_fw))
c(summary(hhi_bh),sd(hhi_bh))

##SLR
c(summary(slr_gmvp),sd(slr_gmvp))
c(summary(slr_mvp),sd(slr_mvp))
c(summary(slr_tan),sd(slr_tan))
c(summary(slr_nsmvp),sd(slr_nsmvp))
c(summary(slr_fw),sd(slr_fw))
c(summary(slr_bh),sd(slr_bh))

##plot returns
windows(width = 8, height = 10)
par(mfrow = c(2,3))

date_OoS <- seq.Date(from = as.Date("2012/12/31",format = "%Y/%m/%d"), by = "month", length.out = 95)

##gmvp
plot(x = date_OoS,
     y = por_netrx_gmvp, type = "l", lwd =1.5,
     main = "Return (gmvp)",
     xlab = "Month", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##mvp
plot(x = date_OoS,
     y = por_netrx_mvp, type = "l", lwd =1.5,
     main = "Return (mvp)",
     xlab = "Month", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##tangency porfolio
plot(x = date_OoS,
     y = por_netrx_tan, type = "l", lwd =1.5,
     main = "Return (tan)",
     xlab = "Month", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##nsmvp
plot(x = date_OoS,
     y = por_netrx_nsmvp, type = "l", lwd =1.5,
     main = "Return (nsmvp)",
     xlab = "Month", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##fixed-weight
plot(x = date_OoS,
     y = por_netrx_fw, type = "l", lwd =1.5,
     main = "Return (fw)",
     xlab = "Month", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##bh
plot(x = date_OoS,
     y = por_netrx_bh, type = "l", lwd =1.5,
     main = "Return (bh)",
     xlab = "Month", ylab = "Return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)



##plot cumulative returns
windows(width = 8, height = 10)
par(mfrow = c(2,3))

date_OoS <- seq.Date(from = as.Date("2012/12/31",format = "%Y/%m/%d"), by = "month", length.out = 95)

##gmvp
cumr_gmvp<-cumprod(1+por_netrx_gmvp) 
length(cumr_gmvp)
plot(x = date_OoS,
     y = cumr_gmvp, type = "l", lwd =1.5,
     main = "Cumulative net return (gmvp)",
     xlab = "Month", ylab = "Cumulative return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##mvp
cumr_mvp<-cumprod(1+por_netrx_mvp) 
plot(x = date_OoS,
     y = cumr_mvp, type = "l", lwd =1.5,
     main = "Cumulative net return (mvp)",
     xlab = "Month", ylab = "Cumulative return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##tangency porfolio
cumr_tan<-cumprod(1+por_netrx_tan) 
plot(x = date_OoS,
     y = cumr_tan, type = "l", lwd =1.5,
     main = "Cumulative net return (tan)",
     xlab = "Month", ylab = "Cumulative return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##nsmvp
cumr_nsmvp<-cumprod(1+por_netrx_nsmvp)                                   
plot(x = date_OoS,
     y = cumr_nsmvp, type = "l", lwd =1.5,
     main = "Cumulative net return (nsmvp)",
     xlab = "Month", ylab = "Cumulative return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##fixed-weight
retFW_cumr
plot(x = date_OoS,
     y = retFW_cumr[26:120], type = "l", lwd =1.5,
     main = "Cumulative net return (fw)",
     xlab = "Month", ylab = "Cumulative return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

##bh
plot(x = date_OoS,
     y = bh_cumr[26:120], type = "l", lwd =1.5,
     main = "Cumulative net return (bh)",
     xlab = "Month", ylab = "Cumulative return",
     cex.lab = 1.8, cex.axis = 1.5, cex.main = 1.8,
)

