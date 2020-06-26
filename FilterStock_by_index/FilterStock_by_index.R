# 產生績效指標套件
library(PerformanceAnalytics)
# 轉換長寬資料套件
library(tidyr)
# 設定時間序列套件
library(timeSeries)
# 資料整理好用套件
library(dplyr)
# 轉換日期格式套件
library(lubridate)
# 最適化套件
library(fPortfolio)


filepath<-"/Users/USER/Desktop/FilterStock_by_index" 
setwd(filepath)
load("FinancialData1.RData")

AdjStockPrice<-FinancialData1$AdjStockPrice     
FinancialReport<-FinancialData1$FinancialReport

# 初篩指標四分位數
quantile(FinancialReport$net_sales)
quantile(FinancialReport$roe_after_tax)
quantile(FinancialReport$current_ratio)
quantile(FinancialReport$eps)

#################投資策略(一)####################
# 回測開始日
StartDate<-20180901
# 回測結束日
EndDate<-20191231
# 投組股票檔數
PortfolioNum<-5
# 再平衡日期
rebalanceDays<-10
# insample data 天數
insampledays<-500

AdjStockPrice<-FinancialData1$AdjStockPrice     
FinancialReport<-FinancialData1$FinancialReport
## 資料整理
# 取出所有交易日
tradeDate<-AdjStockPrice %>%pull(date) %>%unique()

# 產生再平衡日期
rebalanceDate<-tradeDate[which((tradeDate>=StartDate)&(tradeDate<=EndDate))] %>% .[seq(1,length(.),rebalanceDays)]

# 產生股價日報酬
StockRetData<-AdjStockPrice               %>%
  select(code,date,close)                 %>% 
  arrange(code,date)                      %>% 
  group_by(code)                          %>% 
  mutate(ret=(close/dplyr::lag(close))-1) %>% 
  na.omit()                               %>% 
  select(code,date,ret) 

## 建立儲存表
holdInfoTable <- tibble(portfolioName = character(), code = character(), inDate = numeric(), 
                        inWeight = numeric(),outDate = numeric(),periodRet=numeric())
historyHoldInfoTable <- NULL
ix<-1

## 初篩
FinDate<-FinancialReport %>% 
  dplyr::filter(usedate<=rebalanceDate[ix]) %>% 
  pull(usedate) %>%                             
  unique() %>%                                  
  max()                                         


FinSelectCode<-FinancialReport %>%
  dplyr::filter(usedate==FinDate) %>% 
  select(code,usedate,net_sales,roe_after_tax,current_ratio,eps) %>%      
  dplyr::filter(net_sales>2500000) %>% 
  dplyr::filter(roe_after_tax>0.1) %>%          
  dplyr::filter(current_ratio>200) %>%
  dplyr::filter(eps>1) %>%
  pull(code)

FinSelectCode
length(FinSelectCode)


# 取出insample data 日期
insampledate<-tradeDate[(which(tradeDate==rebalanceDate[ix])-insampledays):(which(tradeDate==rebalanceDate[ix])-1)]

# 取出insample日期間的股價日報酬，並轉成寬資料
iStockRetData <- StockRetData %>% 
  dplyr::filter(date%in%insampledate,code%in%FinSelectCode)%>% 
  select(code, date, ret) %>%                                   
  group_by(code) %>%                                            
  dplyr::filter(n() >= (insampledays*0.7)) %>%                  
  mutate(date = ymd(date)) %>%                                  
  spread(key = "code", value = "ret") %>%                       
  select(date, everything())

iStockRetData <- xts(iStockRetData[, -1], order.by = iStockRetData$date)  

# 取出股票代碼
iStockCode<-colnames(iStockRetData)
iStockCode
length(iStockCode) 

##再篩
criterioData<-SharpeRatio(iStockRetData,FUN = "StdDev") %>%                 
  as.numeric()  %>%                                            
  data.frame(code=iStockCode,criterio=.) %>%                   
  mutate(code=as.character(code),criterio=as.numeric(criterio))

criterioSelectCode<-criterioData               %>%                        
  arrange(desc(criterio))    %>%                         
  dplyr::filter(row_number()<=PortfolioNum) %>%     # 取出排名前5檔支股票
  pull(code)                                             

criterioSelectCode
length(criterioSelectCode) # 10檔


## 最適化權重
iWeightRetData<-iStockRetData[,which(colnames(iStockRetData)%in%criterioSelectCode)] %>% as.timeSeries()                                                          # 轉換成時間序列格式
# 帶入fPortfolio套件參數
TangencySpec <- portfolioSpec()                   

#限制每股至少要投資0
atleastpercent<-0                                         
boxConstraints <- c("minW[1:ncol(iWeightRetData)]=atleastpercent")                     # 要代入函數裡的參數


#樣本估計
tgPortfolio <- tangencyPortfolio(data = iWeightRetData, spec = TangencySpec, constraints = boxConstraints) # 代入函數產生最適化權重
tgWeight<- getWeights(tgPortfolio@portfolio)                                            # 只取出權重部分
assetWeight<-tgWeight  
tgWeight

historyHoldInfoTable <- historyHoldInfoTable %>% bind_rows(holdInfoTable)                     # 每期持有資訊併入該變數

# 建立新持有部位表
holdInfoTable <- tibble(portfolioName = character(), code = character(), inDate = numeric(),  # 儲存後就將前期的資訊清空
                        inWeight = numeric(),outDate = numeric(),periodRet=numeric())


holdInfoTable <- holdInfoTable %>%                                                              # 將前述挑選結果進行儲存
  bind_rows(tibble(portfolioName = "測試投資策略", 
                   code = colnames(iWeightRetData), 
                   inDate = rebalanceDate[ix], 
                   inWeight = assetWeight, 
                   outDate=ifelse(ix<length(rebalanceDate),rebalanceDate[ix+1],EndDate)))%>%
  
  left_join(AdjStockPrice%>%
              select(code,date,inclose=close),by=c("code"="code","inDate"="date"))   %>%        # 併入買進時調整後收盤價
  left_join(AdjStockPrice %>%         
              select(code,date,outclose=close),by=c("code"="code","outDate"="date")) %>%        # 併入賣出時調整後收盤價
  mutate(periodRet=(outclose/inclose)-1) %>%                                                  # 計算各股期間報酬
  select(portfolioName,code,inDate,inWeight,outDate,periodRet)                                # 挑選所需欄位

# 建立儲存表
holdInfoTable <- tibble(portfolioName = character(), code = character(), inDate = numeric(),
                        inWeight = numeric(),outDate = numeric(),periodRet=numeric())

historyHoldInfoTable <- NULL


ix<-1

for(ix in 1:length(rebalanceDate)){
  
  cat(paste0("目前正在回測日期: ", rebalanceDate[ix], "  進度:", ix, " / ", length(rebalanceDate),"\n"))
  
  ################# 財報選股(初篩) #############################
  
  # 先挑好過去最接近再平衡日的日期
  
  FinDate<-FinancialReport %>% 
    dplyr::filter(usedate<=rebalanceDate[ix]) %>% # 取所有比rebalance date還早的資料
    pull(usedate) %>%                             # 只取出usedate的部分(用pull可跳脫data frame的框架)
    unique() %>%                                  # 去除重複資料
    max()                                         # 只取最靠近rebalance date的那一天
  
  FinSelectCode<-FinancialReport %>%
    dplyr::filter(usedate==FinDate) %>% 
    select(code,usedate,net_sales,roe_after_tax,current_ratio,eps) %>%      
    dplyr::filter(net_sales>2500000) %>% 
    dplyr::filter(roe_after_tax>0.1) %>%          
    dplyr::filter(current_ratio>200) %>%
    dplyr::filter(eps>1) %>%
    pull(code)
  ################# 績效指標選股(再篩) ########################
  
  
  # 取出insample data 日期
  insampledate<-tradeDate[(which(tradeDate==rebalanceDate[ix])-insampledays):(which(tradeDate==rebalanceDate[ix])-1)]
  
  # 取出insample日期間的股價日報酬，並轉成寬資料
  iStockRetData <- StockRetData %>% 
    dplyr::filter(date%in%insampledate,code%in%FinSelectCode) %>% # 挑出樣本內日期之資料，並只取財報篩選過的股票代碼
    select(code, date, ret) %>%                                   # 取出需要欄位
    group_by(code) %>%                                            # 以股票代碼為單位進行計算
    dplyr::filter(n() >= (insampledays*0.7)) %>%                  # 限制至少要有7成日數
    mutate(date = ymd(date)) %>%                                  # 將日期格式進行改變(ex:20190101 => 2019-01-01)
    spread(key = "code", value = "ret") %>%                       # 將長資料以code展開變成寬資料
    select(date, everything())
  
  # 將資料轉成時間序列
  iStockRetData <- xts(iStockRetData[, -1], order.by = iStockRetData$date)            # 需要將資料轉變成時間序列格式，並依照日期排序
  
  # 取出股票代碼，以便後續整理
  iStockCode<-colnames(iStockRetData)
  
  
  criterioData<-SharpeRatio(iStockRetData,FUN = "StdDev") %>%                 # 使用此函數產生shape ratio數值(input的iStockRetData須為時間序列格式)
    as.numeric()  %>%                                            # 將output的東西轉變成數值格式
    data.frame(code=iStockCode,criterio=.) %>%                   # 將股票代碼及數值進行合併
    mutate(code=as.character(code),criterio=as.numeric(criterio))# 轉換票代碼及績效指標格式
  
  criterioSelectCode<-criterioData               %>%                        
    arrange(desc(criterio))    %>%                         # arrange原始設定為從小到大排序(desc改由大到小排序) 
    dplyr::filter(row_number()<=PortfolioNum) %>%          # 取出排名前10檔支股票
    pull(code)                                             # 只取股票代碼的部分
  
  ############################## 最適化權重配置 ###################################
  
  iWeightRetData<-iStockRetData[,which(colnames(iStockRetData)%in%criterioSelectCode)] %>% # 將前面的insample data選出最終篩選出的股票資料
    as.timeSeries()                                                          # 轉換成時間序列格式
  
  TangencySpec <- portfolioSpec()                                                         # 帶入fPortfolio套件參數
  
  #限制每股至少要投資0%
  atleastpercent<-0                                                                  
  boxConstraints <- c( "minW[1:ncol(iWeightRetData)]=atleastpercent")                     # 要代入函數裡的參數
  
  #樣本估計
  tgPortfolio <- tangencyPortfolio(data = iWeightRetData, spec = TangencySpec, constraints = boxConstraints) # 代入函數產生最適化權重
  
  tgWeight<- getWeights(tgPortfolio@portfolio)                                            # 只取出權重部分
  assetWeight<-tgWeight                                                                   # 儲存至另一變數中
  
  
  ############################### 持有部位儲存 #########################
  
  
  # 紀錄舊持有部位表
  historyHoldInfoTable <- historyHoldInfoTable %>% bind_rows(holdInfoTable)                                             # 每期持有資訊併入該變數
  
  # 建立新持有部位表
  holdInfoTable <- tibble(portfolioName = character(), code = character(), inDate = numeric(), inWeight = numeric(),    # 儲存後就將前期的資訊清空
                          outDate = numeric(),periodRet=numeric())
  
  
  holdInfoTable <- holdInfoTable %>% 
    bind_rows(tibble(portfolioName = "測試投資策略", code = colnames(iWeightRetData), inDate = rebalanceDate[ix],       # 將前述挑選結果進行儲存
                     inWeight = assetWeight, outDate =ifelse(ix<length(rebalanceDate),rebalanceDate[ix+1],EndDate)))%>% 
    
    left_join(AdjStockPrice %>% select(code,date,inclose=close),by=c("code"="code","inDate"="date"))   %>%              # 併入買進時調整後收盤價
    left_join(AdjStockPrice %>% select(code,date,outclose=close),by=c("code"="code","outDate"="date")) %>%              # 併入賣出時調整後收盤價
    mutate(periodRet=(outclose/inclose)-1) %>%                                                                          # 計算各股期間報酬
    select(portfolioName,code,inDate,inWeight,outDate,periodRet)                                                        # 挑選所需欄位
  
}
retTable<-historyHoldInfoTable %>% group_by(inDate) %>% summarise(sumRet=sum(inWeight*periodRet))
retTable$inDate<-as.Date(retTable$inDate %>% as.character(), format = '%Y%m%d')          
retTable = xts(retTable[,2], order.by = retTable$inDate)

# 需要之時間序列格式

nav<-cumprod(1+retTable$sumRet)                                     # 累積報酬率序列
cumRet<-nav[nrow(nav)] %>% as.numeric() %>% .[]-1                             # 累積報酬率
annualRet<-Return.annualized(retTable) %>%  as.numeric()            # 年化報酬率
annualStd<-StdDev.annualized(retTable) %>%  as.numeric()            # 年化標準差
sharpeRatio<-SharpeRatio.annualized(retTable) %>%   as.numeric()    # 夏普指數
mdd<-maxDrawdown(retTable) %>% as.numeric()                         # 最大回撤率

performance <- data.frame(cumRet=cumRet,annualRet=annualRet,annualStd=annualStd,sharpeRatio=sharpeRatio,mdd=mdd) # 整理績效表
colnames(performance)<-c("累積報酬率","年化報酬率","年化標準差","夏普指數","最大回測率")

performance
par(family="STSongti-TC-Light")
plot(nav-1,main="投資組合(一)累積報酬率走勢圖",col="tomato",cex=0.8,bg="White",lwd=3)

######################################### 與加權指數比較 ###########################################################
TW = read.csv("/Users/USER/Desktop/FilterStock_by_index/^TWII.csv")
TW$Date = as.Date(TW$Date)
TW = TW[,c(1,6)]
##報酬率
retx<-function(x){            
  
  x[-1]/x[-length(x)]-1  
  
}
TW$return = c(NA, retx(TW$Adj.Close))
TW = TW[-1,]
xts.TW = xts(TW[,2:3], order.by = TW$Date)

nav<-cumprod(1+xts.TW$return) 
cumRet<-nav[nrow(nav)] %>% as.numeric() %>% .[]-1                             # 累積報酬率
annualRet<-Return.annualized(xts.TW$return) %>%  as.numeric()            # 年化報酬率
annualStd<-StdDev.annualized(xts.TW) %>%  as.numeric()            # 年化標準差
sharpeRatio<-SharpeRatio.annualized(xts.TW$return) %>%   as.numeric()    # 夏普指數
mdd<-maxDrawdown(xts.TW$return) %>% as.numeric()                         # 最大回撤率

performance <- data.frame(cumRet=cumRet,annualRet=annualRet,annualStd=annualStd,sharpeRatio=sharpeRatio,mdd=mdd) # 整理績效表
colnames(performance)<-c("累積報酬率","年化報酬率","年化標準差","夏普指數","最大回測率")
performance
plot(nav-1,main="TSEC weighted index累積報酬率走勢圖") 


#################投資策略(二)####################
# 回測開始日
StartDate<-20180901
# 回測結束日
EndDate<-20191231
# 投組股票檔數
PortfolioNum<-10
# 再平衡日期
rebalanceDays<-10
# insample data 天數
insampledays<-500

AdjStockPrice<-FinancialData1$AdjStockPrice     
FinancialReport<-FinancialData1$FinancialReport
## 資料整理
# 取出所有交易日
tradeDate<-AdjStockPrice %>%pull(date) %>%unique()

# 產生再平衡日期
rebalanceDate<-tradeDate[which((tradeDate>=StartDate)&(tradeDate<=EndDate))] %>% .[seq(1,length(.),rebalanceDays)]

# 產生股價日報酬
StockRetData<-AdjStockPrice               %>%
  select(code,date,close)                 %>% 
  arrange(code,date)                      %>% 
  group_by(code)                          %>% 
  mutate(ret=(close/dplyr::lag(close))-1) %>% 
  na.omit()                               %>% 
  select(code,date,ret) 

## 建立儲存表
holdInfoTable <- tibble(portfolioName = character(), code = character(), inDate = numeric(), 
                        inWeight = numeric(),outDate = numeric(),periodRet=numeric())
historyHoldInfoTable <- NULL
ix<-1

## 初篩
FinDate<-FinancialReport %>% 
  dplyr::filter(usedate<=rebalanceDate[ix]) %>% 
  pull(usedate) %>%                             
  unique() %>%                                  
  max()                                         


FinSelectCode<-FinancialReport %>%
  dplyr::filter(usedate==FinDate) %>% 
  select(code,usedate,net_sales,roe_after_tax,current_ratio,eps) %>%      
  dplyr::filter(net_sales>2500000) %>% 
  dplyr::filter(roe_after_tax>0.1) %>%          
  dplyr::filter(current_ratio>200) %>%
  dplyr::filter(eps>1) %>%
  pull(code)

FinSelectCode
length(FinSelectCode)


# 取出insample data 日期
insampledate<-tradeDate[(which(tradeDate==rebalanceDate[ix])-insampledays):(which(tradeDate==rebalanceDate[ix])-1)]

# 取出insample日期間的股價日報酬，並轉成寬資料
iStockRetData <- StockRetData %>% 
  dplyr::filter(date%in%insampledate,code%in%FinSelectCode)%>% 
  select(code, date, ret) %>%                                   
  group_by(code) %>%                                            
  dplyr::filter(n() >= (insampledays*0.7)) %>%                  
  mutate(date = ymd(date)) %>%                                  
  spread(key = "code", value = "ret") %>%                       
  select(date, everything())

iStockRetData <- xts(iStockRetData[, -1], order.by = iStockRetData$date)  

# 取出股票代碼
iStockCode<-colnames(iStockRetData)
iStockCode
length(iStockCode) 

##再篩
criterioData<-SharpeRatio(iStockRetData,FUN = "StdDev") %>%                 
  as.numeric()  %>%                                            
  data.frame(code=iStockCode,criterio=.) %>%                   
  mutate(code=as.character(code),criterio=as.numeric(criterio))

criterioSelectCode<-criterioData               %>%                        
  arrange(desc(criterio))    %>%                         
  dplyr::filter(row_number()<=10) %>%     # 取出排名前10檔支股票
  pull(code)                                             

criterioSelectCode
length(criterioSelectCode) # 10檔


## 最適化權重
iWeightRetData<-iStockRetData[,which(colnames(iStockRetData)%in%criterioSelectCode)] %>% as.timeSeries()                                                          # 轉換成時間序列格式
# 帶入fPortfolio套件參數
TangencySpec <- portfolioSpec()                   

#限制每股至少要投資0
atleastpercent<-0                                         
boxConstraints <- c("minW[1:ncol(iWeightRetData)]=atleastpercent")                     # 要代入函數裡的參數


#樣本估計
tgPortfolio <- tangencyPortfolio(data = iWeightRetData, spec = TangencySpec, constraints = boxConstraints) # 代入函數產生最適化權重
tgWeight<- getWeights(tgPortfolio@portfolio)                                            # 只取出權重部分
assetWeight<-tgWeight  
tgWeight

historyHoldInfoTable <- historyHoldInfoTable %>% bind_rows(holdInfoTable)                     # 每期持有資訊併入該變數

# 建立新持有部位表
holdInfoTable <- tibble(portfolioName = character(), code = character(), inDate = numeric(),  # 儲存後就將前期的資訊清空
                        inWeight = numeric(),outDate = numeric(),periodRet=numeric())


holdInfoTable <- holdInfoTable %>%                                                              # 將前述挑選結果進行儲存
  bind_rows(tibble(portfolioName = "測試投資策略", 
                   code = colnames(iWeightRetData), 
                   inDate = rebalanceDate[ix], 
                   inWeight = assetWeight, 
                   outDate=ifelse(ix<length(rebalanceDate),rebalanceDate[ix+1],EndDate)))%>%
  
  left_join(AdjStockPrice%>%
              select(code,date,inclose=close),by=c("code"="code","inDate"="date"))   %>%        # 併入買進時調整後收盤價
  left_join(AdjStockPrice %>%         
              select(code,date,outclose=close),by=c("code"="code","outDate"="date")) %>%        # 併入賣出時調整後收盤價
  mutate(periodRet=(outclose/inclose)-1) %>%                                                  # 計算各股期間報酬
  select(portfolioName,code,inDate,inWeight,outDate,periodRet)                                # 挑選所需欄位

# 建立儲存表
holdInfoTable <- tibble(portfolioName = character(), code = character(), inDate = numeric(),
                        inWeight = numeric(),outDate = numeric(),periodRet=numeric())

historyHoldInfoTable <- NULL


ix<-1

for(ix in 1:length(rebalanceDate)){
  
  cat(paste0("目前正在回測日期: ", rebalanceDate[ix], "  進度:", ix, " / ", length(rebalanceDate),"\n"))
  
  ################# 財報選股(初篩) #############################
  
  # 先挑好過去最接近再平衡日的日期
  
  FinDate<-FinancialReport %>% 
    dplyr::filter(usedate<=rebalanceDate[ix]) %>% # 取所有比rebalance date還早的資料
    pull(usedate) %>%                             # 只取出usedate的部分(用pull可跳脫data frame的框架)
    unique() %>%                                  # 去除重複資料
    max()                                         # 只取最靠近rebalance date的那一天
  
  FinSelectCode<-FinancialReport %>%
    dplyr::filter(usedate==FinDate) %>% 
    select(code,usedate,net_sales,roe_after_tax,current_ratio,eps) %>%      
    dplyr::filter(net_sales>2500000) %>% 
    dplyr::filter(roe_after_tax>0.1) %>%          
    dplyr::filter(current_ratio>200) %>%
    dplyr::filter(eps>1) %>%
    pull(code)
  ################# 績效指標選股(再篩) ########################
  
  
  # 取出insample data 日期
  insampledate<-tradeDate[(which(tradeDate==rebalanceDate[ix])-insampledays):(which(tradeDate==rebalanceDate[ix])-1)]
  
  # 取出insample日期間的股價日報酬，並轉成寬資料
  iStockRetData <- StockRetData %>% 
    dplyr::filter(date%in%insampledate,code%in%FinSelectCode) %>% # 挑出樣本內日期之資料，並只取財報篩選過的股票代碼
    select(code, date, ret) %>%                                   # 取出需要欄位
    group_by(code) %>%                                            # 以股票代碼為單位進行計算
    dplyr::filter(n() >= (insampledays*0.7)) %>%                  # 限制至少要有7成日數
    mutate(date = ymd(date)) %>%                                  # 將日期格式進行改變(ex:20190101 => 2019-01-01)
    spread(key = "code", value = "ret") %>%                       # 將長資料以code展開變成寬資料
    select(date, everything())
  
  # 將資料轉成時間序列
  iStockRetData <- xts(iStockRetData[, -1], order.by = iStockRetData$date)            # 需要將資料轉變成時間序列格式，並依照日期排序
  
  # 取出股票代碼，以便後續整理
  iStockCode<-colnames(iStockRetData)
  
  
  criterioData<-SharpeRatio(iStockRetData,FUN = "StdDev") %>%                 # 使用此函數產生shape ratio數值(input的iStockRetData須為時間序列格式)
    as.numeric()  %>%                                            # 將output的東西轉變成數值格式
    data.frame(code=iStockCode,criterio=.) %>%                   # 將股票代碼及數值進行合併
    mutate(code=as.character(code),criterio=as.numeric(criterio))# 轉換票代碼及績效指標格式
  
  criterioSelectCode<-criterioData               %>%                        
    arrange(desc(criterio))    %>%                         # arrange原始設定為從小到大排序(desc改由大到小排序) 
    dplyr::filter(row_number()<=PortfolioNum) %>%          # 取出排名前五檔支股票
    pull(code)                                             # 只取股票代碼的部分
  
  ############################## 最適化權重配置 ###################################
  
  iWeightRetData<-iStockRetData[,which(colnames(iStockRetData)%in%criterioSelectCode)] %>% # 將前面的insample data選出最終篩選出的股票資料
    as.timeSeries()                                                          # 轉換成時間序列格式
  
  TangencySpec <- portfolioSpec()                                                         # 帶入fPortfolio套件參數
  
  #限制每股至少要投資0%
  atleastpercent<-0                                                                  
  boxConstraints <- c( "minW[1:ncol(iWeightRetData)]=atleastpercent")                     # 要代入函數裡的參數
  
  #樣本估計
  tgPortfolio <- tangencyPortfolio(data = iWeightRetData, spec = TangencySpec, constraints = boxConstraints) # 代入函數產生最適化權重
  
  tgWeight<- getWeights(tgPortfolio@portfolio)                                            # 只取出權重部分
  assetWeight<-tgWeight                                                                   # 儲存至另一變數中
  
  
  ############################### 持有部位儲存 #########################
  
  
  # 紀錄舊持有部位表
  historyHoldInfoTable <- historyHoldInfoTable %>% bind_rows(holdInfoTable)                                             # 每期持有資訊併入該變數
  
  # 建立新持有部位表
  holdInfoTable <- tibble(portfolioName = character(), code = character(), inDate = numeric(), inWeight = numeric(),    # 儲存後就將前期的資訊清空
                          outDate = numeric(),periodRet=numeric())
  
  
  holdInfoTable <- holdInfoTable %>% 
    bind_rows(tibble(portfolioName = "測試投資策略", code = colnames(iWeightRetData), inDate = rebalanceDate[ix],       # 將前述挑選結果進行儲存
                     inWeight = assetWeight, outDate =ifelse(ix<length(rebalanceDate),rebalanceDate[ix+1],EndDate)))%>% 
    
    left_join(AdjStockPrice %>% select(code,date,inclose=close),by=c("code"="code","inDate"="date"))   %>%              # 併入買進時調整後收盤價
    left_join(AdjStockPrice %>% select(code,date,outclose=close),by=c("code"="code","outDate"="date")) %>%              # 併入賣出時調整後收盤價
    mutate(periodRet=(outclose/inclose)-1) %>%                                                                          # 計算各股期間報酬
    select(portfolioName,code,inDate,inWeight,outDate,periodRet)                                                        # 挑選所需欄位
  
}
retTable<-historyHoldInfoTable %>% group_by(inDate) %>% summarise(sumRet=sum(inWeight*periodRet))
retTable$inDate<-as.Date(retTable$inDate %>% as.character(), format = '%Y%m%d')          
retTable = xts(retTable[,2], order.by = retTable$inDate)

# 需要之時間序列格式

nav<-cumprod(1+retTable$sumRet)                                     # 累積報酬率序列
cumRet<-nav[nrow(nav)] %>% as.numeric() %>% .[]-1                             # 累積報酬率
annualRet<-Return.annualized(retTable) %>%  as.numeric()            # 年化報酬率
annualStd<-StdDev.annualized(retTable) %>%  as.numeric()            # 年化標準差
sharpeRatio<-SharpeRatio.annualized(retTable) %>%   as.numeric()    # 夏普指數
mdd<-maxDrawdown(retTable) %>% as.numeric()                         # 最大回撤率

performance <- data.frame(cumRet=cumRet,annualRet=annualRet,annualStd=annualStd,sharpeRatio=sharpeRatio,mdd=mdd) # 整理績效表
colnames(performance)<-c("累積報酬率","年化報酬率","年化標準差","夏普指數","最大回測率")

performance
par(family="STSongti-TC-Light")
plot(nav-1,main="投資組合(二)累積報酬率走勢圖",col="tomato",cex=0.8,bg="White",lwd=3)

