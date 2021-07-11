# Title     : TODO
# Objective : TODO
# Created by: m20190417
# Created on: 4/6/2021

pacman::p_load(forecast,quantmod, randomForest, mlbench, caret, car, rugarch, rmgarch,coinmarketcapr,xts, tidyverse, ggthemes,
               gridExtra, tseries, lmtest, FinTS, mgarchBEKK, ccgarch, xtable, MTS, plm)

# tirar os dados e fazer back e forward fill
AAPL <- na.locf(na.locf(getSymbols("AAPL", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
AAPL_adj <- xts(AAPL$AAPL.Adjusted)
plot(AAPL_adj)
MSFT <- na.locf(na.locf(getSymbols("MSFT", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
MSFT_adj <- MSFT$MSFT.Adjusted
plot(MSFT_adj)
V <- na.locf(na.locf(getSymbols("V", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
V_adj <- V$V.Adjusted
plot(V_adj)
JPM <- na.locf(na.locf(getSymbols("JPM", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
JPM_adj <- JPM$JPM.Adjusted
plot(JPM_adj)
JNJ <- na.locf(na.locf(getSymbols("JNJ", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
JNJ_adj <- JNJ$JNJ.Adjusted
plot(JNJ_adj)
WMT <- na.locf(na.locf(getSymbols("WMT", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
WMT_adj <- WMT$WMT.Adjusted
plot(WMT_adj)
UNH <- na.locf(na.locf(getSymbols("UNH", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
UNH_adj <- UNH$UNH.Adjusted
plot(UNH_adj)
PG <- na.locf(na.locf(getSymbols("PG", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
PG_adj <- PG$PG.Adjusted
plot(PG_adj)
DIS <- na.locf(na.locf(getSymbols("DIS", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
DIS_adj <- DIS$DIS.Adjusted
plot(DIS_adj)
HD <- na.locf(na.locf(getSymbols("HD", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
HD_adj <- HD$HD.Adjusted
plot(HD_adj)
VZ <- na.locf(na.locf(getSymbols("VZ", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
VZ_adj <- VZ$VZ.Adjusted
plot(VZ_adj)
INTC <- na.locf(na.locf(getSymbols("INTC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
INTC_adj <- INTC$INTC.Adjusted
plot(INTC_adj)

# fazer log returns dos dados
ret_AAPL <- dailyReturn(AAPL_adj, type = "log")
ret_MSFT <- dailyReturn(MSFT_adj, type = "log")
ret_V <- dailyReturn(V_adj, type = "log")
ret_JPM <- dailyReturn(JPM_adj, type = "log")
ret_JNJ <- dailyReturn(JNJ_adj, type = "log")
ret_WMT <- dailyReturn(WMT_adj, type = "log")
ret_UNH <- dailyReturn(UNH_adj, type = "log")
ret_PG <- dailyReturn(PG_adj, type = "log")
ret_DIS <- dailyReturn(DIS_adj, type = "log")
ret_HD <- dailyReturn(HD_adj, type = "log")
ret_VZ <- dailyReturn(VZ_adj, type = "log")
ret_INTC <- dailyReturn(INTC_adj, type = "log")

# por os dados em matriz e fazer scaling
data <- na.locf(na.locf(cbind(ret_AAPL, ret_MSFT, ret_V, ret_JPM, ret_JNJ, ret_WMT, ret_UNH, ret_PG, ret_DIS, ret_HD, ret_VZ, ret_INTC)*1000,fromLast = TRUE))
colnames(data) <- c("ret_AAPL", "ret_MSFT", "ret_V", "ret_JPM", "ret_JNJ", "ret_WMT", "ret_UNH", "ret_PG", "ret_DIS", "ret_HD", "ret_VZ", "ret_INTC")

lag_data <- function(data, column_to_remove, lag) {

  removed_data <- data[,-column_to_remove]
  rows <- (length(removed_data[,1])-(lag-1)):length(removed_data[,1])
  removed_row <- removed_data[-rows,]
  surplus <- coredata(removed_data[1,])
  if(lag > 1){
    for(i in 1:(lag-1)){
      surplus <- rbind(surplus,coredata(removed_data[1,]))
    }
  }
  exog_var <- rbind(surplus,coredata(removed_row))
}

for(i in 1:4){#12){

  exog_var <- lag_data(data, i, lag = 1)
  for(j in 2:4){
    temp <- lag_data(data, i, lag = j)
    temp_names <- colnames(temp)
    for(z in 1:3){
      temp_names[z] <- paste(temp_names[z],"_",toString(j),sep="")
    }
    colnames(temp) <- temp_names
    exog_var <- cbind(exog_var, temp)
  }
  temp_data <- data[,1]
  colnames(temp_data) <- "target"
  data_for_selection <- cbind(temp_data, exog_var)

  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  # train the model
  model <- caret::train(form = target~., data=data_for_selection, method="lm", trControl=control)
  # estimate variable importance
  importance <- varImp(model, scale=FALSE)
  # summarize importance
  print(importance)

  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  # run the RFE algorithm
  results <- rfe(form = target~., data = data_for_selection, sizes = c(1:4), rfeControl=control)
  # summarize the results
  print(results)
  # list the chosen features
  predictors(results)
  # plot the results
  plot(results, type=c("g", "o"))
}
model <- lm(target~., data = data_for_selection)
Anova(model)

models_garch_auto <- list()
for(i in 1:4) {
  exog_var <- lag_data(data, i, lag = 1)
  for(j in 2:5){
    temp <- lag_data(data, i, lag = j)
    temp_names <- colnames(temp)
    for(z in 1:3){
      temp_names[z] <- paste(temp_names[z],"_",toString(j),sep="")
    }
    colnames(temp) <- temp_names
    exog_var <- cbind(exog_var, temp)
  }
  temp <- auto.arima(data[,i],xreg = exog_var, seasonal = TRUE, stationary = TRUE)
  print("fiz modelo")
  #print(temp)
  models_garch_auto<-c(models_garch_auto,list(temp))
}
resid_vec <- matrix(nrow = length(ret_AAPL), ncol = 4)
for(i in 1:4){
  resid_vec[,i] <- resid(models_garch_auto[[i]])
}
var(resid_vec)

models_auto_base <- list()
for(i in 1:4){
  models_auto_base <- c(models_auto_base, list(auto.arima(data[,i])))
}
resid_vec_base <- matrix(nrow = length(ret_AAPL), ncol = 4)
for(i in 1:4){
  resid_vec_base[,i] <- resid(models_auto_base[[i]])
}
var(resid_vec_base)



models_garch_auto <- list()
for(i in 1:4) {
  exog_var <- lag_data(data, i, lag = 1)
  for(j in 2:5){
    temp <- lag_data(data, i, lag = j)
    temp_names <- colnames(temp)
    for(z in 1:3){
      temp_names[z] <- paste(temp_names[z],"_",toString(j),sep="")
    }
    colnames(temp) <- temp_names
    exog_var <- cbind(exog_var, temp)
  }
  temp <- auto.arima(data[,i], D= 1,xreg = exog_var, seasonal = TRUE)
  print("fiz modelo")
  #print(temp)
  models_garch_auto<-c(models_garch_auto,list(temp))
}
dados <- c
models_garch_auto[1]
plot(, type = 'l')
uspec = ugarchspec(mean.model = list(armaOrder = c(15,0), include.mean = FALSE), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "sstd")
spec1 = dccspec(uspec = multispec(replicate(2, uspec)), dccOrder = c(1,1),  distribution = "mvnorm")
fit1 = dccfit(spec1, data = cbind(data[,1], resid_vec[,1]))
print(fit1)
plot(rcor(fit1)[1,2,], type = "l")
plot(AAPL_adj)