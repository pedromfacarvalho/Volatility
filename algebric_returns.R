# Title     : TODO
# Objective : TODO
# Created by: C097564
# Created on: 2021-09-20

pacman::p_load(BatchGetSymbols, gogarch, vars, keras, rmgarch, doParallel, aTSA, reshape2, purrr, LSTS, fDMA, tidyquant, data.table, pls, dynlm, stats, ggplot2, ggfortify, plotly, zoo, TSA, gridExtra, GeneCycle, qrmdata,XML,quantmod,zoo,chron, rmgarch, rugarch, rvest, dplyr, FinTS, forecast, car, fpp2, lmtest,tseries,stats, seastests)


scrape <- read_html('https://www.slickcharts.com/nasdaq100')
tickers_DJI <- c("MMM","AXP","AMGN","AAPL","BA","CAT","CVX","CSCO","KO","DOW","GS","HD","HON","IBM","INTC","JNJ","JPM","MCD","MRK","MSFT","NKE","PG","CRM","TRV","UNH","VZ","V","WMT","WBA","DIS")

table_url <- (scrape %>% html_table(fill = TRUE))[[1]]
tickers_NDX <- c(table_url["Symbol"])[[1]]
tickers_SP500 <- c(GetSP500Stocks()["Tickers"])[[1]]

index <- BatchGetSymbols(tickers = "^DJI" , first.date = "2018-01-01", freq.data = "daily", type.return = "log")
indexes <- index[[2]]
DJI_adj <- dplyr::select(indexes, ticker, ret.adjusted.prices,ref.date)
index <- BatchGetSymbols(tickers =  "^GSPC" , first.date = "2018-01-01", freq.data = "daily", type.return = "log")
indexes <- index[[2]]
GSPC_adj <- dplyr::select(indexes, ticker, ret.adjusted.prices,ref.date)
index <- BatchGetSymbols(tickers = "^NDX", first.date = "2018-01-01", freq.data = "daily", type.return = "log")
indexes <- index[[2]]
NDX_adj <- dplyr::select(indexes, ticker, ret.adjusted.prices,ref.date)


getSymbols(tickers_DJI,
           from = "2018-01-01")
prices <- map(tickers_DJI,function(x) Ad(get(x)))
prices <- reduce(prices,merge)
colnames(prices) <- tickers_DJI

models_sarima <- list()
for(i in 1:length(data[1,])) {

  model_data <- data[,i]

  exog_var <- data[,-i]
  var_names <- colnames(exog_var)
  #p <- TSA::periodogram(model_data)
  #f <- data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:3]$period
  #q1 <- fourier(ts(model_data, frequency=f[1]), K=1)

  #q2 <- diff.ts(fourier(ts(model_data, frequency=f[2]), K=1))
  #q3 <- diff.ts(fourier(ts(model_data, frequency=f[3]), K=1))
  ##q4 <- fourier(ts(model_data, frequency=164.57), K=1)
  weeknames <- colnames(weekday_dummies[3:6])
  monthnames <- colnames(monthly_dummies[3:13])
  #quarternames <- colnames(quarterly_dummies[3:5])

  for(z in 1:3){
    for(j in 1:length(exog_var[1,])){
          if(z==1 & j==1){
              exog <- lag_transform(exog_var[,1], 1)
           } else{
                exog <- cbind(exog, lag_transform(exog_var[,j], z))
        }
    }
  }
  print(i)
  temp <- arima(model_data[4:935,], order = c(3,1,0), xreg = exog[4:935])
    #temp <- models(data = model_data[z:length(model_data)], stock_order = c(z,0), external = exog_garch, pars = as.list(na.omit(fix_var_garch)))
  #}

  # seasonal = c(models_garch_auto[[i]]$arma[3], 0, models_garch_auto[[i]]$arma[4]),
  #temp <- arima(model_data, xreg = exog, include.mean = TRUE, order = c(1,0,0))
  #print(i)
  #print(temp)
  models_sarima<-cbind(models_sarima,list(temp))
  #print(length(exog[1,])/z)
  #rm(exog)
}

models_sgarch <- function(data, stock_order = c(0,0),  external = NULL, pars = NULL) {
  model.index.spec <- ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = stock_order, include.mean = TRUE, external.regressors = external), fixed.pars = pars,  distribution.model = "norm")

  (model.index.fit <- ugarchfit(spec = model.index.spec , data = data, solver = 'hybrid'))
}

data <- diff(data)
data <- data[-1,]
models_sarima <- list()
for(i in 1:length(data[1,])) {

  model_data <- data[,i]

  exog_var <- data[,-i]
  var_names <- colnames(exog_var)
  #p <- TSA::periodogram(model_data)
  #f <- data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:3]$period
  #q1 <- fourier(ts(model_data, frequency=f[1]), K=1)

  #q2 <- diff.ts(fourier(ts(model_data, frequency=f[2]), K=1))
  #q3 <- diff.ts(fourier(ts(model_data, frequency=f[3]), K=1))
  ##q4 <- fourier(ts(model_data, frequency=164.57), K=1)
  #weeknames <- colnames(weekday_dummies[3:6])
  #monthnames <- colnames(monthly_dummies[3:13])
  #quarternames <- colnames(quarterly_dummies[3:5])

  for(z in 1:2){
    for(j in 1:length(exog_var[1,])){
          if(z==1 & j==1){
              exog <- lag_transform(exog_var[,1], 1)
           } else{
                exog <- cbind(exog, lag_transform(exog_var[,j], z))
        }
    }
  }
  print(i)
  temp <- temp <- models_sgarch(data = model_data[3:length(model_data)], stock_order = c(2,0), external = exog[3:length(model_data),])

    #temp <- models(data = model_data[z:length(model_data)], stock_order = c(z,0), external = exog_garch, pars = as.list(na.omit(fix_var_garch)))
  #}

  # seasonal = c(models_garch_auto[[i]]$arma[3], 0, models_garch_auto[[i]]$arma[4]),
  #temp <- arima(model_data, xreg = exog, include.mean = TRUE, order = c(1,0,0))
  #print(i)
  #print(temp)
  models_sarima<-cbind(models_sarima,list(temp))
  #print(length(exog[1,])/z)
  #rm(exog)
}

resid_vec <- matrix(nrow = length(residuals(temp)), ncol = length(data[1,]))
for(i in 1:length(data[1,])){
  resid_vec[,i] <- residuals(models_sarima[[i]])
}

resid_returns <- matrix(nrow = length(residuals(temp)), ncol = length(data[1,]))
for(i in 1:length(data[1,])){
  resid_returns[,i] <- dailyReturn(xts(resid_vec[,i], order.by = index(AAPL)[4:935]), type = "arithmetic")
}
resid_returns <- matrix(nrow = length(residuals(temp)), ncol = length(data[1,]))
for(i in 1:length(data[1,])){
  resid_returns[,i] <- resid_vec[,i]/coredata(data[1:(length(residuals(temp))),i])
}

resid_returns[is.infinite(resid_returns)]<-0
correlacoes <- cor(resid_returns)

#correlacoes <- cor(resid_vec)
M <- length(resid_vec[1,])
lambda_cor <- sum(rowSums(correlacoes^2 * upper.tri(correlacoes^2, diag=FALSE)))

lambda <- length(resid_returns[305:length(resid_returns[,1]),1])*lambda_cor
#lambda <- nrow(resid_crash)*lambda_cor
#lambda <- nrow(resid_not_crash)*lambda_cor

df = M*(M-1)/2
qchisq(.05, df=df)

lambda

model_index <- arima(DJI_adj$ret.adjusted.prices[4:935], order = c(1,0,0), xreg = resid_returns)
ArchTest(resid(model_index, lag = 1))$p.value


temp <- models_sgarch(data = DJI_adj$ret.adjusted.prices[4:935], stock_order = c(1,0), external = resid_returns)
fix_var_garch <- temp@fit$matcoef[,4]
while(all(na.omit(fix_var_garch<0.10))==FALSE){
  fix_var_garch[is.na(fix_var_garch)] <- max(na.omit(fix_var_garch))
  fix_var_garch[fix_var_garch!=max(na.omit(fix_var_garch))] <- NA
  fix_var_garch[fix_var_garch==max(na.omit(fix_var_garch))] <- 0
  temp <- models_sgarch(data = DJI_adj$ret.adjusted.prices[4:935], stock_order = c(1,0), external = resid_returns, pars = as.list(na.omit(fix_var_garch)))
  fix_var_garch <- temp@fit$matcoef[,4]
}
temp


sigma <- matrix(nrow = length(residuals(temp)), ncol = length(models_sarima))
for(i in 1:length(models_sarima)){
  sigma[,i] <- sigma(models_sarima[[i]])
}
sigma_index <- sigma(temp)
coefs <- coef(temp)[3:33]

h <- matrix(nrow = length(sigma[,1]), ncol = length(sigma[1,]))
for (i in 1:length(models_sarima)){

  h[,i] <- coefs[i]^2 * sigma[,i]^2
}

H <- rowSums(h) + coredata(sigma_index)^2



cond_corr <- matrix(nrow = length(sigma[,1]), ncol = length(models_sarima))
for (i in 1:length(models_sarima)){
  cond_corr[,i] <- sqrt(h[,i])/sqrt(H)
}
cond_corr <- cond_corr[, qr(cond_corr)$pivot[seq_len(qr(cond_corr)$rank)]]

plot(x = as.Date(DJI_adj$ref.date[4:935]), cond_corr[,1], type = 'l')
for (i in 2:length(cond_corr[1,])){
  lines(x = as.Date(DJI_adj$ref.date[4:935]), cond_corr[,i], type = 'l')
}

h <- h[, qr(h)$pivot[seq_len(qr(h)$rank)]]

VR <- matrix(nrow = length(sigma_index[,1]), ncol = length(models_sarima))
for (i in 1:length(models_sarima)){
  VR[,i] <- (coefs[i]^2 * sigma[,i]^2)/H
}

plot(x = as.Date(DJI_adj$ref.date[4:935]), VR[,1], type = 'l')
for (i in 2:length(cond_corr[1,])){
  lines(x = as.Date(DJI_adj$ref.date[4:935]), VR[,i], type = 'l')
}