# Title     : TODO
# Objective : TODO
# Created by: m20190417
# Created on: 2/7/2021

pacman::p_load(stargazer, BatchGetSymbols, aTSA, purrr, LSTS, fDMA, tidyquant, data.table, pls, dynlm, stats, ggplot2, ggfortify, plotly, zoo, TSA, gridExtra, GeneCycle, qrmdata,XML,quantmod,zoo,chron, rmgarch, rugarch, rvest, dplyr, FinTS, forecast, car, fpp2, lmtest,tseries,stats, seastests)

#pacman::p_load(forecast,quantmod, tidyquant, rugarch, rmgarch,coinmarketcapr,xts, tidyverse, ggthemes,
             #  gridExtra, tseries, lmtest, FinTS, mgarchBEKK, ccgarch, xtable, MTS, plm,zoo)

# dados com back e forward fill
index <- BatchGetSymbols(tickers = "^DJI" , thresh.bad.data = 0, first.date = "2010-01-01", freq.data = "daily", type.return = "log")
indexes <- index[[2]]
DJI_adj <- dplyr::select(indexes, ticker, ret.adjusted.prices,ref.date)
index <- BatchGetSymbols(tickers =  "^GSPC" , thresh.bad.data = 0, first.date = "2010-01-01", freq.data = "daily", type.return = "log")
indexes <- index[[2]]
GSPC_adj <- dplyr::select(indexes, ticker, ret.adjusted.prices,ref.date)
index <- BatchGetSymbols(tickers = "^NDX" , thresh.bad.data = 0, first.date = "2010-01-01", freq.data = "daily", type.return = "log")
indexes <- index[[2]]
NDX_adj <- dplyr::select(indexes, ticker, ret.adjusted.prices,ref.date)


BTC <- na.locf(na.locf(getSymbols("BTC-USD", src = "yahoo", auto.assign = FALSE)), fromLast = TRUE)
BTC_adj <- ts(BTC$`BTC-USD.Adjusted`)
plot(BTC_adj)
ETH <- na.locf(na.locf(getSymbols("ETH-USD", src = "yahoo", auto.assign = FALSE)), fromLast = TRUE)
ETH_adj <- ts(ETH$`ETH-USD.Adjusted`)
plot(ETH_adj)
XRP <- na.locf(na.locf(getSymbols("XRP-USD", src = "yahoo", auto.assign = FALSE)), fromLast = TRUE)
XRP_adj <- ts(XRP$`XRP-USD.Adjusted`)
plot(XRP_adj)
XMR <- na.locf(na.locf(getSymbols("XMR-USD", src = "yahoo", auto.assign = FALSE)), fromLast = TRUE)
XMR_adj <- ts(XMR$`XMR-USD.Adjusted`)
plot(XMR_adj)

# returns
ret_DJI <- xts(DJI_adj$ret.adjusted.prices, order.by=as.POSIXct(DJI_adj$ref.date))
ret_GSPC <- xts(GSPC_adj$ret.adjusted.prices, order.by=as.POSIXct(GSPC_adj$ref.date))
ret_NDX <- xts(NDX_adj$ret.adjusted.prices, order.by=as.POSIXct(NDX_adj$ref.date))
ret_BTC <- xts(dailyReturn(BTC_adj, type = "log"),order.by = index(BTC))
ret_ETH <- xts(dailyReturn(ETH_adj, type = "log"),order.by = index(ETH))
ret_XRP <- xts(dailyReturn(XRP_adj, type = "log"),order.by = index(XRP))
ret_XMR <- xts(dailyReturn(XMR_adj, type = "log"),order.by = index(XMR))

#colnames(merge_total) <- c('DJI', 'GSPC', 'NDX', 'BTC', 'ETH', 'XRP')

# criação de pares
rDJI_rBTC <- merge(ret_DJI, ret_BTC, join = "right")
rDJI_rBTC <- na.fill(rDJI_rBTC,0)
rDJI_rETH <- merge(ret_DJI, ret_ETH, join = 'right')
rDJI_rETH <- na.fill(rDJI_rETH,0)
rDJI_rXRP <- merge(ret_DJI, ret_XRP, join = 'right')
rDJI_rXRP <- na.fill(rDJI_rXRP,0)
rDJI_rXMR <- merge(ret_DJI, ret_XMR, join = 'right')
rDJI_rXMR <- na.fill(rDJI_rXMR,0)

rGSPC_rBTC <- merge(ret_GSPC, ret_BTC, join = 'right')
rGSPC_rBTC <- na.fill(rGSPC_rBTC,0)
rGSPC_rETH <- merge(ret_GSPC, ret_ETH, join = 'right')
rGSPC_rETH <- na.fill(rGSPC_rETH,0)
rGSPC_rXRP <- merge(ret_GSPC, ret_XRP, join = 'right')
rGSPC_rXRP <- na.fill(rGSPC_rXRP,0)
rGSPC_rXMR <- merge(ret_GSPC, ret_XMR, join = 'right')
rGSPC_rXMR <- na.fill(rGSPC_rXMR,0)

rNDX_rBTC <- merge(ret_NDX, ret_BTC, join = 'right')
rNDX_rBTC <- na.fill(rNDX_rBTC,0)
rNDX_rETH <- merge(ret_NDX, ret_ETH, join = 'right')
rNDX_rETH <- na.fill(rNDX_rETH,0)
rNDX_rXRP <- merge(ret_NDX, ret_XRP, join = 'right')
rNDX_rXRP <- na.fill(rNDX_rXRP,0)
rNDX_rXMR <- merge(ret_NDX, ret_XMR, join = 'right')
rNDX_rXMR <- na.fill(rNDX_rXMR,0)


# testes à normalidade
jarque.bera.test(ret_XMR)
adf.test(ret_XMR)

# testes de causalidade
grangertest(ret_DJI ~ daily.returns, order = 4, data = rDJI_rBTC) #2
grangertest(ret_DJI ~ daily.returns, order = 2, data = rDJI_rETH) #2 e 4
grangertest(ret_DJI ~ daily.returns, order = 4, data = rDJI_rXRP) #2 é o melhor 0.1871 não causal, encontrar alternativa
grangertest(ret_DJI ~ daily.returns, order = 4, data = rDJI_rXMR) #2
grangertest(ret_GSPC ~ daily.returns, order = 4, data = rGSPC_rBTC) #2+
grangertest(ret_GSPC ~ daily.returns, order = 4, data = rGSPC_rETH) #2 4
grangertest(ret_GSPC ~ daily.returns, order = 4, data = rGSPC_rXRP) #2 mas 15% significancia
grangertest(ret_GSPC ~ daily.returns, order = 6, data = rGSPC_rXMR) #2
grangertest(ret_NDX ~ daily.returns, order = 4, data = rNDX_rBTC) #2
grangertest(ret_NDX ~ daily.returns, order = 4, data = rNDX_rETH) #2
grangertest(ret_NDX ~ daily.returns, order = 4, data = rNDX_rXRP) #2 mas 6.57% significancia
grangertest(ret_NDX ~ daily.returns, order = 4, data = rNDX_rXMR) #2 mas 6.21% significancia


# modelo ARMA-GARCH para os pares
models_spillovers <- function(data, index_order, crypto_order, value_lagged) {
  # modelo ARMA-GARCH para o indice
  model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = index_order, include.mean = TRUE), distribution.model = "sstd")

  (model.index.fit = ugarchfit(spec = model.index.spec , data = data$daily.returns, solver = 'hybrid'))

  #junção dos dados com lag fornecido por granger
  index_data <- merge(data$daily.returns, residuals(model.index.fit))[0:(length(data$daily.returns)-value_lagged)]
  lagged <- data$daily.returns.1[(1+value_lagged):length(data$daily.returns.1)]

  # modelo ARMA-GARCH para crypto
  model.crypto.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)),
                        mean.model = list(armaOrder = crypto_order, include.mean = TRUE, external.regressors = index_data), distribution.model = "sstd")

  (model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = lagged, solver = 'hybrid'))

  # usar os parametros do modelo para calcular os spillovers e fazer plot do gráfico
  params <- coef(model.crypto.fit)
  cond_vol_index <- params["mxreg2"]^2 * sigma(model.index.fit)[0:length(lagged)]^2
  cond_vol_crypto <- sigma(model.crypto.fit)^2
  h <- (cond_vol_index + cond_vol_crypto)
  spillover <- cond_vol_index / h
  dates <- index(lagged[0:(length(lagged)-value_lagged)])
  plot(x = dates, y = spillover, type = "l")



}


# modelos ARMA-GARCH com dummy variables
models_spillovers_dummy <- function(data, index_order, crypto_order, value_lagged) {
  # modelo ARMA-GARCH para indice
  model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = index_order, include.mean = TRUE), distribution.model = "sstd")

  (model.index.fit = ugarchfit(spec = model.index.spec , data = data$daily.returns, solver = 'hybrid'))

  # ciração das dummies de acordo com o crash de março de 2020
  dummy_returns_simple <- ifelse(index(data$daily.returns) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
  dummy_residuals_simple <- ifelse(index(data$daily.returns) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
  dummy_returns <- dummy_returns_simple*data$daily.returns
  dummy_residuals <- dummy_residuals_simple*residuals(model.index.fit)

  #junção dos dados com o lag dado por granger
  index_data <- merge(data$daily.returns, residuals(model.index.fit), dummy_returns, dummy_residuals)[0:(length(data$daily.returns)-value_lagged)]

  lagged <- data$daily.returns.1[(1+value_lagged):length(data$daily.returns.1)]

  # modelo ARMA-GARCH para crypto com os dummies
  model.crypto.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)),
                        mean.model = list(armaOrder = crypto_order, include.mean = TRUE, external.regressors = index_data), distribution.model = "sstd")

  (model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = lagged, solver = 'hybrid'))

  #calculo dos spillovers usandos os coeficientes do modelo e criação de gráficos
  params <- coef(model.crypto.fit)
  cond_vol_index <- (params["mxreg2"]^2 + (dummy_residuals_simple*params["mxreg4"]^2))[0:length(lagged)] * sigma(model.index.fit)[0:length(lagged)]^2
  cond_vol_crypto <- sigma(model.crypto.fit)^2
  h <- (cond_vol_index + cond_vol_crypto)
  spillover <- cond_vol_index / h
  dates <- index(lagged[0:(length(lagged)-value_lagged)])
  plot(x = dates, y = spillover, type = "l")



}

# aplicação dos modelos
models_spillovers(data = rDJI_rBTC, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 4)
models_spillovers(data = rDJI_rETH, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 4)
models_spillovers(data = rDJI_rXRP, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers(data = rDJI_rXMR, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 5)
models_spillovers(data = rGSPC_rBTC, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers(data = rGSPC_rETH, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 4)
models_spillovers(data = rGSPC_rXRP, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers(data = rGSPC_rXMR, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers(data = rNDX_rBTC, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers(data = rNDX_rETH, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers(data = rNDX_rXRP, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers(data = rNDX_rXMR, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)

models_spillovers_dummy(data = rDJI_rBTC, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 4)
models_spillovers_dummy(data = rDJI_rETH, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 4)
models_spillovers_dummy(data = rDJI_rXRP, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rDJI_rXMR, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 5)
models_spillovers_dummy(data = rGSPC_rBTC, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rGSPC_rETH, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 4)
models_spillovers_dummy(data = rGSPC_rXRP, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rGSPC_rXMR, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rNDX_rBTC, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rNDX_rETH, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rNDX_rXRP, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rNDX_rXMR, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)


lag_transform <- function(x, k){

      lagged =  na.locf(na.locf(c(rep(NA, k), x[1:(length(x)-k)]), fromLast = TRUE))
      return(lagged)
}


#DJI_teste <- ts(rDJI_rBTC$ret_DJI)



#model_index = arima(ret_DJI, order = c(4,0,0), include.mean = TRUE)
model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                      mean.model = list(armaOrder = c(4,0,0), include.mean = TRUE), distribution.model = "norm")

(model.index.fit = ugarchfit(spec = model.index.spec , data = na.fill(ret_DJI,0), solver = 'hybrid'))


index_resid <- xts(residuals(model.index.fit), order.by=as.POSIXct(DJI_adj$ref.date))
filled_index_resid_merge <- na.fill(merge(index_resid, ret_BTC, join = "right"),0)
filled_index_resid <- ts(filled_index_resid_merge$x)
lags = 4
lagged_index_resid <- matrix(nrow = length(filled_index_resid), ncol = lags)
for (i in 1:lags){
  lagged_index_resid[,i] <- lag_transform(filled_index_resid, k = i)
}
returns_index <- ts(rDJI_rBTC$ret_DJI)
lagged_returns <- matrix(nrow = length(returns_index), ncol = lags)
for (i in 1:lags){
  lagged_returns[,i] <- lag_transform(returns_index, k = i)
}

exogenous_crypto = ts(lagged_returns[,1])
for (i in 2:lags){
 exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_index_resid[,i]))
}
(model_crypto = arima(ret_BTC, order = c(1,0,0), xreg = exogenous_crypto))
model.crypto.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = ret_BTC, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit)[7:10]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from DJI to BTC", xlab = "Time", ylab = "VR")

dummy_returns_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_residuals_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_returns <- dummy_returns_simple*returns_index
dummy_residuals <- dummy_residuals_simple*filled_index_resid
lagged_dummy_returns <- matrix(nrow = length(dummy_returns), ncol = lags)
for (i in 1:lags){
  lagged_dummy_returns[,i] <- lag_transform(dummy_returns, k = i)
}
lagged_dummy_residuals <- matrix(nrow = length(dummy_residuals), ncol = lags)
for (i in 1:lags){
  lagged_dummy_residuals[,i] <- lag_transform(dummy_residuals, k = i)
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_residuals[,i]))
}

model.crypto.spec.dummy = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit.dummy = ugarchfit(spec = model.crypto.spec.dummy , data = ret_BTC, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit.dummy)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit.dummy)[7:10]^2
dummy_params <- coef(model.crypto.fit.dummy)[15:18]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  if (dummy_residuals[i] == 1){
    variances[i,1] <- (crypto_params[1]+dummy_params[1])*var_index[i+3]+(crypto_params[2]+dummy_params[2])*var_index[i+2]+(crypto_params[3]+dummy_params[3])*var_index[i+1]+(crypto_params[4]+dummy_params[4])*var_index[i]
  }
  else {
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
  }
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from DJI to BTC with dummy variables", xlab = "Time", ylab = "VR")



stargazer::stargazer(model.index.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit.dummy@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")






##################################################################################ETH
model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                      mean.model = list(armaOrder = c(4,0,0), include.mean = TRUE), distribution.model = "norm")

(model.index.fit = ugarchfit(spec = model.index.spec , data = na.fill(ret_DJI,0), solver = 'hybrid'))


index_resid <- xts(residuals(model.index.fit), order.by=as.POSIXct(DJI_adj$ref.date))
filled_index_resid_merge <- na.fill(merge(index_resid, ret_ETH, join = "right"),0)
filled_index_resid <- ts(filled_index_resid_merge$x)
lags = 4
lagged_index_resid <- matrix(nrow = length(filled_index_resid), ncol = lags)
for (i in 1:lags){
  lagged_index_resid[,i] <- lag_transform(filled_index_resid, k = i)
}
returns_index <- ts(rDJI_rETH$ret_DJI)
lagged_returns <- matrix(nrow = length(returns_index), ncol = lags)
for (i in 1:lags){
  lagged_returns[,i] <- lag_transform(returns_index, k = i)
}

exogenous_crypto = ts(lagged_returns[,1])
for (i in 2:lags){
 exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_index_resid[,i]))
}
(model_crypto = arima(ret_ETH, order = c(1,0,0), xreg = exogenous_crypto))
model.crypto.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = ret_ETH, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit)[7:10]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from DJI to ETH", xlab = "Time", ylab = "VR")

dummy_returns_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_residuals_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_returns <- dummy_returns_simple*returns_index
dummy_residuals <- dummy_residuals_simple*filled_index_resid
lagged_dummy_returns <- matrix(nrow = length(dummy_returns), ncol = lags)
for (i in 1:lags){
  lagged_dummy_returns[,i] <- lag_transform(dummy_returns, k = i)
}
lagged_dummy_residuals <- matrix(nrow = length(dummy_residuals), ncol = lags)
for (i in 1:lags){
  lagged_dummy_residuals[,i] <- lag_transform(dummy_residuals, k = i)
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_residuals[,i]))
}

model.crypto.spec.dummy = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit.dummy = ugarchfit(spec = model.crypto.spec.dummy , data = ret_ETH, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit.dummy)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit.dummy)[7:10]^2
dummy_params <- coef(model.crypto.fit.dummy)[15:18]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  if (dummy_residuals[i] == 1){
    variances[i,1] <- (crypto_params[1]+dummy_params[1])*var_index[i+3]+(crypto_params[2]+dummy_params[2])*var_index[i+2]+(crypto_params[3]+dummy_params[3])*var_index[i+1]+(crypto_params[4]+dummy_params[4])*var_index[i]
  }
  else {
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
  }
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from DJI to ETH with dummy variables", xlab = "Time", ylab = "VR")



stargazer::stargazer(model.index.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit.dummy@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")

###########################XRP
model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                      mean.model = list(armaOrder = c(4,0,0), include.mean = TRUE), distribution.model = "norm")

(model.index.fit = ugarchfit(spec = model.index.spec , data = na.fill(ret_DJI,0), solver = 'hybrid'))


index_resid <- xts(residuals(model.index.fit), order.by=as.POSIXct(DJI_adj$ref.date))
filled_index_resid_merge <- na.fill(merge(index_resid, ret_XRP, join = "right"),0)
filled_index_resid <- ts(filled_index_resid_merge$x)
lags = 4
lagged_index_resid <- matrix(nrow = length(filled_index_resid), ncol = lags)
for (i in 1:lags){
  lagged_index_resid[,i] <- lag_transform(filled_index_resid, k = i)
}
returns_index <- ts(rDJI_rXRP$ret_DJI)
lagged_returns <- matrix(nrow = length(returns_index), ncol = lags)
for (i in 1:lags){
  lagged_returns[,i] <- lag_transform(returns_index, k = i)
}

exogenous_crypto = ts(lagged_returns[,1])
for (i in 2:lags){
 exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_index_resid[,i]))
}
(model_crypto = arima(ret_XRP, order = c(1,0,0), xreg = exogenous_crypto))
model.crypto.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = ret_XRP, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit)[7:10]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from DJI to XRP", xlab = "Time", ylab = "VR")

dummy_returns_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_residuals_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_returns <- dummy_returns_simple*returns_index
dummy_residuals <- dummy_residuals_simple*filled_index_resid
lagged_dummy_returns <- matrix(nrow = length(dummy_returns), ncol = lags)
for (i in 1:lags){
  lagged_dummy_returns[,i] <- lag_transform(dummy_returns, k = i)
}
lagged_dummy_residuals <- matrix(nrow = length(dummy_residuals), ncol = lags)
for (i in 1:lags){
  lagged_dummy_residuals[,i] <- lag_transform(dummy_residuals, k = i)
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_residuals[,i]))
}

model.crypto.spec.dummy = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit.dummy = ugarchfit(spec = model.crypto.spec.dummy , data = ret_XRP, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit.dummy)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit.dummy)[7:10]^2
dummy_params <- coef(model.crypto.fit.dummy)[15:18]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  if (dummy_residuals[i] == 1){
    variances[i,1] <- (crypto_params[1]+dummy_params[1])*var_index[i+3]+(crypto_params[2]+dummy_params[2])*var_index[i+2]+(crypto_params[3]+dummy_params[3])*var_index[i+1]+(crypto_params[4]+dummy_params[4])*var_index[i]
  }
  else {
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
  }
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from DJI to XRP with dummy variables", xlab = "Time", ylab = "VR")



stargazer::stargazer(model.index.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit.dummy@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")

#########################XMR

model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                      mean.model = list(armaOrder = c(4,0,0), include.mean = TRUE), distribution.model = "norm")

(model.index.fit = ugarchfit(spec = model.index.spec , data = na.fill(ret_DJI,0), solver = 'hybrid'))


index_resid <- xts(residuals(model.index.fit), order.by=as.POSIXct(DJI_adj$ref.date))
filled_index_resid_merge <- na.fill(merge(index_resid, ret_XMR, join = "right"),0)
filled_index_resid <- ts(filled_index_resid_merge$x)
lags = 4
lagged_index_resid <- matrix(nrow = length(filled_index_resid), ncol = lags)
for (i in 1:lags){
  lagged_index_resid[,i] <- lag_transform(filled_index_resid, k = i)
}
returns_index <- ts(rDJI_rXMR$ret_DJI)
lagged_returns <- matrix(nrow = length(returns_index), ncol = lags)
for (i in 1:lags){
  lagged_returns[,i] <- lag_transform(returns_index, k = i)
}

exogenous_crypto = ts(lagged_returns[,1])
for (i in 2:lags){
 exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_index_resid[,i]))
}
(model_crypto = arima(ret_XMR, order = c(1,0,0), xreg = exogenous_crypto))
model.crypto.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = ret_XMR, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit)[7:10]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from DJI to XMR", xlab = "Time", ylab = "VR")

dummy_returns_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_residuals_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_returns <- dummy_returns_simple*returns_index
dummy_residuals <- dummy_residuals_simple*filled_index_resid
lagged_dummy_returns <- matrix(nrow = length(dummy_returns), ncol = lags)
for (i in 1:lags){
  lagged_dummy_returns[,i] <- lag_transform(dummy_returns, k = i)
}
lagged_dummy_residuals <- matrix(nrow = length(dummy_residuals), ncol = lags)
for (i in 1:lags){
  lagged_dummy_residuals[,i] <- lag_transform(dummy_residuals, k = i)
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_residuals[,i]))
}

model.crypto.spec.dummy = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit.dummy = ugarchfit(spec = model.crypto.spec.dummy , data = ret_XMR, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit.dummy)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit.dummy)[7:10]^2
dummy_params <- coef(model.crypto.fit.dummy)[15:18]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  if (dummy_residuals[i] == 1){
    variances[i,1] <- (crypto_params[1]+dummy_params[1])*var_index[i+3]+(crypto_params[2]+dummy_params[2])*var_index[i+2]+(crypto_params[3]+dummy_params[3])*var_index[i+1]+(crypto_params[4]+dummy_params[4])*var_index[i]
  }
  else {
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
  }
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from DJI to XMR with dummy variables", xlab = "Time", ylab = "VR")



stargazer::stargazer(model.index.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit.dummy@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")

################################################################################################
##################################################################################################
#####################################################################################################
######################################################################################################

model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                      mean.model = list(armaOrder = c(4,0,0), include.mean = TRUE), distribution.model = "norm")

(model.index.fit = ugarchfit(spec = model.index.spec , data = na.fill(ret_GSPC,0), solver = 'hybrid'))


index_resid <- xts(residuals(model.index.fit), order.by=as.POSIXct(GSPC_adj$ref.date))
filled_index_resid_merge <- na.fill(merge(index_resid, ret_BTC, join = "right"),0)
filled_index_resid <- ts(filled_index_resid_merge$x)
lags = 4
lagged_index_resid <- matrix(nrow = length(filled_index_resid), ncol = lags)
for (i in 1:lags){
  lagged_index_resid[,i] <- lag_transform(filled_index_resid, k = i)
}
returns_index <- ts(rGSPC_rBTC$ret_GSPC)
lagged_returns <- matrix(nrow = length(returns_index), ncol = lags)
for (i in 1:lags){
  lagged_returns[,i] <- lag_transform(returns_index, k = i)
}

exogenous_crypto = ts(lagged_returns[,1])
for (i in 2:lags){
 exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_index_resid[,i]))
}
(model_crypto = arima(ret_BTC, order = c(1,0,0), xreg = exogenous_crypto))
model.crypto.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = ret_BTC, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit)[7:10]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from GSPC to BTC", xlab = "Time", ylab = "VR")

dummy_returns_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_residuals_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_returns <- dummy_returns_simple*returns_index
dummy_residuals <- dummy_residuals_simple*filled_index_resid
lagged_dummy_returns <- matrix(nrow = length(dummy_returns), ncol = lags)
for (i in 1:lags){
  lagged_dummy_returns[,i] <- lag_transform(dummy_returns, k = i)
}
lagged_dummy_residuals <- matrix(nrow = length(dummy_residuals), ncol = lags)
for (i in 1:lags){
  lagged_dummy_residuals[,i] <- lag_transform(dummy_residuals, k = i)
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_residuals[,i]))
}

model.crypto.spec.dummy = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit.dummy = ugarchfit(spec = model.crypto.spec.dummy , data = ret_BTC, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit.dummy)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit.dummy)[7:10]^2
dummy_params <- coef(model.crypto.fit.dummy)[15:18]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  if (dummy_residuals[i] == 1){
    variances[i,1] <- (crypto_params[1]+dummy_params[1])*var_index[i+3]+(crypto_params[2]+dummy_params[2])*var_index[i+2]+(crypto_params[3]+dummy_params[3])*var_index[i+1]+(crypto_params[4]+dummy_params[4])*var_index[i]
  }
  else {
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
  }
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from GSPC to BTC with dummy variables", xlab = "Time", ylab = "VR")



stargazer::stargazer(model.index.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit.dummy@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")






##################################################################################ETH
model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                      mean.model = list(armaOrder = c(4,0,0), include.mean = TRUE), distribution.model = "norm")

(model.index.fit = ugarchfit(spec = model.index.spec , data = na.fill(ret_GSPC,0), solver = 'hybrid'))


index_resid <- xts(residuals(model.index.fit), order.by=as.POSIXct(GSPC_adj$ref.date))
filled_index_resid_merge <- na.fill(merge(index_resid, ret_ETH, join = "right"),0)
filled_index_resid <- ts(filled_index_resid_merge$x)
lags = 4
lagged_index_resid <- matrix(nrow = length(filled_index_resid), ncol = lags)
for (i in 1:lags){
  lagged_index_resid[,i] <- lag_transform(filled_index_resid, k = i)
}
returns_index <- ts(rGSPC_rETH$ret_GSPC)
lagged_returns <- matrix(nrow = length(returns_index), ncol = lags)
for (i in 1:lags){
  lagged_returns[,i] <- lag_transform(returns_index, k = i)
}

exogenous_crypto = ts(lagged_returns[,1])
for (i in 2:lags){
 exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_index_resid[,i]))
}
(model_crypto = arima(ret_ETH, order = c(1,0,0), xreg = exogenous_crypto))
model.crypto.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = ret_ETH, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit)[7:10]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from GSPC to ETH", xlab = "Time", ylab = "VR")

dummy_returns_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_residuals_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_returns <- dummy_returns_simple*returns_index
dummy_residuals <- dummy_residuals_simple*filled_index_resid
lagged_dummy_returns <- matrix(nrow = length(dummy_returns), ncol = lags)
for (i in 1:lags){
  lagged_dummy_returns[,i] <- lag_transform(dummy_returns, k = i)
}
lagged_dummy_residuals <- matrix(nrow = length(dummy_residuals), ncol = lags)
for (i in 1:lags){
  lagged_dummy_residuals[,i] <- lag_transform(dummy_residuals, k = i)
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_residuals[,i]))
}

model.crypto.spec.dummy = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit.dummy = ugarchfit(spec = model.crypto.spec.dummy , data = ret_ETH, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit.dummy)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit.dummy)[7:10]^2
dummy_params <- coef(model.crypto.fit.dummy)[15:18]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  if (dummy_residuals[i] == 1){
    variances[i,1] <- (crypto_params[1]+dummy_params[1])*var_index[i+3]+(crypto_params[2]+dummy_params[2])*var_index[i+2]+(crypto_params[3]+dummy_params[3])*var_index[i+1]+(crypto_params[4]+dummy_params[4])*var_index[i]
  }
  else {
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
  }
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from GSPC to ETH with dummy variables", xlab = "Time", ylab = "VR")



stargazer::stargazer(model.index.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit.dummy@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")

###########################XRP
model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                      mean.model = list(armaOrder = c(4,0,0), include.mean = TRUE), distribution.model = "norm")

(model.index.fit = ugarchfit(spec = model.index.spec , data = na.fill(ret_GSPC,0), solver = 'hybrid'))


index_resid <- xts(residuals(model.index.fit), order.by=as.POSIXct(GSPC_adj$ref.date))
filled_index_resid_merge <- na.fill(merge(index_resid, ret_XRP, join = "right"),0)
filled_index_resid <- ts(filled_index_resid_merge$x)
lags = 4
lagged_index_resid <- matrix(nrow = length(filled_index_resid), ncol = lags)
for (i in 1:lags){
  lagged_index_resid[,i] <- lag_transform(filled_index_resid, k = i)
}
returns_index <- ts(rGSPC_rXRP$ret_GSPC)
lagged_returns <- matrix(nrow = length(returns_index), ncol = lags)
for (i in 1:lags){
  lagged_returns[,i] <- lag_transform(returns_index, k = i)
}

exogenous_crypto = ts(lagged_returns[,1])
for (i in 2:lags){
 exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_index_resid[,i]))
}
(model_crypto = arima(ret_XRP, order = c(1,0,0), xreg = exogenous_crypto))
model.crypto.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = ret_XRP, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit)[7:10]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from GSPC to XRP", xlab = "Time", ylab = "VR")

dummy_returns_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_residuals_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_returns <- dummy_returns_simple*returns_index
dummy_residuals <- dummy_residuals_simple*filled_index_resid
lagged_dummy_returns <- matrix(nrow = length(dummy_returns), ncol = lags)
for (i in 1:lags){
  lagged_dummy_returns[,i] <- lag_transform(dummy_returns, k = i)
}
lagged_dummy_residuals <- matrix(nrow = length(dummy_residuals), ncol = lags)
for (i in 1:lags){
  lagged_dummy_residuals[,i] <- lag_transform(dummy_residuals, k = i)
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_residuals[,i]))
}

model.crypto.spec.dummy = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit.dummy = ugarchfit(spec = model.crypto.spec.dummy , data = ret_XRP, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit.dummy)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit.dummy)[7:10]^2
dummy_params <- coef(model.crypto.fit.dummy)[15:18]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  if (dummy_residuals[i] == 1){
    variances[i,1] <- (crypto_params[1]+dummy_params[1])*var_index[i+3]+(crypto_params[2]+dummy_params[2])*var_index[i+2]+(crypto_params[3]+dummy_params[3])*var_index[i+1]+(crypto_params[4]+dummy_params[4])*var_index[i]
  }
  else {
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
  }
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from GSPC to XRP with dummy variables", xlab = "Time", ylab = "VR")



stargazer::stargazer(model.index.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit.dummy@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")

#########################XMR

model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                      mean.model = list(armaOrder = c(4,0,0), include.mean = TRUE), distribution.model = "norm")

(model.index.fit = ugarchfit(spec = model.index.spec , data = na.fill(ret_GSPC,0), solver = 'hybrid'))


index_resid <- xts(residuals(model.index.fit), order.by=as.POSIXct(GSPC_adj$ref.date))
filled_index_resid_merge <- na.fill(merge(index_resid, ret_XMR, join = "right"),0)
filled_index_resid <- ts(filled_index_resid_merge$x)
lags = 4
lagged_index_resid <- matrix(nrow = length(filled_index_resid), ncol = lags)
for (i in 1:lags){
  lagged_index_resid[,i] <- lag_transform(filled_index_resid, k = i)
}
returns_index <- ts(rGSPC_rXMR$ret_GSPC)
lagged_returns <- matrix(nrow = length(returns_index), ncol = lags)
for (i in 1:lags){
  lagged_returns[,i] <- lag_transform(returns_index, k = i)
}

exogenous_crypto = ts(lagged_returns[,1])
for (i in 2:lags){
 exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_index_resid[,i]))
}
(model_crypto = arima(ret_XMR, order = c(1,0,0), xreg = exogenous_crypto))
model.crypto.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = ret_XMR, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit)[7:10]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from GSPC to XMR", xlab = "Time", ylab = "VR")

dummy_returns_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_residuals_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_returns <- dummy_returns_simple*returns_index
dummy_residuals <- dummy_residuals_simple*filled_index_resid
lagged_dummy_returns <- matrix(nrow = length(dummy_returns), ncol = lags)
for (i in 1:lags){
  lagged_dummy_returns[,i] <- lag_transform(dummy_returns, k = i)
}
lagged_dummy_residuals <- matrix(nrow = length(dummy_residuals), ncol = lags)
for (i in 1:lags){
  lagged_dummy_residuals[,i] <- lag_transform(dummy_residuals, k = i)
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_residuals[,i]))
}

model.crypto.spec.dummy = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit.dummy = ugarchfit(spec = model.crypto.spec.dummy , data = ret_XMR, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit.dummy)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit.dummy)[7:10]^2
dummy_params <- coef(model.crypto.fit.dummy)[15:18]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  if (dummy_residuals[i] == 1){
    variances[i,1] <- (crypto_params[1]+dummy_params[1])*var_index[i+3]+(crypto_params[2]+dummy_params[2])*var_index[i+2]+(crypto_params[3]+dummy_params[3])*var_index[i+1]+(crypto_params[4]+dummy_params[4])*var_index[i]
  }
  else {
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
  }
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from GSPC to XMR with dummy variables", xlab = "Time", ylab = "VR")



stargazer::stargazer(model.index.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit.dummy@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")

##################################################################################
###################################################################################
######################################################################################
##########################################################################################

model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                      mean.model = list(armaOrder = c(4,0,0), include.mean = TRUE), distribution.model = "norm")

(model.index.fit = ugarchfit(spec = model.index.spec , data = na.fill(ret_NDX,0), solver = 'hybrid'))


index_resid <- xts(residuals(model.index.fit), order.by=as.POSIXct(NDX_adj$ref.date))
filled_index_resid_merge <- na.fill(merge(index_resid, ret_BTC, join = "right"),0)
filled_index_resid <- ts(filled_index_resid_merge$x)
lags = 4
lagged_index_resid <- matrix(nrow = length(filled_index_resid), ncol = lags)
for (i in 1:lags){
  lagged_index_resid[,i] <- lag_transform(filled_index_resid, k = i)
}
returns_index <- ts(rNDX_rBTC$ret_NDX)
lagged_returns <- matrix(nrow = length(returns_index), ncol = lags)
for (i in 1:lags){
  lagged_returns[,i] <- lag_transform(returns_index, k = i)
}

exogenous_crypto = ts(lagged_returns[,1])
for (i in 2:lags){
 exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_index_resid[,i]))
}
(model_crypto = arima(ret_BTC, order = c(1,0,0), xreg = exogenous_crypto))
model.crypto.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = ret_BTC, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit)[7:10]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from NDX to BTC", xlab = "Time", ylab = "VR")

dummy_returns_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_residuals_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_returns <- dummy_returns_simple*returns_index
dummy_residuals <- dummy_residuals_simple*filled_index_resid
lagged_dummy_returns <- matrix(nrow = length(dummy_returns), ncol = lags)
for (i in 1:lags){
  lagged_dummy_returns[,i] <- lag_transform(dummy_returns, k = i)
}
lagged_dummy_residuals <- matrix(nrow = length(dummy_residuals), ncol = lags)
for (i in 1:lags){
  lagged_dummy_residuals[,i] <- lag_transform(dummy_residuals, k = i)
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_residuals[,i]))
}

model.crypto.spec.dummy = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit.dummy = ugarchfit(spec = model.crypto.spec.dummy , data = ret_BTC, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit.dummy)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit.dummy)[7:10]^2
dummy_params <- coef(model.crypto.fit.dummy)[15:18]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  if (dummy_residuals[i] == 1){
    variances[i,1] <- (crypto_params[1]+dummy_params[1])*var_index[i+3]+(crypto_params[2]+dummy_params[2])*var_index[i+2]+(crypto_params[3]+dummy_params[3])*var_index[i+1]+(crypto_params[4]+dummy_params[4])*var_index[i]
  }
  else {
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
  }
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from NDX to BTC with dummy variables", xlab = "Time", ylab = "VR")



stargazer::stargazer(model.index.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit.dummy@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")






##################################################################################ETH
model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                      mean.model = list(armaOrder = c(4,0,0), include.mean = TRUE), distribution.model = "norm")

(model.index.fit = ugarchfit(spec = model.index.spec , data = na.fill(ret_NDX,0), solver = 'hybrid'))


index_resid <- xts(residuals(model.index.fit), order.by=as.POSIXct(NDX_adj$ref.date))
filled_index_resid_merge <- na.fill(merge(index_resid, ret_ETH, join = "right"),0)
filled_index_resid <- ts(filled_index_resid_merge$x)
lags = 4
lagged_index_resid <- matrix(nrow = length(filled_index_resid), ncol = lags)
for (i in 1:lags){
  lagged_index_resid[,i] <- lag_transform(filled_index_resid, k = i)
}
returns_index <- ts(rNDX_rETH$ret_NDX)
lagged_returns <- matrix(nrow = length(returns_index), ncol = lags)
for (i in 1:lags){
  lagged_returns[,i] <- lag_transform(returns_index, k = i)
}

exogenous_crypto = ts(lagged_returns[,1])
for (i in 2:lags){
 exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_index_resid[,i]))
}
(model_crypto = arima(ret_ETH, order = c(1,0,0), xreg = exogenous_crypto))
model.crypto.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = ret_ETH, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit)[7:10]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from NDX to ETH", xlab = "Time", ylab = "VR")

dummy_returns_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_residuals_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_returns <- dummy_returns_simple*returns_index
dummy_residuals <- dummy_residuals_simple*filled_index_resid
lagged_dummy_returns <- matrix(nrow = length(dummy_returns), ncol = lags)
for (i in 1:lags){
  lagged_dummy_returns[,i] <- lag_transform(dummy_returns, k = i)
}
lagged_dummy_residuals <- matrix(nrow = length(dummy_residuals), ncol = lags)
for (i in 1:lags){
  lagged_dummy_residuals[,i] <- lag_transform(dummy_residuals, k = i)
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_residuals[,i]))
}

model.crypto.spec.dummy = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit.dummy = ugarchfit(spec = model.crypto.spec.dummy , data = ret_ETH, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit.dummy)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit.dummy)[7:10]^2
dummy_params <- coef(model.crypto.fit.dummy)[15:18]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  if (dummy_residuals[i] == 1){
    variances[i,1] <- (crypto_params[1]+dummy_params[1])*var_index[i+3]+(crypto_params[2]+dummy_params[2])*var_index[i+2]+(crypto_params[3]+dummy_params[3])*var_index[i+1]+(crypto_params[4]+dummy_params[4])*var_index[i]
  }
  else {
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
  }
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from NDX to ETH with dummy variables", xlab = "Time", ylab = "VR")



stargazer::stargazer(model.index.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit.dummy@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")

###########################XRP
model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                      mean.model = list(armaOrder = c(4,0,0), include.mean = TRUE), distribution.model = "norm")

(model.index.fit = ugarchfit(spec = model.index.spec , data = na.fill(ret_NDX,0), solver = 'hybrid'))


index_resid <- xts(residuals(model.index.fit), order.by=as.POSIXct(NDX_adj$ref.date))
filled_index_resid_merge <- na.fill(merge(index_resid, ret_XRP, join = "right"),0)
filled_index_resid <- ts(filled_index_resid_merge$x)
lags = 4
lagged_index_resid <- matrix(nrow = length(filled_index_resid), ncol = lags)
for (i in 1:lags){
  lagged_index_resid[,i] <- lag_transform(filled_index_resid, k = i)
}
returns_index <- ts(rNDX_rXRP$ret_NDX)
lagged_returns <- matrix(nrow = length(returns_index), ncol = lags)
for (i in 1:lags){
  lagged_returns[,i] <- lag_transform(returns_index, k = i)
}

exogenous_crypto = ts(lagged_returns[,1])
for (i in 2:lags){
 exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_index_resid[,i]))
}
(model_crypto = arima(ret_XRP, order = c(1,0,0), xreg = exogenous_crypto))
model.crypto.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = ret_XRP, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit)[7:10]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from NDX to XRP", xlab = "Time", ylab = "VR")

dummy_returns_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_residuals_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_returns <- dummy_returns_simple*returns_index
dummy_residuals <- dummy_residuals_simple*filled_index_resid
lagged_dummy_returns <- matrix(nrow = length(dummy_returns), ncol = lags)
for (i in 1:lags){
  lagged_dummy_returns[,i] <- lag_transform(dummy_returns, k = i)
}
lagged_dummy_residuals <- matrix(nrow = length(dummy_residuals), ncol = lags)
for (i in 1:lags){
  lagged_dummy_residuals[,i] <- lag_transform(dummy_residuals, k = i)
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_residuals[,i]))
}

model.crypto.spec.dummy = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit.dummy = ugarchfit(spec = model.crypto.spec.dummy , data = ret_XRP, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit.dummy)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit.dummy)[7:10]^2
dummy_params <- coef(model.crypto.fit.dummy)[15:18]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  if (dummy_residuals[i] == 1){
    variances[i,1] <- (crypto_params[1]+dummy_params[1])*var_index[i+3]+(crypto_params[2]+dummy_params[2])*var_index[i+2]+(crypto_params[3]+dummy_params[3])*var_index[i+1]+(crypto_params[4]+dummy_params[4])*var_index[i]
  }
  else {
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
  }
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from NDX to XRP with dummy variables", xlab = "Time", ylab = "VR")



stargazer::stargazer(model.index.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit.dummy@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")

#########################XMR

model.index.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                      mean.model = list(armaOrder = c(4,0,0), include.mean = TRUE), distribution.model = "norm")

(model.index.fit = ugarchfit(spec = model.index.spec , data = na.fill(ret_NDX,0), solver = 'hybrid'))


index_resid <- xts(residuals(model.index.fit), order.by=as.POSIXct(NDX_adj$ref.date))
filled_index_resid_merge <- na.fill(merge(index_resid, ret_XMR, join = "right"),0)
filled_index_resid <- ts(filled_index_resid_merge$x)
lags = 4
lagged_index_resid <- matrix(nrow = length(filled_index_resid), ncol = lags)
for (i in 1:lags){
  lagged_index_resid[,i] <- lag_transform(filled_index_resid, k = i)
}
returns_index <- ts(rNDX_rXMR$ret_NDX)
lagged_returns <- matrix(nrow = length(returns_index), ncol = lags)
for (i in 1:lags){
  lagged_returns[,i] <- lag_transform(returns_index, k = i)
}

exogenous_crypto = ts(lagged_returns[,1])
for (i in 2:lags){
 exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto,ts(lagged_index_resid[,i]))
}
(model_crypto = arima(ret_XMR, order = c(1,0,0), xreg = exogenous_crypto))
model.crypto.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit = ugarchfit(spec = model.crypto.spec , data = ret_XMR, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit)[7:10]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from NDX to XMR", xlab = "Time", ylab = "VR")

dummy_returns_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_residuals_simple <- ifelse(as.Date(format(index(filled_index_resid_merge), format="%Y-%m-%d")) %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
dummy_returns <- dummy_returns_simple*returns_index
dummy_residuals <- dummy_residuals_simple*filled_index_resid
lagged_dummy_returns <- matrix(nrow = length(dummy_returns), ncol = lags)
for (i in 1:lags){
  lagged_dummy_returns[,i] <- lag_transform(dummy_returns, k = i)
}
lagged_dummy_residuals <- matrix(nrow = length(dummy_residuals), ncol = lags)
for (i in 1:lags){
  lagged_dummy_residuals[,i] <- lag_transform(dummy_residuals, k = i)
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_returns[,i]))
}
for (i in 1:lags){
  exogenous_crypto <- cbind(exogenous_crypto, ts(lagged_dummy_residuals[,i]))
}

model.crypto.spec.dummy = ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)),
                      mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE, external.regressors = exogenous_crypto), distribution.model = "norm")

(model.crypto.fit.dummy = ugarchfit(spec = model.crypto.spec.dummy , data = ret_XMR, solver = 'hybrid'))

var_index <- sigma(model.index.fit)^2
var_crypto <- sigma(model.crypto.fit.dummy)^2
cond_vars <- na.locf(na.locf(merge(var_index, var_crypto, join = "right")), fromLast = TRUE)
crypto_params <- coef(model.crypto.fit.dummy)[7:10]^2
dummy_params <- coef(model.crypto.fit.dummy)[15:18]^2
var_index <- coredata(cond_vars$var_index)
var_crypto <- coredata(var_crypto)
variances <- matrix(nrow = (length(var_index)-lags), ncol = 3)

for (i in 1:(length(var_index)-lags)){
  if (dummy_residuals[i] == 1){
    variances[i,1] <- (crypto_params[1]+dummy_params[1])*var_index[i+3]+(crypto_params[2]+dummy_params[2])*var_index[i+2]+(crypto_params[3]+dummy_params[3])*var_index[i+1]+(crypto_params[4]+dummy_params[4])*var_index[i]
  }
  else {
  variances[i,1] <- crypto_params[1]*var_index[i+3]+crypto_params[2]*var_index[i+2]+crypto_params[3]*var_index[i+1]+crypto_params[4]*var_index[i]
  }
}
variances[,2] <- variances[,1]+var_crypto[5:length(var_crypto)]
variances[,3] <- variances[,1]/variances[,2]
plot(x = index(filled_index_resid_merge)[5:length(var_crypto)], y = variances[,3], type = 'l', main = "Variance Ratio from NDX to XMR with dummy variables", xlab = "Time", ylab = "VR")



stargazer::stargazer(model.index.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")


stargazer::stargazer(model.crypto.fit.dummy@fit$matcoef,
  title = "Parameter Estimates of the GARCH(1, 1)") %>%
  gsub("Std. Error", "Rob. Std. Error", .) %>%
  gsub("t value", "Rob. t value", .) %>%
  gsub("mu", "\\\\mu", .) %>%
  gsub("alpha1", "\\\\alpha", .) %>%
  gsub("omega", "\\\\omega", .) %>%
  gsub("beta1", "\\\\beta", .) %>%
  gsub("shape", "\\\\nu", .)  %>%
  writeLines("arch_output.tex")

