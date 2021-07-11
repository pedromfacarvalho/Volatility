# Title     : TODO
# Objective : TODO
# Created by: m20190417
# Created on: 2/7/2021


pacman::p_load(forecast,quantmod, tidyquant, rugarch, rmgarch,coinmarketcapr,xts, tidyverse, ggthemes,
               gridExtra, tseries, lmtest, FinTS, mgarchBEKK, ccgarch, xtable, MTS, plm,zoo)

# dados com back e forward fill
DJI <- na.locf(na.locf(getSymbols("DJI", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
DJI_adj <- xts(DJI$DJI.Adjusted)
plot(DJI_adj)
GSPC <- na.locf(na.locf(getSymbols("^GSPC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
GSPC_adj <- xts(GSPC$GSPC.Adjusted)
plot(GSPC_adj)
IXIC <- na.locf(na.locf(getSymbols("^IXIC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
IXIC_adj <- IXIC$IXIC.Adjusted
plot(IXIC_adj)

BTC <- na.locf(na.locf(getSymbols("BTC-USD", src = "yahoo", auto.assign = FALSE)), fromLast = TRUE)
BTC_adj <- xts(BTC$`BTC-USD.Adjusted`)
plot(BTC_adj)
ETH <- na.locf(na.locf(getSymbols("ETH-USD", src = "yahoo", auto.assign = FALSE)), fromLast = TRUE)
ETH_adj <- ETH$`ETH-USD.Adjusted`
plot(ETH_adj)
XRP <- na.locf(na.locf(getSymbols("XRP-USD", src = "yahoo", auto.assign = FALSE)), fromLast = TRUE)
XRP_adj <- XRP$`XRP-USD.Adjusted`
plot(XRP_adj)
XMR <- na.locf(na.locf(getSymbols("XMR-USD", src = "yahoo", auto.assign = FALSE)), fromLast = TRUE)
XMR_adj <- XMR$`XMR-USD.Adjusted`
plot(XMR_adj)

# returns
ret_DJI <- dailyReturn(DJI_adj, type = "log")
ret_GSPC <- dailyReturn(GSPC_adj, type = "log")
ret_IXIC <- dailyReturn(IXIC_adj, type = "log")
ret_BTC <- dailyReturn(BTC_adj, type = "log")
ret_ETH <- dailyReturn(ETH_adj, type = "log")
ret_XRP <- dailyReturn(XRP_adj, type = "log")
ret_XMR <- dailyReturn(XMR_adj, type = "log")

colnames(merge_total) <- c('DJI', 'GSPC', 'IXIC', 'BTC', 'ETH', 'XRP')

# criação de pares
rDJI_rBTC <- na.locf(na.locf(merge(ret_DJI, ret_BTC, join = "right")), fromLast = TRUE)
rDJI_rETH <- na.locf(na.locf(merge(ret_DJI, ret_ETH, join = 'right')), fromLast = TRUE)
rDJI_rXRP <- na.locf(na.locf(merge(ret_DJI, ret_XRP, join = 'right')), fromLast = TRUE)
rDJI_rXMR <- na.locf(na.locf(merge(ret_DJI, ret_XMR, join = 'right')), fromLast = TRUE)

rGSPC_rBTC <- merge(ret_GSPC, ret_BTC, join = 'right')
rGSPC_rETH <- merge(ret_GSPC, ret_ETH, join = 'right')
rGSPC_rXRP <- merge(ret_GSPC, ret_XRP, join = 'right')
rGSPC_rXMR <-merge(ret_GSPC, ret_XMR, join = 'right')

rIXIC_rBTC <- merge(ret_IXIC, ret_BTC, join = 'right')
rIXIC_rETH <- merge(ret_IXIC, ret_ETH, join = 'right')
rIXIC_rXRP <- merge(ret_IXIC, ret_XRP, join = 'right')
rIXIC_rXMR <- merge(ret_IXIC, ret_XMR, join = 'right')

# testes à normalidade
jarque.bera.test(ret_XMR)
adf.test(ret_XMR)

# testes de causalidade
grangertest(daily.returns ~ daily.returns.1, order = 4, data = rDJI_rBTC) #2
grangertest(daily.returns ~ daily.returns.1, order = 4, data = rDJI_rETH) #2 e 4
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rDJI_rXRP) #2 é o melhor 0.1871 não causal, encontrar alternativa
grangertest(daily.returns ~ daily.returns.1, order = 5, data = rDJI_rXMR) #2
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rGSPC_rBTC) #2+
grangertest(daily.returns ~ daily.returns.1, order = 4, data = rGSPC_rETH) #2 4
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rGSPC_rXRP) #2 mas 15% significancia
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rGSPC_rXMR) #2
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rIXIC_rBTC) #2
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rIXIC_rETH) #2
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rIXIC_rXRP) #2 mas 6.57% significancia
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rIXIC_rXMR) #2 mas 6.21% significancia


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
models_spillovers(data = rIXIC_rBTC, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers(data = rIXIC_rETH, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers(data = rIXIC_rXRP, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers(data = rIXIC_rXMR, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)

models_spillovers_dummy(data = rDJI_rBTC, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 4)
models_spillovers_dummy(data = rDJI_rETH, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 4)
models_spillovers_dummy(data = rDJI_rXRP, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rDJI_rXMR, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 5)
models_spillovers_dummy(data = rGSPC_rBTC, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rGSPC_rETH, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 4)
models_spillovers_dummy(data = rGSPC_rXRP, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rGSPC_rXMR, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rIXIC_rBTC, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rIXIC_rETH, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rIXIC_rXRP, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)
models_spillovers_dummy(data = rIXIC_rXMR, index_order = c(1,0,0), crypto_order = c(0,0,0), value_lagged = 2)


