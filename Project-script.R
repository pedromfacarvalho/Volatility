# Title     : TODO
# Objective : TODO
# Created by: pedro
# Created on: 03/09/20
pacman::p_load(forecast,quantmod, rugarch, rmgarch,coinmarketcapr,xts, tidyverse, ggthemes,
               gridExtra, tseries, lmtest, FinTS)

DJI <- getSymbols("DJI", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)
DJI_adj <- DJI$DJI.Adjusted
#eGARCH, ARIMA (1,0,0) sstd
GSPC <- getSymbols("^GSPC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)
GSPC_adj <- GSPC$GSPC.Adjusted
#eGARCH, ARIMA (0,0,1) sstd
IXIC <- getSymbols("^IXIC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)
IXIC_adj <- IXIC$IXIC.Adjusted
#eGARCH,ARIMA (0,0,1) sstd


BTC <- getSymbols("BTC-USD", src = "yahoo", auto.assign = FALSE)
BTC_adj <- BTC$`BTC-USD.Adjusted
#sGARCH, ARIMA(1,0,0) sged`
ETH <- getSymbols("ETH-USD", src = "yahoo", auto.assign = FALSE)
ETH_adj <- ETH$`ETH-USD.Adjusted`
#eGARCH, ARIMA(1,1,1) sged
XRP <- getSymbols("XRP-USD", src = "yahoo", auto.assign = FALSE)
XRP_adj <- XRP$`XRP-USD.Adjusted`
#sGARCH, ARIMA (1,0,3) sged

ret_DJI <- dailyReturn(DJI_adj, type = "log")
ret_GSPC <- dailyReturn(GSPC_adj, type = "log")
ret_IXIC <- dailyReturn(IXIC_adj, type = "log")
ret_BTC <- dailyReturn(BTC_adj, type = "log")
ret_ETH <- dailyReturn(ETH_adj, type = "log")
ret_XRP <- dailyReturn(XRP_adj, type = "log")




rDJI_rBTC <- merge(ret_DJI, ret_BTC, join = "inner")
rDJI_rETH <- merge(ret_DJI, ret_ETH, join = 'inner')
rDJI_rXRP <- merge(ret_DJI, ret_XRP, join = 'inner')

rGSPC_rBTC <- merge(ret_GSPC, ret_BTC, join = 'inner')
rGSPC_rETH <- merge(ret_GSPC, ret_ETH, join = 'inner')
rGSPC_rXRP <- merge(ret_GSPC, ret_XRP, join = 'inner')

rIXIC_rBTC <- merge(ret_IXIC, ret_BTC, join = 'inner')
rIXIC_rETH <- merge(ret_IXIC, ret_ETH, join = 'inner')
rIXIC_rXRP <- merge(ret_IXIC, ret_XRP, join = 'inner')




#DJI vs BTC

uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1,1)),
                                     mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE), distribution.model = 'sstd'),
                         ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1,1)),
                                     mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE), distribution.model = 'sged')))
(multf = multifit(uspec.n, rDJI_rBTC))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
(fit1 = dccfit(spec1, data = rDJI_rBTC, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_DJI_BTC <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_DJI_BTC <- as.xts(cor_DJI_BTC)  # imposes the xts time series format - useful for plotting

plot(cor_DJI_BTC)


#DJI vs ETH


uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                                     mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE), distribution.model = 'sged'),
                         ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1,1)),
                                     mean.model = list(armaOrder = c(1,1,1), include.mean = TRUE), distribution.model = 'sged')))
(multf = multifit(uspec.n, rDJI_rETH))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
(fit1 = dccfit(spec1, data = rDJI_rETH, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_DJI_ETH <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_DJI_ETH <- as.xts(cor_DJI_ETH)  # imposes the xts time series format - useful for plotting

plot(cor_DJI_ETH)


#DJI vs XRP

uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1,1)),
                                     mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE), distribution.model = 'sstd'),
                         ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                                     mean.model = list(armaOrder = c(1,0,3), include.mean = TRUE), distribution.model = 'sged')))
(multf = multifit(uspec.n, rDJI_rXRP))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
(fit1 = dccfit(spec1, data = rDJI_rXRP, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_DJI_XRP <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_DJI_XRP <- as.xts(cor_DJI_XRP)  # imposes the xts time series format - useful for plotting

plot(cor_DJI_XRP)

