# Title     : TODO
# Objective : TODO
# Created by: pedro
# Created on: 03/09/20
pacman::p_load(forecast,quantmod, rugarch, rmgarch,coinmarketcapr,xts, tidyverse, ggthemes,
               gridExtra, tseries, lmtest, FinTS)

DJI <- getSymbols("DJI", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)
DJI_adj <- DJI$DJI.Adjusted
GSPC <- getSymbols("^GSPC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)
GSPC_adj <- GSPC$GSPC.Adjusted
IXIC <- getSymbols("^IXIC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)
IXIC_adj <- IXIC$IXIC.Adjusted


BTC <- getSymbols("BTC-USD", src = "yahoo", auto.assign = FALSE)
BTC_adj <- BTC$`BTC-USD.Adjusted`
ETH <- getSymbols("ETH-USD", src = "yahoo", auto.assign = FALSE)
ETH_adj <- ETH$`ETH-USD.Adjusted`
XRP <- getSymbols("XRP-USD", src = "yahoo", auto.assign = FALSE)
XRP_adj <- XRP$`XRP-USD.Adjusted`

ret_DJI <- dailyReturn(DJI_adj, type = "log")
ret_GSPC <- dailyReturn(GSPC_adj, type = "log")
ret_IXIC <- dailyReturn(IXIC_adj, type = "log")
ret_BTC <- dailyReturn(BTC_adj, type = "log")
ret_ETH <- dailyReturn(ETH_adj, type = "log")
ret_XRP <- dailyReturn(XRP_adj, type = "log")




rDJI_rBTC <- merge(ret_DJI, ret_BTC, join = "inner")





uspec.n = multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf = multifit(uspec.n, rDJI_rBTC)
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
fit1 = dccfit(spec1, data = rDJI_rBTC, fit.control = list(eval.se = TRUE), fit = multf)
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_DJI_BTC <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_DJI_BTC <- as.xts(cor_BG)  # imposes the xts time series format - useful for plotting

plot(cor_DJI_BTC)