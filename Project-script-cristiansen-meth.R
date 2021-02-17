# Title     : TODO
# Objective : TODO
# Created by: m20190417
# Created on: 2/7/2021


pacman::p_load(forecast,quantmod, rugarch, rmgarch,coinmarketcapr,xts, tidyverse, ggthemes,
               gridExtra, tseries, lmtest, FinTS, mgarchBEKK, ccgarch, xtable, MTS, plm)

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
BTC_adj <- BTC$`BTC-USD.Adjusted`
#sGARCH, ARIMA(1,0,0) sged
ETH <- getSymbols("ETH-USD", src = "yahoo", auto.assign = FALSE)
ETH_adj <- ETH$`ETH-USD.Adjusted`
#eGARCH, ARIMA(1,1,1) sged
XRP <- getSymbols("XRP-USD", src = "yahoo", auto.assign = FALSE)
XRP_adj <- XRP$`XRP-USD.Adjusted`
#sGARCH, ARIMA (1,0,3) sged
XMR <- getSymbols("XMR-USD", src = "yahoo", auto.assign = FALSE)
XMR_adj <- XMR$`XMR-USD.Adjusted`


ret_DJI <- dailyReturn(DJI_adj, type = "log")
ret_GSPC <- dailyReturn(GSPC_adj, type = "log")
ret_IXIC <- dailyReturn(IXIC_adj, type = "log")
ret_BTC <- dailyReturn(BTC_adj, type = "log")
ret_ETH <- dailyReturn(ETH_adj, type = "log")
ret_XRP <- dailyReturn(XRP_adj, type = "log")
ret_XMR <- dailyReturn(XMR_adj, type = "log")

merge_total <- na.omit(merge(ret_DJI, ret_GSPC, ret_IXIC, ret_BTC, ret_ETH, ret_XRP))
colnames(merge_total) <- c('DJI', 'GSPC', 'IXIC', 'BTC', 'ETH', 'XRP')

rDJI_rBTC <- merge(ret_DJI, ret_BTC, join = "right")
rDJI_rETH <- merge(ret_DJI, ret_ETH, join = 'inner')
rDJI_rXRP <- merge(ret_DJI, ret_XRP, join = 'inner')
rDJI_rXMR <- merge(ret_DJI, ret_XMR, join = 'inner')

rGSPC_rBTC <- merge(ret_GSPC, ret_BTC, join = 'inner')
rGSPC_rETH <- merge(ret_GSPC, ret_ETH, join = 'inner')
rGSPC_rXRP <- merge(ret_GSPC, ret_XRP, join = 'inner')
rGSPC_rXMR <-merge(ret_GSPC, ret_XMR, join = 'inner')

rIXIC_rBTC <- merge(ret_IXIC, ret_BTC, join = 'inner')
rIXIC_rETH <- merge(ret_IXIC, ret_ETH, join = 'inner')
rIXIC_rXRP <- merge(ret_IXIC, ret_XRP, join = 'inner')
rIXIC_rXMR <- merge(ret_IXIC, ret_XRP, join = 'inner')

jarque.bera.test(ret_XMR)
adf.test(ret_XMR)


grangertest(daily.returns ~ daily.returns.1, order = 4, data = rDJI_rBTC) #2
grangertest(daily.returns ~ daily.returns.1, order = 4, data = rDJI_rETH) #2 e 4
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rDJI_rXRP) #2 é o melhor 0.1871 não causal, encontrar alternativa
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rDJI_rXMR) #2
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rGSPC_rBTC) #2+
grangertest(daily.returns ~ daily.returns.1, order = 4, data = rGSPC_rETH) #2 4
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rGSPC_rXRP) #2 mas 15% significancia
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rGSPC_rXMR) #2
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rIXIC_rBTC) #2
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rIXIC_rETH) #2
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rIXIC_rXRP) #2 mas 6.57% significancia
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rIXIC_rXMR) #2 mas 6.21% significancia


model.DJI = arima(rDJI_rXRP$daily.returns, order = c(1,0,0))
model.XRP = arima(rDJI_rXRP$daily.returns.1, order = c(2,0,2))
resid_DJI_XRP <- cbind(model.DJI$residuals, model.XRP$residuals)

model.bekk <- BEKK11(resid_DJI_XRP, cond.dist = "normal")


A_0 = matrix(c(model.bekk$estimates[3], model.bekk$estimates[4], model.bekk$estimates[4], model.bekk$estimates[5]), nrow = 2, ncol = 2)
A_0
A_1 = matrix(c(model.bekk$estimates[6:9]), nrow = 2, ncol = 2)
A_1
B = matrix(c(model.bekk$estimates[10:13]), nrow = 2, ncol = 2)